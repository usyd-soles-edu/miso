'use strict';

const assert = require('assert');
const path = require('path');
const fs = require('fs');

const root = path.resolve(__dirname, '../../..');
const timers = [];
global.setTimeout = callback => {
    timers.push(callback);
    return timers.length;
};

const defaults = {
    useFixedSeed: true,
    seed: 0,
    factor: [],
    nmdsEnv: [],
    nmdsEnvPerm: 99,
    nmdsOverlay: false,
    nmdsHull: false,
    nmdsEllipse: false,
    nmdsSpider: false,
    nmdsShepard: true,
    nmdsSiteTable: false,
    nmdsFeatureTable: false,
    nmdsShepardTable: false,
    nmdsTrymax: 20,
    nmdsMaxit: 200,
    simperAssess: false,
    simperN: 999,
    simperAdjust: 'holm',
    anosimN: 999,
    anosimPairwise: false,
    anosimAdjust: 'holm',
    permN: 999,
    permScheme: 'free',
    permRestriction: 'free',
    permBy: 'terms',
    permFactors: [],
    covariates: [],
    strata: [],
    permInteractions: false,
    permPairwise: false,
    permAdjust: 'holm',
    useParallel: false,
    pcoaDisplayFactor: [],
    showCompanionPcoa: false,
    pcoaCentroids: false,
    pcoaSpiders: false,
    showRankPlot: true,
    dispPairwise: false,
    dispAdjust: 'holm',
    distBinary: false,
    distSqrt: false,
    distAdd: 'none',
    dispBias: false,
};

const createUi = overrides => {
    const state = { ...defaults, ...overrides };
    const enabled = {};
    const ui = {};

    for (const [name, value] of Object.entries(state)) {
        ui[name] = {
            value: () => state[name],
            setValue: next => {
                const changed = !Object.is(state[name], next);
                state[name] = next;
                if (changed && ui[name].onChange)
                    ui[name].onChange();
            },
            setEnabled: next => { enabled[name] = Boolean(next); },
            expand() {},
        };
    }

    for (const name of [
        'groupOptions', 'environmentAssessment', 'reproducibility', 'tables',
        'studyDesign', 'plots', 'advancedOptions', 'permutationAssessment',
    ])
        ui[name] = { expand() {} };

    return { ui, state, enabled };
};

const exercise = name => {
    const controller = require(path.join(root, 'jamovi', 'js', `${name}.js`));
    const layout = fs.readFileSync(path.join(root, 'jamovi', `${name}.u.yaml`), 'utf8');
    assert.match(layout, /events:\n  updated: view_updated\n/,
        `${name}: normalisation runs after silent option hydration`);
    assert.doesNotMatch(layout, /loaded: view_loaded/,
        `${name}: seed startup does not depend on pre-hydration loaded`);
    const { ui, state, enabled } = createUi();
    ui.useFixedSeed.onChange = () => controller.fixed_seed_changed(ui);
    ui.seed.onChange = () => controller.seed_changed(ui);
    ui.simperAssess.onChange = () => controller.update_control_states(ui);

    // A control-state update can happen before option hydration. It must not turn
    // the raw compatibility default (checked + random sentinel) into a fixed
    // seed by itself.
    controller.update_control_states(ui);
    assert.strictEqual(state.useFixedSeed, true, `${name}: early mode`);
    assert.strictEqual(state.seed, 0, `${name}: early seed`);

    controller.view_updated(ui);
    while (timers.length > 0)
        timers.shift()();
    assert.strictEqual(state.useFixedSeed, false, `${name}: new UI mode`);
    assert.strictEqual(state.seed, 0, `${name}: new UI seed`);
    assert.strictEqual(enabled.seed, false, `${name}: random seed disabled`);
    controller.view_updated(ui);
    assert.strictEqual(state.useFixedSeed, false, `${name}: repeated hydration stays random`);

    if (name === 'simper') {
        assert.strictEqual(state.simperAssess, false, 'SIMPER assessment stays off');
        assert.strictEqual(enabled.useFixedSeed, false,
            'SIMPER seed mode stays subordinate to assessment');
        assert.strictEqual(enabled.seed, false,
            'SIMPER numeric seed stays subordinate to assessment');
        ui.simperAssess.setValue(true);
        assert.strictEqual(enabled.useFixedSeed, true,
            'SIMPER seed mode enabled only after assessment is enabled');
    }

    ui.useFixedSeed.setValue(true);
    assert.strictEqual(state.useFixedSeed, true, `${name}: fixed mode checked`);
    assert.strictEqual(state.seed, 123, `${name}: first-use value supplied`);
    assert.strictEqual(enabled.seed, true, `${name}: fixed seed enabled`);

    // A checked field cannot remain visibly fixed while carrying the random
    // sentinel. The controller normalizes an attempted zero edit to 123.
    ui.seed.setValue(0);
    assert.strictEqual(state.seed, 123, `${name}: checked zero normalized`);

    ui.useFixedSeed.setValue(false);
    assert.strictEqual(state.useFixedSeed, false, `${name}: fixed mode cleared`);
    assert.strictEqual(state.seed, 123, `${name}: fixed value retained`);
    assert.strictEqual(enabled.seed, false, `${name}: retained value disabled`);
    ui.seed.setValue(456);
    assert.strictEqual(state.useFixedSeed, false,
        `${name}: inactive edit does not enable fixed mode`);
    assert.strictEqual(state.seed, 456, `${name}: inactive value retained`);
    controller.view_updated(ui);
    assert.strictEqual(state.useFixedSeed, false,
        `${name}: post-hydration update preserves unchecked positive seed`);

    const legacy = createUi({ useFixedSeed: true, seed: 789, simperAssess: true });
    legacy.ui.useFixedSeed.onChange = () => controller.fixed_seed_changed(legacy.ui);
    legacy.ui.seed.onChange = () => controller.seed_changed(legacy.ui);
    legacy.ui.simperAssess.onChange = () => controller.update_control_states(legacy.ui);
    controller.view_updated(legacy.ui);
    while (timers.length > 0)
        timers.shift()();
    assert.strictEqual(legacy.state.useFixedSeed, true,
        `${name}: legacy positive value is visibly fixed`);
    assert.strictEqual(legacy.state.seed, 789,
        `${name}: legacy positive value retained`);
};

for (const analysis of ['permanova', 'anosim', 'permdisp', 'simper', 'nmds'])
    exercise(analysis);

console.log('seed UI controller contracts pass');

// Saved table preferences should remain discoverable when reopening the panel.
for (const option of ['nmdsSiteTable', 'nmdsFeatureTable', 'nmdsShepardTable']) {
    const { ui } = createUi({ [option]: true });
    let expanded = false;
    ui.tables.expand = () => { expanded = true; };
    require(path.join(root, 'jamovi/js/nmds.js')).view_updated(ui);
    assert.strictEqual(expanded, true, `${option}: Tables section expands`);
}
