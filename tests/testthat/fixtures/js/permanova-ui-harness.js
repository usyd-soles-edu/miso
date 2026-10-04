'use strict';

const assert = require('assert');
const events = require(process.argv[2]);

global.setTimeout = callback => callback();

const control = initial => {
    const element = {};
    return {
        current: initial,
        enabled: null,
        focused: 0,
        $el: [{
            contains(candidate) {
                return candidate === element;
            },
        }],
        element,
        value() {
            return this.current;
        },
        setValue(value) {
            this.current = value;
        },
        setEnabled(value) {
            this.enabled = value;
        },
        focus() {
            this.focused += 1;
        },
    };
};

const makeUi = ({
    primary=['group'], additional=[], displayed=null, requested=true,
    pairwise=false, interactions=false, permBy='terms', adjust='holm',
    scheme='free', strata=[], covariates=[], transform='none',
    distance='euclidean', binary=false, sqrt=false, add='none',
}={}) => ({
    factor: control(primary),
    permFactors: control(additional),
    pcoaDisplayFactor: control(displayed),
    showCompanionPcoa: control(requested),
    pcoaCentroids: control(false),
    pcoaSpiders: control(false),
    permPairwise: control(pairwise),
    permInteractions: control(interactions),
    permBy: control(permBy),
    permAdjust: control(adjust),
    permScheme: control(scheme),
    strata: control(strata),
    covariates: control(covariates),
    transform: control(transform),
    distance: control(distance),
    distBinary: control(binary),
    distSqrt: control(sqrt),
    distAdd: control(add),
});

// Primary-only model: Display Groups By must stay enabled whether or not
// the companion plot is on, so users can assign or reassign the display
// factor before enabling the plot. Dependent rules are retained: centroids
// and spiders stay gated on the companion toggle and a valid display group.
let ui = makeUi({requested: false});
events.update_control_states(ui);
assert.strictEqual(ui.pcoaDisplayFactor.enabled, true);
assert.strictEqual(ui.pcoaCentroids.enabled, false);
assert.strictEqual(ui.pcoaSpiders.enabled, false);
ui.showCompanionPcoa.current = true;
events.update_control_states(ui);
assert.strictEqual(ui.pcoaDisplayFactor.enabled, true);
assert.strictEqual(ui.pcoaCentroids.enabled, true);
assert.strictEqual(ui.pcoaSpiders.enabled, true);

// With no primary and no additional model factor there is no eligible group
// to name, so the display target must remain disabled.
ui = makeUi({primary: [], requested: false});
events.update_control_states(ui);
assert.strictEqual(ui.pcoaDisplayFactor.enabled, false);
assert.strictEqual(ui.pcoaCentroids.enabled, false);
assert.strictEqual(ui.pcoaSpiders.enabled, false);

// Additional factors keep the display target enabled even without a
// primary factor, matching the multifactor rule.
ui = makeUi({primary: [], additional: ['site'], requested: false});
events.update_control_states(ui);
assert.strictEqual(ui.pcoaDisplayFactor.enabled, true);
assert.strictEqual(ui.pcoaCentroids.enabled, false);
assert.strictEqual(ui.pcoaSpiders.enabled, false);

ui = makeUi({additional: ['site']});
events.update_control_states(ui);
assert.strictEqual(ui.pcoaDisplayFactor.enabled, true);
assert.strictEqual(ui.pcoaCentroids.enabled, false);
assert.strictEqual(ui.pcoaSpiders.enabled, false);

// A retained eligible factor can be assigned before the companion plot is on.
// Focus must remain on this enabled target while the plot is off.
ui = makeUi({additional: ['site'], displayed: 'site', requested: false});
global.document = {activeElement: ui.pcoaDisplayFactor.element};
events.update_control_states(ui);
assert.strictEqual(ui.pcoaDisplayFactor.current, 'site');
assert.strictEqual(ui.pcoaDisplayFactor.enabled, true);
assert.strictEqual(ui.pcoaDisplayFactor.focused, 0);
ui.showCompanionPcoa.current = true;
events.update_control_states(ui);
assert.strictEqual(ui.pcoaDisplayFactor.current, 'site');
assert.strictEqual(ui.pcoaCentroids.enabled, true);
assert.strictEqual(ui.pcoaSpiders.enabled, true);

// Primary-only models allow the same pre-assignment of an eligible display
// factor while the plot is off; the R-side guard honours it on rerun.
ui = makeUi({displayed: 'group', requested: false});
global.document = {activeElement: ui.pcoaDisplayFactor.element};
events.update_control_states(ui);
assert.strictEqual(ui.pcoaDisplayFactor.current, 'group');
assert.strictEqual(ui.pcoaDisplayFactor.enabled, true);
assert.strictEqual(ui.pcoaDisplayFactor.focused, 0);
ui.showCompanionPcoa.current = true;
events.update_control_states(ui);
assert.strictEqual(ui.pcoaDisplayFactor.current, 'group');
assert.strictEqual(ui.pcoaCentroids.enabled, true);
assert.strictEqual(ui.pcoaSpiders.enabled, true);

for (const selected of ['group', 'site']) {
    ui = makeUi({additional: ['site'], displayed: selected});
    events.update_control_states(ui);
    assert.strictEqual(ui.pcoaDisplayFactor.enabled, true);
    assert.strictEqual(ui.pcoaCentroids.enabled, true);
    assert.strictEqual(ui.pcoaSpiders.enabled, true);
}

ui = makeUi({additional: ['site'], displayed: 'outside'});
global.document = {activeElement: ui.pcoaCentroids.element};
events.update_control_states(ui);
assert.strictEqual(ui.pcoaCentroids.enabled, false);
assert.strictEqual(ui.pcoaSpiders.enabled, false);
assert.strictEqual(ui.pcoaDisplayFactor.focused, 1);

ui = makeUi({additional: ['site'], displayed: 'site'});
events.update_control_states(ui);
ui.permFactors.current = [];
events.update_control_states(ui);
assert.strictEqual(ui.pcoaDisplayFactor.current, 'site');
// Returning to a primary-only model keeps the display target available so
// the retained assignment can be reviewed or replaced.
assert.strictEqual(ui.pcoaDisplayFactor.enabled, true);
assert.strictEqual(ui.pcoaCentroids.enabled, true);

ui = makeUi({additional: ['site'], displayed: 'site', requested: false});
events.update_control_states(ui);
assert.strictEqual(ui.pcoaDisplayFactor.enabled, true);
assert.strictEqual(ui.pcoaCentroids.enabled, false);
assert.strictEqual(ui.pcoaSpiders.enabled, false);

delete global.document;

// Without interactions, pairwise availability is unchanged: any non-omnibus
// test type keeps the checkbox available, and the adjustment picker follows
// the checkbox state.
ui = makeUi();
events.update_control_states(ui);
assert.strictEqual(ui.permPairwise.enabled, true);
assert.strictEqual(ui.permAdjust.enabled, false);
ui.permPairwise.current = true;
events.update_control_states(ui);
assert.strictEqual(ui.permAdjust.enabled, true);
ui.permBy.current = 'omnibus';
events.update_control_states(ui);
assert.strictEqual(ui.permPairwise.enabled, true);
assert.strictEqual(ui.permAdjust.enabled, false);

const conditionalUi = (overrides = {}) => makeUi({
    additional: ['site'],
    interactions: true,
    permBy: 'margin',
    ...overrides,
});

// With interactions selected, availability follows the conditional mode the
// interface can see. Factor retention and levels stay authoritative in R.
ui = conditionalUi();
events.update_control_states(ui);
assert.strictEqual(ui.permPairwise.enabled, true);
ui.permPairwise.current = true;
events.update_control_states(ui);
assert.strictEqual(ui.permAdjust.enabled, true);

// The interaction checkbox stays operable with additional factors even
// while pairwise comparisons are checked, including saved non-Holm values.
ui = conditionalUi({pairwise: true, adjust: 'bonferroni'});
events.update_control_states(ui);
assert.strictEqual(ui.permInteractions.enabled, true);
assert.strictEqual(ui.permPairwise.enabled, true);
assert.strictEqual(ui.permAdjust.enabled, true);

// Interface-visible support matrix for the conditional mode.
for (const [label, overrides, expected] of [
    ['sequential terms', {permBy: 'terms'}, false],
    ['omnibus test type', {permBy: 'omnibus'}, false],
    ['restricted permutations', {scheme: 'stratified'}, false],
    ['blocking variable', {strata: ['block']}, false],
    ['covariates', {covariates: ['depth']}, false],
    ['two additional factors', {additional: ['site', 'block']}, false],
    ['binary distances', {binary: true}, false],
    ['square-root distances', {sqrt: true}, false],
    ['additive constant', {add: 'cailliez'}, false],
    ['square-root transform', {transform: 'sqrt'}, false],
    ['jaccard distance', {distance: 'jaccard'}, false],
    ['bray without fourth root', {distance: 'bray'}, false],
    ['fourth-root bray', {distance: 'bray', transform: 'fourthroot'}, true],
    ['untransformed euclidean', {}, true],
]) {
    ui = conditionalUi(overrides);
    events.update_control_states(ui);
    assert.strictEqual(ui.permPairwise.enabled, expected, label);
}

// Saved invalid states remain clearable: a checked pairwise box stays
// operable, while the adjustment picker only follows a supported mode.
ui = conditionalUi({pairwise: true, permBy: 'terms'});
events.update_control_states(ui);
assert.strictEqual(ui.permInteractions.enabled, true);
assert.strictEqual(ui.permPairwise.enabled, true);
assert.strictEqual(ui.permAdjust.enabled, false);
ui = conditionalUi({pairwise: true, covariates: ['depth']});
events.update_control_states(ui);
assert.strictEqual(ui.permPairwise.enabled, true);
assert.strictEqual(ui.permAdjust.enabled, false);
ui = conditionalUi({pairwise: true, permBy: 'omnibus'});
events.update_control_states(ui);
assert.strictEqual(ui.permPairwise.enabled, true);

// Interactions still require an additional factor; checking pairwise alone
// does not make the interaction checkbox operable.
ui = makeUi({pairwise: true});
events.update_control_states(ui);
assert.strictEqual(ui.permInteractions.enabled, false);

process.stdout.write('ok\n');
