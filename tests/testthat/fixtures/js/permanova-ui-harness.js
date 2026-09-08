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
}={}) => ({
    factor: control(primary),
    permFactors: control(additional),
    pcoaDisplayFactor: control(displayed),
    showCompanionPcoa: control(requested),
    pcoaCentroids: control(false),
    pcoaSpiders: control(false),
    permPairwise: control(false),
    permInteractions: control(false),
    permBy: control('terms'),
    permAdjust: control('holm'),
});

let ui = makeUi({requested: false});
events.update_control_states(ui);
assert.strictEqual(ui.pcoaDisplayFactor.enabled, false);
assert.strictEqual(ui.pcoaCentroids.enabled, false);
assert.strictEqual(ui.pcoaSpiders.enabled, false);
ui.showCompanionPcoa.current = true;
events.update_control_states(ui);
assert.strictEqual(ui.pcoaDisplayFactor.enabled, false);
assert.strictEqual(ui.pcoaCentroids.enabled, true);
assert.strictEqual(ui.pcoaSpiders.enabled, true);

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
assert.strictEqual(ui.pcoaDisplayFactor.enabled, false);
assert.strictEqual(ui.pcoaCentroids.enabled, true);

ui = makeUi({additional: ['site'], displayed: 'site', requested: false});
events.update_control_states(ui);
assert.strictEqual(ui.pcoaDisplayFactor.enabled, true);
assert.strictEqual(ui.pcoaCentroids.enabled, false);
assert.strictEqual(ui.pcoaSpiders.enabled, false);

delete global.document;
process.stdout.write('ok\n');
