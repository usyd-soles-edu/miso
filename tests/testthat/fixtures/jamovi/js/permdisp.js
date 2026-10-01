'use strict';

const syncSeedControl = ui => {
    ui.seed.setEnabled(ui.useFixedSeed.value());
};

const normaliseLegacySeedMode = ui => {
    if (ui.useFixedSeed.value() && Number(ui.seed.value()) === 0)
        ui.useFixedSeed.setValue(false);
};

const enforcePositiveSeed = ui => {
    if (ui.useFixedSeed.value() && Number(ui.seed.value()) === 0)
        ui.seed.setValue(123);
};

const updateControlStates = ui => {
    ui.dispAdjust.setEnabled(ui.dispPairwise.value());
};

const revealActiveSections = ui => {
    const advancedIsActive =
        ui.distBinary.value() ||
        ui.distSqrt.value() ||
        ui.distAdd.value() !== 'none' ||
        ui.dispBias.value();

    const reproducibilityIsActive =
        Number(ui.permN.value()) !== 999 ||
        ui.permRestriction.value() !== 'free' ||
        ui.useFixedSeed.value() ||
        ui.useParallel.value();

    if (advancedIsActive)
        ui.advancedOptions.expand();
    if (reproducibilityIsActive)
        ui.reproducibility.expand();
};

const refreshView = ui => {
    syncSeedControl(ui);
    updateControlStates(ui);
    revealActiveSections(ui);
};

module.exports = {
    // jamovi hydrates stored options after View.loaded, then calls updated.
    view_updated(ui) {
        normaliseLegacySeedMode(ui);
        refreshView(ui);
    },
    fixed_seed_changed(ui) {
        enforcePositiveSeed(ui);
        refreshView(ui);
    },
    seed_changed(ui) {
        enforcePositiveSeed(ui);
        refreshView(ui);
    },
    update_control_states(ui) {
        updateControlStates(ui);
    },
};
