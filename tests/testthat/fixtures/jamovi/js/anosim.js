'use strict';

const hasSelection = value => Array.isArray(value)
    ? value.length > 0
    : value !== null && value !== undefined && value !== '';

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
    ui.anosimAdjust.setEnabled(ui.anosimPairwise.value());
};

const revealActiveSections = ui => {
    ui.plots.expand();
    const studyIsActive =
        hasSelection(ui.strata.value()) ||
        ui.permRestriction.value() !== 'free';

    const reproducibilityIsActive =
        Number(ui.anosimN.value()) !== 999 ||
        ui.useFixedSeed.value() ||
        ui.useParallel.value();

    if (studyIsActive)
        ui.studyDesign.expand();
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
