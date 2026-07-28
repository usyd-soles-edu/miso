'use strict';

const hasSelection = value => Array.isArray(value)
    ? value.length > 0
    : value !== null && value !== undefined && value !== '';

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
        Number(ui.seed.value()) !== 0 ||
        ui.useParallel.value();

    if (studyIsActive)
        ui.studyDesign.expand();
    if (reproducibilityIsActive)
        ui.reproducibility.expand();
};

const refreshView = ui => {
    updateControlStates(ui);
    revealActiveSections(ui);
};

module.exports = {
    view_loaded(ui) {
        refreshView(ui);
        setTimeout(() => refreshView(ui), 100);
    },
    update_control_states(ui) {
        updateControlStates(ui);
    },
};
