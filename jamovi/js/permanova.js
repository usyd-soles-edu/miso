'use strict';

const hasSelection = value => Array.isArray(value)
    ? value.length > 0
    : value !== null && value !== undefined && value !== '';

const updateControlStates = ui => {
    const pairwise = ui.permPairwise.value();
    const interactions = ui.permInteractions.value();
    const pairwiseSupported = !interactions && ui.permBy.value() !== 'omnibus';

    ui.permInteractions.setEnabled(
        interactions || (!pairwise && hasSelection(ui.permFactors.value())));
    ui.permPairwise.setEnabled(pairwise || pairwiseSupported);
    ui.permAdjust.setEnabled(pairwise && pairwiseSupported);
};

const revealActiveSections = ui => {
    const studyIsActive =
        hasSelection(ui.permFactors.value()) ||
        hasSelection(ui.strata.value()) ||
        hasSelection(ui.covariates.value()) ||
        ui.permInteractions.value() ||
        ui.permBy.value() !== 'terms' ||
        ui.permScheme.value() !== 'free';

    const reproducibilityIsActive =
        Number(ui.permN.value()) !== 999 ||
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
