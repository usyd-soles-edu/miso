'use strict';

// Observed contrast labels only exist after the R analysis runs, while jamovi
// List choices are fixed by the analysis schema. The results therefore use a
// bounded, independently titled plot item for each observed contrast.

const updateControlStates = ui => {
    ui.useFixedSeed.setEnabled(ui.simperAssess.value());
    ui.simperN.setEnabled(ui.simperAssess.value());
    ui.simperAdjust.setEnabled(ui.simperAssess.value());
    ui.seed.setEnabled(ui.simperAssess.value() && ui.useFixedSeed.value());
};

const normaliseLegacySeedMode = ui => {
    if (ui.useFixedSeed.value() && Number(ui.seed.value()) === 0)
        ui.useFixedSeed.setValue(false);
};

const enforcePositiveSeed = ui => {
    if (ui.useFixedSeed.value() && Number(ui.seed.value()) === 0)
        ui.seed.setValue(123);
};

const revealActiveSections = ui => {
    ui.plots.expand();
    const assessmentIsActive =
        ui.simperAssess.value() ||
        Number(ui.simperN.value()) !== 999 ||
        ui.simperAdjust.value() !== 'holm' ||
        ui.useFixedSeed.value();

    if (assessmentIsActive)
        ui.permutationAssessment.expand();
};

const refreshView = ui => {
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
