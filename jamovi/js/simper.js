'use strict';

const updateControlStates = ui => {
    ui.simperN.setEnabled(ui.simperAssess.value());
    ui.simperAdjust.setEnabled(ui.simperAssess.value());
    ui.seed.setEnabled(ui.simperAssess.value());
};

const revealActiveSections = ui => {
    const assessmentIsActive =
        ui.simperAssess.value() ||
        Number(ui.simperN.value()) !== 999 ||
        ui.simperAdjust.value() !== 'holm' ||
        Number(ui.seed.value()) !== 0;

    if (assessmentIsActive)
        ui.permutationAssessment.expand();
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
