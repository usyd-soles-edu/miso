'use strict';

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
        Number(ui.seed.value()) !== 0 ||
        ui.useParallel.value();

    if (advancedIsActive)
        ui.advancedOptions.expand();
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
