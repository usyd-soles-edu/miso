'use strict';

const updateControlStates = ui => {
    const defining = ui.defineClusters.value();
    const byNumber = ui.cutMode.value() === 'number';

    ui.cutMode.setEnabled(defining);
    ui.numberClusters.setEnabled(defining && byNumber);
    ui.cutHeight.setEnabled(defining && !byNumber);
};

const refreshView = ui => {
    updateControlStates(ui);
    ui.plots.expand();
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
