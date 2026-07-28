const events = {
    update_control_states: function(ui) {
        const grouped = ui.factor.value() !== null && ui.factor.value() !== '';
        ui.showCentroids.setEnabled(grouped);
        ui.showSpiders.setEnabled(grouped);
        if (!grouped) {
            ui.showCentroids.setValue(false);
            ui.showSpiders.setValue(false);
        }
    },

    view_loaded: function(ui) {
        events.update_control_states(ui);
    }
};

module.exports = events;
