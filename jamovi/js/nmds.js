'use strict';

const hasValue = value => {
    if (Array.isArray(value))
        return value.length > 0;
    return value !== null && value !== undefined && value !== '';
};

const controlElement = control => {
    if (!control)
        return null;
    if (control.$el && control.$el[0])
        return control.$el[0];
    if (control.el)
        return control.el;
    return null;
};

const containsFocus = control => {
    if (typeof document === 'undefined')
        return false;
    const element = controlElement(control);
    return Boolean(element && element.contains(document.activeElement));
};

const focusControl = control => {
    if (control && typeof control.focus === 'function') {
        control.focus();
        return;
    }
    const element = controlElement(control);
    if (element && typeof element.focus === 'function')
        element.focus();
};

const updateControlStates = ui => {
    const hasGroup = hasValue(ui.factor.value());
    const hasEnvironment = hasValue(ui.nmdsEnv.value());
    const groupControls = [ui.nmdsOverlay, ui.nmdsHull, ui.nmdsEllipse, ui.nmdsSpider];
    const environmentControls = [ui.nmdsEnvPerm];
    const groupHadFocus = groupControls.some(containsFocus);
    const environmentHadFocus = environmentControls.some(containsFocus);

    ui.nmdsOverlay.setEnabled(hasGroup);
    ui.nmdsHull.setEnabled(hasGroup);
    ui.nmdsEllipse.setEnabled(hasGroup);
    ui.nmdsSpider.setEnabled(hasGroup);
    ui.nmdsEnvPerm.setEnabled(hasEnvironment);

    if (!hasGroup && groupHadFocus)
        setTimeout(() => focusControl(ui.factor), 0);
    if (!hasEnvironment && environmentHadFocus)
        setTimeout(() => focusControl(ui.nmdsEnv), 0);
};

const revealActiveSections = ui => {
    if (hasValue(ui.factor.value()) && (
        ui.nmdsOverlay.value() || ui.nmdsHull.value() ||
        ui.nmdsEllipse.value() || ui.nmdsSpider.value()))
        ui.groupOptions.expand();

    if (hasValue(ui.nmdsEnv.value()) || Number(ui.nmdsEnvPerm.value()) !== 99)
        ui.environmentAssessment.expand();

    if (Number(ui.seed.value()) !== 0 ||
        Number(ui.nmdsTrymax.value()) !== 20 ||
        Number(ui.nmdsMaxit.value()) !== 200)
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
