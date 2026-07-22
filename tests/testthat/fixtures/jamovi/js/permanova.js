'use strict';

const hasSelection = value => Array.isArray(value)
    ? value.length > 0
    : value !== null && value !== undefined && value !== '';

const selections = value => {
    if (!hasSelection(value))
        return [];
    return (Array.isArray(value) ? value : [value]).map(String);
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

const previousModelFactors = new WeakMap();

const updateControlStates = ui => {
    const pairwise = ui.permPairwise.value();
    const interactions = ui.permInteractions.value();
    const pairwiseSupported = !interactions && ui.permBy.value() !== 'omnibus';

    ui.permInteractions.setEnabled(
        interactions || (!pairwise && hasSelection(ui.permFactors.value())));
    ui.permPairwise.setEnabled(pairwise || pairwiseSupported);
    ui.permAdjust.setEnabled(pairwise && pairwiseSupported);

    const primary = selections(ui.factor.value());
    const additional = selections(ui.permFactors.value());
    const modelFactors = primary.concat(additional);
    const multifactor = additional.length > 0;
    let displayed = selections(ui.pcoaDisplayFactor.value());
    const previousFactors = previousModelFactors.get(ui) || [];
    const displayWasRemoved = displayed.length === 1 &&
        previousFactors.includes(displayed[0]) &&
        !modelFactors.includes(displayed[0]);
    if ((!multifactor && displayed.length > 0) || displayWasRemoved) {
        ui.pcoaDisplayFactor.setValue(null);
        displayed = [];
    }

    const requested = ui.showCompanionPcoa.value();
    const automaticGroup = !multifactor && primary.length === 1;
    const explicitGroup = multifactor && displayed.length === 1 &&
        modelFactors.includes(displayed[0]);
    const validGroup = automaticGroup || explicitGroup;
    const dependentControls = [
        ui.pcoaDisplayFactor, ui.pcoaCentroids, ui.pcoaSpiders,
    ];
    const dependentHadFocus = dependentControls.some(containsFocus);

    ui.pcoaDisplayFactor.setEnabled(requested && multifactor);
    ui.pcoaCentroids.setEnabled(requested && validGroup);
    ui.pcoaSpiders.setEnabled(requested && validGroup);

    if ((!requested || !validGroup) && dependentHadFocus) {
        const destination = requested && multifactor
            ? ui.pcoaDisplayFactor
            : ui.showCompanionPcoa;
        setTimeout(() => focusControl(destination), 0);
    }
    previousModelFactors.set(ui, modelFactors);
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
    if (ui.showCompanionPcoa.value() ||
            hasSelection(ui.pcoaDisplayFactor.value()) ||
            ui.pcoaCentroids.value() || ui.pcoaSpiders.value())
        ui.plots.expand();
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
