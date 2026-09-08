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
    const displayed = selections(ui.pcoaDisplayFactor.value());

    const requested = ui.showCompanionPcoa.value();
    const automaticGroup = !multifactor && primary.length === 1;
    const explicitGroup = multifactor && displayed.length === 1 &&
        modelFactors.includes(displayed[0]);
    const validGroup = automaticGroup || explicitGroup;
    const dependentControls = [
        ui.pcoaDisplayFactor, ui.pcoaCentroids, ui.pcoaSpiders,
    ];
    const dependentHadFocus = dependentControls.some(containsFocus);
    const displayEnabled = multifactor;
    const centroidsEnabled = requested && validGroup;
    const spidersEnabled = requested && validGroup;

    // Keep this target available while the plot is off so users can assign a
    // model factor before enabling the companion PCoA. The R-side guard
    // explains retained, dropped, and unavailable assignments.
    ui.pcoaDisplayFactor.setEnabled(displayEnabled);
    ui.pcoaCentroids.setEnabled(centroidsEnabled);
    ui.pcoaSpiders.setEnabled(spidersEnabled);

    const focusedDependentDisabled = dependentHadFocus && (
        (containsFocus(ui.pcoaDisplayFactor) && !displayEnabled) ||
        (containsFocus(ui.pcoaCentroids) && !centroidsEnabled) ||
        (containsFocus(ui.pcoaSpiders) && !spidersEnabled));
    if (focusedDependentDisabled) {
        const destination = displayEnabled
            ? ui.pcoaDisplayFactor
            : ui.showCompanionPcoa;
        setTimeout(() => focusControl(destination), 0);
    }
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
