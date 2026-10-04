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

const syncSeedControl = ui => {
    ui.seed.setEnabled(ui.useFixedSeed.value());
};

const normaliseLegacySeedMode = ui => {
    if (ui.useFixedSeed.value() && Number(ui.seed.value()) === 0)
        ui.useFixedSeed.setValue(false);
};

const enforcePositiveSeed = ui => {
    if (ui.useFixedSeed.value() && Number(ui.seed.value()) === 0)
        ui.seed.setValue(123);
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

// Distance settings the conditional (simple-effect) pairwise mode supports.
// The interface cannot know which additional factors the fitted model
// retains or their levels, so the R-side guard stays authoritative; this is
// an availability hint only.
const conditionalDistanceMode = ui =>
    !ui.distBinary.value() &&
    !ui.distSqrt.value() &&
    ui.distAdd.value() === 'none' && (
        (ui.distance.value() === 'euclidean' && ui.transform.value() === 'none') ||
        (ui.distance.value() === 'bray' && ui.transform.value() === 'fourthroot'));

const updateControlStates = ui => {
    const pairwise = ui.permPairwise.value();
    const interactions = ui.permInteractions.value();
    const additional = selections(ui.permFactors.value());
    const pooledSupported = !interactions && ui.permBy.value() !== 'omnibus';
    const conditionalSupported = interactions &&
        ui.permBy.value() === 'margin' &&
        additional.length === 1 &&
        ui.permScheme.value() === 'free' &&
        !hasSelection(ui.strata.value()) &&
        !hasSelection(ui.covariates.value()) &&
        conditionalDistanceMode(ui);
    const pairwiseSupported = pooledSupported || conditionalSupported;

    // Interactions stay operable with additional factors even while pairwise
    // comparisons are checked: the backend explains invalid combinations.
    ui.permInteractions.setEnabled(interactions || additional.length > 0);
    ui.permPairwise.setEnabled(pairwise || pairwiseSupported);
    ui.permAdjust.setEnabled(pairwise && pairwiseSupported);

    const primary = selections(ui.factor.value());
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
    const displayEnabled = primary.length > 0 || multifactor;
    const centroidsEnabled = requested && validGroup;
    const spidersEnabled = requested && validGroup;

    // Keep this target available while the plot is off so users can assign a
    // model factor before enabling the companion PCoA. Primary-only models
    // keep it available too: the primary factor drives the automatic display
    // group. The R-side guard explains retained, dropped, and unavailable
    // assignments.
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
    const reproducibilityIsActive =
        Number(ui.permN.value()) !== 999 ||
        ui.useFixedSeed.value() ||
        ui.useParallel.value();

    if (reproducibilityIsActive)
        ui.reproducibility.expand();
    if (ui.showCompanionPcoa.value() ||
            hasSelection(ui.pcoaDisplayFactor.value()) ||
            ui.pcoaCentroids.value() || ui.pcoaSpiders.value())
        ui.plots.expand();
};

const refreshView = ui => {
    syncSeedControl(ui);
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
