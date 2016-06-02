calculateAIC = function(fits) {
    #' Calculate AIC and AIC weights for a list of fits
    #'
    #' @param fits List of fit results, e.g. output from fitModels()
    #'
    #' @return Data frame with class "fitAIC"
    #'
    #' @export
    #'
    #' @examples
    #' x = c(23, 25, 27, 29, 31, 32, 33)
    #' y = c(0.1, 0.25, 0.3, 0.6, 0.8, 0.6, 0.1)
    #' fits = fitModels(getModelLibrary(), x, y)
    #' calculateAIC(fits)
    #'
    modelAIC = unlist(lapply(fits, AIC))
    deltaAIC = modelAIC - min(modelAIC)
    modelWeight = exp(-deltaAIC/2) / sum(exp(-deltaAIC/2))
    out = data.frame(modelName = names(fits),
                     modelAIC, deltaAIC, modelWeight)
    class(out) = c("fitAIC", "data.frame")
    return(out)
}
