interpolateAtTemp = function(fitList, temp) {
    #' Interpolate thermal performance using a list of fits
    #'
    #' @param fitList List of model fits (output from fitModels())
    #' @param temp Numerical value (not a vector with multiple values),
    #'   temperature value for which interpolation is to be calculated. 
    #' 
    #' @return list
    #'
    #' @export
    weights = calculateAIC(fitList)
    fits = fitList
    fits[["data"]] = NULL # Remove the original data, keep only fits
    predictions = list()
    for (n in names(fits)) {
        predictions[[n]] = predict(fits[[n]], newdata = data.frame(x = temp))
    }
    predictions = data.frame(modelName = names(fits),
                             interpolated = unlist(predictions))
    weightPred = merge(weights, predictions, by = "modelName", all = T)
    out = list()
    out[["models"]] = weightPred
    out[["prediction"]] = sum(weightPred$modelWeight * weightPred$interpolated)
    return(out)
}
