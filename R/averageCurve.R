averageCurve = function(fitList, xPredict) {
    #' Calculate an average curve from a list of fits, using the AIC weights.
    #'
    #' @param fitList List of fitted models (from fitModels())
    #' @param xPredict Vector of values where the curve should be estimated
    #'
    #' @return A data frame with x and y columns
    #'
    #' @export
    estimates = list()
    weights = na.omit(calculateAIC(fitList)[, c("modelName", "modelWeight")])
    for (i in 1:nrow(weights)) {
        modelName = as.character(weights$modelName[i])
        y = predict(fitList[[modelName]], newdata = data.frame(x = xPredict))
        estimates[[modelName]] = y * weights$modelWeight[i]
    }
    estimates = as.data.frame(estimates)
    estimates = apply(estimates, 1, sum)
    return(data.frame(x = xPredict, y = estimates))
}
