fitModels = function(customModels, x, y, initParams = NULL) {
    #' Fit a list of previously built models on some experimental data
    #'
    #' The fitting itself is performed with the nlsLM() function from the
    #' minpack.lm package.
    #'
    #' @param customModels List of models built with buildModel()
    #' @param x Numeric vector, x data
    #' @param y Numeric vector of same length as x, y data
    #' @param initParams List of named lists of starting values for model fitting, if
    #'   NULL then the default values specified at model building are used
    #'
    #' @return Output from nlsLM
    #'
    #' @export
    #'
    fit = list()
    for (i in 1:length(customModels)) {
        if (is.null(initParams)) {
            initParam = NULL
        } else {
            initParam = initParams[[i]]
        }
        fit[[i]] = fitModel(customModels[[i]], x, y, initParam)
    }
    names(fit) = names(customModels)
    return(fit)
}
