fitModel = function(customModel, x, y, initParams = NULL) {
    #' Fit a previously built model on some experimental data
    #'
    #' The fitting itself is performed with the nlsLM() function from the
    #' minpack.lm package.
    #'
    #' @param customModel Model built with buildModel()
    #' @param x Numeric vector, x data
    #' @param y Numeric vector of same length as x, y data
    #' @param initParams Named list of starting values for model fitting, if
    #'   NULL then the default values specified at model building are used
    #'
    #' @return Output from nlsLM
    #'
    #' @export
    #'
    if (is.null(initParams)) {
        initParams = customModel[["starting"]]
    }
    fit = nlsLM(customModel[["formula"]],
                start = customModel[["starting"]],
                data = data.frame(x = x, y = y),
                control = nls.lm.control(maxiter = 1e3))
    return(fit)
}
