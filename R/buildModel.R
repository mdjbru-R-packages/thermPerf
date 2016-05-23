buildModel = function(mFun, mName, mFormula, mParams, mStarting) {
    #' Build a model object used for model fitting later on
    #'
    #' @param mFun Function of the form f(x, params), where params is a named
    #'   list of parameters and x a vector of input data
    #' @param mName String, name of the model
    #' @param mFormula Formula describing the model
    #' @param mParams Vector of strings containing the parameters names
    #' @param mStarting Named list with default starting values when fitting
    #'   the model to some data
    #'
    #' @return An object of class "customModel" (basically a list)
    #'
    #' @examples
    #' # Simple linear model
    #'
    #' f = function(x, params) {
    #'       a = params[["a"]]
    #'       b = params[["b"]]
    #'       return(a + b * x)
    #'     }
    #'
    #' myModel = buildModel(f, "linear model",
    #'             y ~ a + b * x, c("a", "b"),
    #'             list(a = 1,  b= 1))
    #'
    #' x = c(1, 1, 2, 3, 4, 4, 5, 7)
    #' y = c(2, 4, 3, 7, 8, 7, 10, 11)
    #'
    #' fit = fitModel(myModel, x, y)
    #'
    #' @export
    #'
    m = list("function" = mFun,
             "name" = mName,
             "formula" = mFormula,
             "parameters" = mParams,
             "starting" = mStarting)
    class(m) = "customModel"
    return(m)
}
