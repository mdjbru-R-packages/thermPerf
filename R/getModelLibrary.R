getModelLibrary = function() {
    #' Get model library
    #'
    #' Store the models included in the package to a list
    #'
    #' @return A list containing pre-built models
    #' @export
    #' @examples
    #' models = getModelLibrary()
    #' names(models) # To see the models included
    #'
    models = list()
    ## Marchioro and Foerster 2011
    ### ** mLinearFit
    mFun = function(x, params) {
        a = params[["a"]]
        b = params[["b"]]
        return(a + b * x)
    }
    mName = "Linear fit"
    mFormula = y ~ a + b * x
    mParams = c("a", "b")
    mStarting = list(a = 0, b = 1)
    models[["linearFit"]] = buildModel(mFun, mName, mFormula, mParams, mStarting)

    ### ** mBriere1

    mFun = function(x, params) {
        a = params[["a"]]
        t0 = params[["t0"]]
        tmax = params[["tmax"]]
        return(a * x * (x - t0) * (tmax - x) ^ (1/2))
    }
    mName = "Briere 1"
    mFormula = y ~ a * x * (x - t0) * (tmax - x) ^ (1/2)
    mParams = c("a", "t0", "tmax")
    mStarting = list(a = 1, t0 = 15, tmax = 35)
    models[["briere1"]] = buildModel(mFun, mName, mFormula, mParams, mStarting)

    ### ** mBriere2

    mFun = function(x, params) {
        a = params[["a"]]
        t0 = params[["t0"]]
        tmax = params[["tmax"]]
        m = params[["m"]]
        return(a * x * (x - t0) * (tmax - x) ^ (1/m))
    }
    mName = "Briere 2"
    mFormula = y ~ a * x * (x - t0) * (tmax - x) ^ (1/m)
    mParams = c("a", "t0", "tmax", "m")
    mStarting = list(a = 1, t0 = 15, tmax = 35, m = 2)
    models[["briere2"]] = buildModel(mFun, mName, mFormula, mParams, mStarting)

    ### ** mLactin1

    mFun = function(x, params) {
        rho = params[["rho"]]
        tmax = params[["tmax"]]
        delta = params[["delta"]]
        return(exp(rho * x) - exp(rho * tmax - (tmax - x) / delta))
    }
    mName = "Lactin 1"
    mFormula = y ~ exp(rho * x) - exp(rho * tmax - (tmax - x) / delta)
    mParams = c("rho", "tmax", "delta")
    mStarting = list(rho = 1, tmax = 35, delta = 20)
    models[["lactin1"]] = buildModel(mFun, mName, mFormula, mParams, mStarting)

    ### ** mCandidate01

    mFun = function(x, params) {
        #' y = a + b * x + c * log(x) ^ 2 + d * x ^ (1/2)
        #'
        #' Parameters: a, b, c, d
        #'
        a = params[["a"]]
        b = params[["b"]]
        c = params[["c"]]
        d = params[["d"]]
        return(a + b * x + c * log(x) ^ 2 + d * x ^ (1/2))
    }
    mName = "Candidate 1 tableCurve"
    mFormula = y ~ a + b * x + c * log(x) ^ 2 + d * x ^ (1/2)
    mParams = c("a", "b", "c", "d")
    mStarting = list(a = 1, b = 1, c = 0.5, d = 1)
    models[["candidate01"]] = buildModel(mFun, mName, mFormula, mParams, mStarting)

    ### ** mCandidate02

    mFun = function(x, params) {
        #' y = a + b * log(x) ^ 2 + c * log(x) + d * log(x) / x
        #'
        #' Parameters: a, b, c, d
        #'
        a = params[["a"]]
        b = params[["b"]]
        c = params[["c"]]
        d = params[["d"]]
        return(a + b * log(x) ^ 2 + c * log(x) + d * log(x) / x)
    }
    mName = "Candidate 2 tableCurve"
    mFormula = y ~ a + b * log(x) ^ 2 + c * log(x) + d * log(x) / x
    mParams = c("a", "b", "c", "d")
    mStarting = list(a = 1, b = 1, c = 0.5, d = 1)
    models[["candidate02"]] = buildModel(mFun, mName, mFormula, mParams, mStarting)

    ### ** mCandidate03

    mFun = function(x, params) {
        #' y = a + b * x^2 * log(x) + c * x^3
        #'
        #' Parameters: a, b, c
        #'
        a = params[["a"]]
        b = params[["b"]]
        c = params[["c"]]
        return(a + b * x^2 * log(x) + c * x^3)
    }
    mName = "Candidate 3 tableCurve"
    mFormula = y ~ a + b * x^2 * log(x) + c * x^3
    mParams = c("a", "b", "c")
    mStarting = list(a = 0, b = 1, c = 1)
    models[["candidate03"]] = buildModel(mFun, mName, mFormula, mParams, mStarting)

    ### ** mCandidate04

    mFun = function(x, params) {
        #' y = a + b * x + c * x^2
        #'
        #' Parameters: a, b, c
        #'
        a = params[["a"]]
        b = params[["b"]]
        c = params[["c"]]
        return(a + b * x + c * x^2)
    }
    mName = "Candidate 4 tableCurve"
    mFormula = y ~ a + b * x + c * x^2
    mParams = c("a", "b", "c")
    mStarting = list(a = 0, b = 1, c = 1)
    models[["candidate04"]] = buildModel(mFun, mName, mFormula, mParams, mStarting)

    ### ** mCandidate05

    mFun = function(x, params) {
        #' y = a + b * x^(1.5) + c * x^2
        #'
        #' Parameters: a, b, c
        #'
        a = params[["a"]]
        b = params[["b"]]
        c = params[["c"]]
        return(a + b * x ^ (1.5) + c * x^2)
    }
    mName = "Candidate 5 tableCurve"
    mFormula = y ~ a + b * x ^ (1.5) + c * x^2
    mParams = c("a", "b", "c")
    mStarting = list(a = 0, b = 1, c = 1)
    models[["candidate05"]] = buildModel(mFun, mName, mFormula, mParams, mStarting)

    ### ** mCandidate06

    mFun = function(x, params) {
        #' y = 1 / (a + b * exp(x) + c * exp(-x))
        #'
        #' Parameters: a, b, c
        #'
        a = params[["a"]]
        b = params[["b"]]
        c = params[["c"]]
        return(1 / (a + b * exp(x) + c * exp(-x)))
    }
    mName = "Candidate 6 tableCurve"
    mFormula = y ~ 1 / (a + b * exp(x) + c * exp(-x))
    mParams = c("a", "b", "c")
    mStarting = list(a = 0.4, b = 1e-16, c = 1e6)
    models[["candidate06"]] = buildModel(mFun, mName, mFormula, mParams, mStarting)
    
    return(models)
}
