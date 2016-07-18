findOptimum = function(fitList, interval) {
    #' Find the optimum performance and the temperature at which it occurs
    #' within a given interval for a list of fits (using their AIC-weighted
    #' average)
    #'
    #' @param fitList List of fits, output from fitModels()
    #' @param interval Vector determining the range in which optimum is searched
    #'   for (e.g. c(15, 40))
    #'
    #' @return Named vector with optimum performance and temperature
    #'
    #' @export
    curve = averageCurve(fitList, seq(interval[1], interval[2], length.out = 256))
    curveFun = splinefun(curve)
    optTemp = optimize(function(x) -curveFun(x), interval = interval)$minimum
    optPerf = curveFun(optTemp)
    out = c(optTemp, optPerf)
    names(out) = c("optTemp", "optPerf")
    return(out)
}
