findCT = function(fitList, interval, fractionOptimum) {
    #' Find the low and high critical temperatures (CT_low and CT_high) for
    #' which the average performance curve built from fitList crosses the
    #' threshold fractionOptimum * optimumPerformance.
    #'
    #' Note: No check is done to ensure that those CTs are unique. The curve
    #' should be checked visually to ensure that the analysis makes sense.
    #'
    #' @param fitList List of fits, output from fitModels()
    #' @param interval Vector determining the range in which CTs are searched
    #'   for (e.g. c(15, 40))
    #' @param fractionOptimum Numeric between 0 and 1
    #'
    #' @return Named vector
    #'
    #' @export
    curve = averageCurve(fitList, seq(interval[1], interval[2], length.out = 256))
    curveFun = splinefun(curve)
    optimum = findOptimum(fitList, interval)
    threshold = optimum["optPerf"] * fractionOptimum
    CTlow = tryCatch(
        {uniroot(function(x) curveFun(x) - threshold,
                 interval = c(interval[1], optimum["optTemp"]))$root},
        error = function(e) NA)
    CThigh = tryCatch(
        {uniroot(function(x) curveFun(x) - threshold,
                 interval = c(optimum["optTemp"], interval[2]))$root},
        error = function(e) NA)
    out = c(CTlow, CThigh)
    names(out) = c("CT_low", "CT_high")
    return(out)
}
