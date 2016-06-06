plot.fitList = function(fitList, fitCol = NULL, drawAverageFit = T, ...) {
    #'
    #' Accepted parameters: xlim, ylim
    #'
    #' @param fitCol If set to "weights", color the fit curves based on their
    #'   AIC weight
    #' @param drawAverageFit If TRUE, draw an average fit based on the AIC weights
    #'
    #' @export
    # http://ipub.com/r-three-dots-ellipsis/
    arguments = list(...)
    d = fitList[["data"]]
    fitList[["data"]] = NULL
    # Default arguments
    defaults = list(xlim = range(d$x, na.rm = T),
                    ylim = range(d$y, na.rm = T),
                    xlab = "x",
                    ylab = "y",
                    main = "")
    args = defaults
    # Update args
    for (n in names(arguments)) {
        if (n %in% names(args)) {
            args[[n]] = arguments[[n]]
        }
    }
    # Plot
    plot(d, xlim = args[["xlim"]], ylim = args[["ylim"]], pch = 21,
         bg = "black", xlab = args[["xlab"]], ylab = args[["ylab"]], las = 1,
         bty = "n", main = args[["main"]])
    # Fit colors
    if (!is.null(fitCol) && fitCol == "weights") {
        weights = calculateAIC(fitList)
        weights$col = plotrix::color.scale(weights$modelWeight,
                                           c(0, 1, 1), 0, c(1, 1, 0),
                                           xrange = c(0, 1))
                                           
    } else {
        weights = calculateAIC(fitList)
        weights$col = "lightgrey"
    }
    # Fits
    xlim = args[["xlim"]]
    xp = seq(xlim[1], xlim[2], length.out = 512)
    for (m in names(fitList)) {
        fit = fitList[[m]]
        yp = predict(fit, newdata = data.frame(x = xp))
        lines(xp, yp, col = weights$col[weights$modelName == m])
    }
    # Average fit
    if (drawAverageFit) {
        xp = seq(xlim[1], xlim[2], length.out = 256)
        yp = matrix(NA, ncol = length(fitList), nrow = length(xp))
        for (i in 1:length(fitList)) {
            yp[, i] = predict(fitList[[i]], newdata = data.frame(x = xp))
        }
        mOrder = names(fitList)
        yWeights = weights$modelWeight
        yWeightsNames = weights$modelName
        orderedIndices = sapply(mOrder, function(x) which(yWeightsNames == x))
        yWeights = yWeights[orderedIndices]
        stopifnot(length(yWeights) == ncol(yp))
        yAverage = apply(yp, 1, function(x) sum(x * yWeights))
        lines(xp, yAverage, col = "green", lwd = 3)
    }
}
