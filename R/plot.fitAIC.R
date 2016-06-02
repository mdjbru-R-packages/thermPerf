plot.fitAIC = function(fitAIC, ...) {
    #' @export
    barplot(fitAIC$modelWeight, ylim = c(0, 1), names.arg = fitAIC$modelName,
            las = 2, ylab = "AIC weight", ...)
}
