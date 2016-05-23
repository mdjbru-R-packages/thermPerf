summary.customModel = function(customModel) {
    #' @export
    m = customModel
    cat("Custom model: ", m[["name"]], "\n", sep = "")
    cat("\n")
    formulaChr = as.character(m[["formula"]])
    # http://stackoverflow.com/questions/5951500/is-it-possible-to-make-print-formula-respect-the-environment-width-option
    formulaChr = paste(formulaChr[c(2, 1, 3)], collapse = " ")
    cat("  model formula: ", formulaChr, "\n", sep = "")
    cat("  parameters (n=", length(m[["parameters"]]), "): ",
        paste(m[["parameters"]], collapse = ", "), "\n", sep = "")
    cat("\n")
}

print.customModel = function(customModel) {
    #' @export
    summary.customModel(customModel)
}
