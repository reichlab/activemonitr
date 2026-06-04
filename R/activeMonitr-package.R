#' activeMonitr: evaluating public health active monitoring
#'
#' Data and functions for evaluating public health active monitoring programs
#' and estimating incubation periods, with an accompanying Shiny application.
#'
#' @keywords internal
#' @importFrom utils globalVariables
"_PACKAGE"

## Quiet R CMD check NOTEs about non-standard evaluation (dplyr/ggplot2 aes)
## referring to data-frame columns that have no visible binding.
globalVariables(c(
    "Var1", "Var2", "value", "p95", "disease",
    "d", "u", "shape", "rate", "meanlog", "sdlog",
    "phi", "phi_lab", "p", "p50", "ltp", "utp"
))
