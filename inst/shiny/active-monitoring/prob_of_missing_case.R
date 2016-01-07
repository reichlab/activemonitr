#' Calculate probabilility of a case not being caught by monitoring
#' @param dat a data.frame with column names explained in details
#' @return the inputted data.frame with an additional column called 'p'
#'
#' @details Column names of the dat argument
#' \itemize{
#'  \item{"phi"}{the probability of an exposed individual becoming symptomatic}
#'  \item{"shape"}{shape parameter of the gamma distribution for incubation periods}
#'  \item{"scale"}{scale parameter of the gamma distribution for incubation periods}
#'  \item{"d"}{the duration of the active monitoring period}
#'  \item{"u"}{the assumed number of days between exposure and the beginning of monitoring}
#' }
#' @export
prob_of_missing_case <- function(dat) {
    require(dplyr)
    mutate(dat, p = pgamma(d+u, shape=shape, scale=scale, lower.tail = FALSE) * phi)
}

