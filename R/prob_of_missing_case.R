#' Calculate probabilility of a case not being caught by monitoring
#' @param dat a data.frame with column names explained in details
#' @param dist character string specifying the parametric distribution of dat
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
prob_of_missing_case <- function(dat, dist="gamma") {
    require(dplyr)
    if(dist=="gamma"){
        if("scale" %in% colnames(dat)){
            new_dat <- dat %>%
                mutate(p = pgamma(d+u, shape=shape, scale=scale, lower.tail = FALSE) * phi)
        }
        if("rate" %in% colnames(dat)){
            new_dat <- dat %>%
                mutate(p = pgamma(d+u, shape=shape, rate=rate, lower.tail = FALSE) * phi)
        }
    }
    if(dist=="lnorm"){
        new_dat <- dat %>%
            mutate(p = plnorm(d+u, meanlog=meanlog, sdlog=sdlog, lower.tail = FALSE) * phi)
    }
    if(dist=="weibull"){
        new_dat <- dat %>%
            mutate(p = pweibull(d+u, shape=shape, scale=scale, lower.tail = FALSE) * phi)
    }
    return(new_dat)
}

