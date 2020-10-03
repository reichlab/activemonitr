#' Posterior distribution of Ebola incubation period
#'
#' A dataset containing samples from an estimated posterior distribution of the
#' incubation period of Ebola. Distribution was estimated from de-identified
#' data from Faye et al. (2015).
#'
#' @format A data frame with 3,000,000 rows and 5 variables:
#' \itemize{
#'   \item \code{shape} shape parameter of gamma distribution
#'   \item \code{scale} scale parameter of gamma distribution
#'   \item \code{idx} index of sample
#'   \item \code{median} the median correponding to this posterior sample
#'   \item \code{p95} the 95th percentile correponding to this posterior sample
#' }
#' @docType data
#' @references Faye, O. et al. Chains of transmission and control of Ebola virus disease in Conakry, Guinea, in 2014: an observational study. The Lancet Infectious Diseases 15, 320–326 (2015).
#' @name pstr_gamma_params_ebola
#' @usage data(pstr_gamma_params_ebola)
NULL


#'
#' Posterior distribution of MERS-CoV incubation period
#'
#' A dataset containing samples from an estimated posterior distribution of the
#' incubation period of MERS-CoV. Distribution was estimated from de-identified
#' data from Virlogeux et al. (2016).
#'
#' @format A data frame with 1,000,000 rows and 6 variables:
#' \itemize{
#'   \item \code{shape} shape parameter of gamma distribution
#'   \item \code{scale} scale parameter of gamma distribution
#'   \item \code{idx} index of sample
#'   \item \code{median} the median correponding to this posterior sample
#'   \item \code{p95} the 95th percentile correponding to this posterior sample
#'   \item \code{chain} which chain the sample belongs to
#'   }
#' @docType data
#' @references Virlogeux, V., Park, M., Wu, J. T. & Cowling, B. J. Association between Severity of MERS-CoV Infection and Incubation Period. Emerging Infectious Diseases 22, (2016).
#' @name pstr_gamma_params_mers
#' @usage data(pstr_gamma_params_mers)
NULL


#'
#' Posterior distribution of smallpox incubation period
#'
#' A dataset containing samples from an estimated posterior distribution of the
#' incubation period of smallpox. Distribution was estimated from de-identified
#' data from several sources (see References).
#'
#' @format A data frame with 1,000,000 rows and 6 variables:
#' \itemize{
#'   \item \code{shape} shape parameter of gamma distribution
#'   \item \code{scale} scale parameter of gamma distribution
#'   \item \code{idx} index of sample
#'   \item \code{median} the median correponding to this posterior sample
#'   \item \code{p95} the 95th percentile correponding to this posterior sample
#'   \item \code{chain} which chain the sample belongs to
#'   }
#' @docType data
#' @references
#' \itemize{
#'   \item Litvinjenko, S., Arsic, B. & Borjanovic, S. Epidemiologic Aspects of Smallpox in Yugoslavia in 1972. Bulletin of the World Health Organization (1972).
#'   \item Mack, T. M. Smallpox in Europe, 1950-1971. Journal of Infectious Diseases 125, 161–169 (1972).
#'   \item Nishiura, H. Determination of the appropriate quarantine period following smallpox exposure: an objective approach using the incubation period distribution. Int J Hyg Environ Health 212, 97–104 (2009).
#' }
#' @name pstr_gamma_params_smallpox
#' @usage data(pstr_gamma_params_smallpox)
NULL


#'
#' Bootstrapped parameter estimates of COVID-19 incubation period
#'
#' A dataset containing samples from an estimated bootstrap distribution of the
#' incubation period of COVID-19 was estimated from data
#' collected and stored at https://github.com/HopkinsIDD/ncov_incubation.
#'
#' @format A data frame with 1,000 rows and 5 variables:
#' \itemize{
#'   \item \code{meanlog} meanlog parameter of log-normal distribution
#'   \item \code{sdlog} sdlog parameter of log-normal distribution
#'   \item \code{idx} index of sample
#'   \item \code{median} the median correponding to this bootstrapped sample
#'   \item \code{p95} the 95th percentile correponding to this bootstrapped sample
#'   }
#' @docType data
#' @references
#' \itemize{
#'   \item Lauer, S. A., Grantz, K. H., Bi, Q., Jones, F. K., Zheng, Q., Meredith, H. R., Azman, A. S., Reich, N. G., & Lessler, J. The incubation period of coronavirus disease 2019 (COVID-19) from publicly reported confirmed cases: estimation and application. Annals of Internal Medicine 172 (9), 577-582 (2020).
#' }
#' @name boot_lnorm_params_covid
#' @usage data(boot_lnorm_params_covid)
NULL

#'
#' Alternative bootstrapped parameter estimates of COVID-19 incubation period from Zhao et al.
#'
#' @format A data frame with 1,000 rows and 6 variables:
#' \itemize{
#'   \item \code{shape} shape parameter of gamma distribution
#'   \item \code{rate} rate parameter of gamma distribution
#'   \item \code{idx} index of sample
#'   \item \code{median} the median correponding to this bootstrapped sample
#'   \item \code{p95} the 95th percentile correponding to this bootstrapped sample
#'   }
#' @docType data
#' @references
#' \itemize{
#'   \item Zhao, Q., Ju N., Bacallado, S., & Shah, R. D. BETS: The dangers of selection bias in early analyses of the coronavirus disease (COVID-19) pandemic. arXiv (2020).
#' }
#' @name bets_results_bootstrap
#' @usage data(bets_results_bootstrap)
NULL


#'
#' Kernel Density Estimates for posterior distributions
#'
#' @format Output from fit_kde() function in inst/analysis-code/inc-per-mcmc.R. Data used in plotting credible regions of posterior estimates.
#' @docType data
#' @name kde_smallpox
#' @name kde_ebola
#' @name kde_mers
#' @name kde_covid
#' @name kde_covid_bets
#' @usage data(kde_ebola)
NULL
