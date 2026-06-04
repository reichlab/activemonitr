#######################################################################
## Deploy the active-monitoring Shiny app to shinyapps.io
##
## Prerequisites (run once, interactively, on your own machine):
##   install.packages(c("rsconnect", "remotes"))
##
##   # 1. The app depends on the activeMonitr package, which is not on CRAN.
##   #    Install it from GitHub (or locally) so rsconnect can snapshot it:
##   remotes::install_github("reichlab/activemonitr")
##   #    ...or, from the package root during development:
##   # remotes::install_local(".", force = TRUE)
##
##   # 2. Authenticate with shinyapps.io. Copy your token + secret from
##   #    https://www.shinyapps.io/admin/#/tokens (Account > Tokens) and run:
##   rsconnect::setAccountInfo(name   = "<your-account>",
##                             token  = "<token>",
##                             secret = "<secret>")
##
## Then deploy by running this script from the package root:
##   Rscript inst/shiny/deploy.R
#######################################################################
library(rsconnect)

app_dir <- "inst/shiny/active-monitoring"

stopifnot(dir.exists(app_dir))

## Sanity check: the app's runtime dependencies must be installed locally so
## rsconnect can detect and bundle them (it scans the library() calls in
## global.R and recursively their dependencies, including activeMonitr).
needed <- c("shiny", "dplyr", "ggplot2", "scales", "DT", "activeMonitr")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
    stop("Install these packages before deploying: ",
         paste(missing, collapse = ", "),
         "\n(activeMonitr: remotes::install_github('reichlab/activemonitr'))")
}

rsconnect::deployApp(
    appDir   = app_dir,
    appName  = "active-monitoring",
    appTitle = "Determining Durations for Active Monitoring",
    forceUpdate = TRUE
)

## After a successful deploy, rsconnect prints the live URL, e.g.
##   https://<your-account>.shinyapps.io/active-monitoring/
## Add that URL to README.md.
