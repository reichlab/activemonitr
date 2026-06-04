#######################################################################
## Make small, app-only thinned views of the large posterior datasets
##
## The canonical posterior objects (pstr_gamma_params_{ebola,mers,smallpox})
## contain 1-3 million MCMC draws and total ~80 MB. The Shiny app only ever
## draws nsamp=1000 samples and otherwise uses column means/medians, so it has
## no need for the full chains. This script creates ~5,000-row thinned views
## (pstr_gamma_params_*_small) that are loaded by the app instead, keeping the
## running app's memory footprint tiny while leaving the canonical objects
## (and full reproducibility) untouched.
##
## boot_lnorm_params_covid (1,000 rows) and the kde_* objects are already
## small and are used by the app directly -- no _small view is needed.
##
## Re-run from the package root with: Rscript inst/analysis-code/make-app-data.R
#######################################################################
suppressMessages(library(dplyr))

set.seed(20200205)          # fixed seed -> reproducible thinned views
n_keep <- 5000              # >= the app's max nsamp (1000), plenty for means/medians

thin_one <- function(name) {
    load(file.path("data", paste0(name, ".rda")))
    full <- get(name)
    small <- dplyr::slice_sample(full, n = n_keep)
    small_name <- paste0(name, "_small")
    assign(small_name, small)

    ## sanity check: thinned column means/medians track the full object closely
    num_cols <- names(full)[vapply(full, is.numeric, logical(1))]
    for (col in num_cols) {
        fm <- mean(full[[col]]); sm <- mean(small[[col]])
        fmed <- median(full[[col]]); smed <- median(small[[col]])
        cat(sprintf("  %-8s %-7s mean %.5g vs %.5g (%.2f%%)  median %.5g vs %.5g (%.2f%%)\n",
                    name, col, fm, sm, 100 * abs(sm - fm) / abs(fm),
                    fmed, smed, 100 * abs(smed - fmed) / abs(fmed)))
    }

    save(list = small_name,
         file = file.path("data", paste0(small_name, ".rda")),
         compress = "xz")
    sz <- file.info(file.path("data", paste0(small_name, ".rda")))$size
    cat(sprintf("  -> wrote data/%s.rda (%d rows, %.1f KB)\n\n",
                small_name, nrow(small), sz / 1024))
}

for (nm in c("pstr_gamma_params_ebola",
             "pstr_gamma_params_mers",
             "pstr_gamma_params_smallpox")) {
    thin_one(nm)
}

cat("Done. Small app-data objects regenerated.\n")
