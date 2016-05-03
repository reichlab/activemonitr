# Analysis workflow

1. `test-analysis.R` script was used to calibrate MCMC parameters and chain lengths.
2. Each `fit-incper-distr-[disease].R` file was run.
3. `check-analysis.R` run to evaluate fits, check plots.
4. Final `.rda` files with posterior distributions were moved to the `data` directory for direct loading by the package.
5. Each paper file was generated dynamically using the datasets within the package.