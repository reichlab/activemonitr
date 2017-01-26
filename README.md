# Modeling of costs and risks of active monitoring

This repository houses code and data for the manuscript "Reducing the threat of imported infectious disease outbreaks with active monitoring" by Nicholas Reich, Justin Lessler, Jay Varma, and Neil Vora. 

The repository is in the form of an R package, which can be installed using the following commands. You only need to run the first line if you don't already have the `devtools` package installed.
```
install.packages('devtools')
devtools::install_github('reichlab/activemonitr')
```

Code to run the analyses for the manuscript can be found in: 
[activemonitr/inst/analysis-code/](https://github.com/reichlab/activemonitr/tree/master/inst/analysis-code)

The manuscript itself is reproducible and can be found in: 
[activemonitr/inst/manuscript/](https://github.com/reichlab/activemonitr/blob/master/inst/manuscript/active-monitoring.Rmd)

There is [an open-source web app](http://iddynamics.jhsph.edu/apps/shiny/activemonitr/) that accompanies the manuscript. The code for the Shiny app that accompanies the publication can be found in:
[activemonitr/inst/shiny/](https://github.com/reichlab/activemonitr/tree/master/inst/shiny)

Raw data used for the analyses can be found in: 
[activemonitr/inst/raw-data/](https://github.com/reichlab/activemonitr/tree/master/inst/raw-data)
The data used has been aggregated from several publications and a public data repository, including

 - Virlogeux, V., Park, M., Wu, J. T. & Cowling, B. J. Association between Severity of MERS-CoV Infection and Incubation Period. Emerging Infectious Diseases 22, (2016).
 - Virlogeux V, Park M, Wu JTK, Cowling BJ (2015) Data from: Association between severity of MERS-CoV infection and incubation period. Dryad Digital Repository. [http://dx.doi.org/10.5061/dryad.v3546]
 - Nishiura, H. Determination of the appropriate quarantine period following smallpox exposure: an objective approach using the incubation period distribution. Int J Hyg Environ Health 212, 97–104 (2009).
 - Nishiura, H. & Eichner, M. Infectiousness of smallpox relative to disease age: estimates based on transmission network and incubation period. Epidemiol. Infect. 135, 1145–1150 (2007).
 - Faye, O. et al. Chains of transmission and control of Ebola virus disease in Conakry, Guinea, in 2014: an observational study. The Lancet Infectious Diseases 15, 320–326 (2015).

