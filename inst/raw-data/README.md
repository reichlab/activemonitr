# Data on incubation periods

This file describes the incubation period data on smallpox, Ebola, and MERS-CoV used for the analysis of the manuscript "Reducing the threat of imported infectious disease outbreaks with active monitoring".

### MERS-CoV

[data_mers.csv](https://raw.githubusercontent.com/reichlab/activemonitr/master/inst/raw-data/data_mers.csv) is a data file containing information on 170 human cases of laboratory-confirmed MERS-CoV infection in South Korea in 2015.
Definition of column headings:

 - Case No.: case identifier number.
 - IncP_min/IncP_Max: the lower & upper bound of the incubation, i.e., delay (in days) from exposure to symptom onset.
 - Age: age of each case, in years.
 - Sex_status: 0-female; 1-male.
 - Missing_exposure: 0-exposure dates given; 1-exposure dates missing.
 - Death_status: 0-alive; 1-dead.
 - Onset_confirm: delay (in days) from sypmtom onset to laboratory confirmed infection.

This data is taken from the following publication: Virlogeux, V., Park, M., Wu, J. T. & Cowling, B. J. Association between Severity of MERS-CoV Infection and Incubation Period. Emerging Infectious Diseases 22, (2016). And is publicly available at [http://datadryad.org/resource/doi:10.5061/dryad.v3546](http://datadryad.org/resource/doi:10.5061/dryad.v3546).

### Smallpox

[smallpox-data.csv](https://raw.githubusercontent.com/reichlab/activemonitr/master/inst/raw-data/smallpox-data.csv) is a data file containing records on the incubation periods of 362 cases of smallpox. 

Definition of column headings:

 - source: source the data was transcribed from
 - outbreak: description of the outbreak that gave rise to the the data
 - original_source: original source of the data
 - reported_incper: the integer value of the reported incubation period

Data from each source are briefly described:
 - table transcribed from Nishiura (2009): 131 cases from 6 original sources [NB: Downie citation is given 84 in new data provided by Nishiura, but only 41 in the 2009 citation, due to according to footnote, “Cases where transmission was suspected to have occurred through objects (e.g. from contaminated clothes in a laundry) were excluded.”]
 - Litvinjenko (1972): 171 cases 
 - Mack (1972): 60 cases (only cases with one-day exposure window included)

### Ebola

[DURATION.INCUB.txt](https://raw.githubusercontent.com/reichlab/activemonitr/master/inst/raw-data/DURATION.INCUB.txt) is a datafile containing posterior draws from a model that estimated the incubation period for a population of 152 individuals in Guinea in 2015. The data was provided by the authors and is republished with permission. Each row of the dataset correponds to an individual. Each column corresponds to a single poseterior sample from the modeled contact network. There are 300 posterior draws in total. Source: Faye, O. et al. Chains of transmission and control of Ebola virus disease in Conakry, Guinea, in 2014: an observational study. The Lancet Infectious Diseases 15, 320–326 (2015).
