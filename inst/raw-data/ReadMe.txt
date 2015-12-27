Data and R syntax for the following publication:
Victor Virlogeux, Minah Park, Joseph T. Wu, Benjamin J. Cowling. Association between severity of MERS-CoV infection and incubation period. EID 2015.


+-----------------+
|  List of files  |
+-----------------+

1. [data_mers.csv] This data file contains information on 170 human cases of laboratory-confirmed MERS-CoV infection in South Korea in 2015.
Definition of column headings >>>
Case No.: case identifier number.
IncP_min/IncP_Max: the lower & upper bound of the incubation, i.e., delay (in days) from exposure to symptom onset.
Age: age of each case, in years.
Sex_status: 0-female; 1-male.
Missing_exposure: 0-exposure dates given; 1-exposure dates missing.
Death_status: 0-alive; 1-dead.
Onset_confirm: delay (in days) from sypmtom onset to laboratory confirmed infection.

2. [mcmc_function_incubation.r] R syntax to define the MCMC functions to be used in other scripts.

4. [Table_1.r] R syntax to reproduce results in Supplementary Table 1.

5. [Table_2.1.r] R syntax to reproduce results in Supplementary Table 2 approach 1.

6. [Table_2.2.r] R syntax to reproduce results in Supplementary Table 2 approach 2 & 2*.

7. [Table_3.r] R syntax to reproduce results in Supplementary Table 3.

8. [Figure_1.r] R syntax to reproduce Figure 1.

Note: The numerical results reported in the main text of the paper are extracted from the respective tables, apart from the reported mean difference in incubation periods between fatal and non-fatal cases. The syntax to generate the mean difference (as well as 95% CI) in incubation periods between fatal and non-fatal cases is provided at the end of the script [Table_1.r].


 











