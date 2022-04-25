# indtermdigit
 
This package replicates the results for the paper "Tests for the independence of terminal digits to detect duplicated data." The scripts to run are stored in the folder "paper."

The file "simulations.Rmd" conducts all reported simulations for type I errors and power analysis. This may take more than 24 hours to run. This script depends on the C++ functions that are part of this package so these have to be loaded first.

The file "results_graphs.R" uses the RDS files saved by "power_simulations" to produce tables and figures.

The file "dhs" conducts the reported analysis for the DHS surveys in Nigeria (2013, 2018). I don't have permission to share this data but it can be required directly from DHS: https://dhsprogram.com/data/Using-Datasets-for-Analysis.cfm To run this script, you'll first have to acquire the relevant data sets. 
