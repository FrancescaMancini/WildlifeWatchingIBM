# WildlifeWatchingIBM

The scripts in this repository contain the code to reproduce the individual-based model in the manuscript "Tourists’ specialisation is crucial to the sustainability of wildlife tourism destinations and adaptive governance is necessary to avoid failure of the wildlife tourism commons".

The files IBMfunctions.R and IBMfunctionsS3.R contain the functions used in the model for scenarios 1 and 2 and for scenarios 3 and 4 respectively. The files IBMScenario*.R contain the code to run the simulations.

The model was implemented in R version 3.5.0 (R Core Team, 2018) using packages dplyr version 0.7.4 (Wickham, Francois, Henry, & Müller, 2017), RGeode version 0.1.0 (Rimella, 2017). Simulation runs were distributed for parallel computing using doParallel version 1.0.11 (Microsoft Corporation & Weston, 2017a) and foreach version 1.4.4 (Microsoft Corporation & Weston, 2017b) on a cluster with Bio-Linux OS (Kernal version (uname -r) = "3.13.0-128-generic" VERSION="14.04.5 LTS, Trusty Tahr"). 

Finally, the file Visualisations.R contains the code to manipulate the data and produce the figures in the manuscript.
