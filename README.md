# Temporal memory ABM

This repository contains the R code to reproduce the Agent-based Model (ABM) and the analyses that belong to the manuscript "Quantity versus quality: When and where does temporal memory pay off when foraging?" by Kavel C. D. Ozturk 1, Bryndan O. C. M. van Pinxteren, Karline R. L. Janmaat, Benjamin Robira (2026).

The output to reproduce the analysis can be obtained from: ....

Script: This folder contains the scripts to run the ABM and the analyses. The code was written in R. 
* Scenario 1: Scripts belonging to Scenario 1
  *  1_ABM_decayperiod.R : simulations where ripe-fruit duration varied
  *  2_ABM_productivity.R : simulations where resource density varied
  *  3_ABM_heterogeneity.R : simulations where patch spread varied
* Scenario 2: Scripts belonging to Scenario 2 with dynamic trade-off's
  *  5_ABM_Tradeoff_linear.R : simulations with a dynamic linear trade-off 
  *  6_ABM_Tradeoff_exp.R : simulations with a dynamic exponential trade-off 
* Scenario 3: Scripts belonging to Scenario 3 where a  memory-guided forager would not actively forget memorized empty trees that it encountered
  *  7_ABM_nonupdating_decay.R : simulations where ripe-fruit duration varied
  *  8_ABM_nonupdating_prod.R : simulations where resource density varied
  *  9_ABM_nonupdating_hetero.R : simulations where patch spread varied
* Naive forager:
  * 4_ABM_sensory.R: simulations for naive forager accross different environmental parameters 
* Analyses:
  * 10_Extract_Output.R: Script to analyse the ABM output
  * 11_Analysis_UD50.R: Script to analyse the UD50 from the simulated movement
* Functions:
  * Functions.R: Script that contains functions that are being called upon by the ABM

The code has received some cleaning for readability, though the structure reflects my early-stage coding practice and is not yet optimal.





