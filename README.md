# To Make an Adversary Inadmissible 
This project provides exhaustive code for using Vegas's historical performance as a prior, and stacked supervised machine learning models as a likelihood, and providing posterior-predictive distributions for total points scored and difference in home vs. away team points, along with corresponding decision rules for which bets to place given Vegas over-under, spread, and money lines for regular season NBA games.

# Folder Stucture

## MasterScipt.R 
When called, will execute the full pipeline from data processing to model training to making predictions and emailing decision rules. 

## PredictAndLoadNBA_2024.R
This code will automatically pull in data from NBA games and process it, and (optionally) take trained stacked models and posterior inference to make predictions and decision rules for future games. If set up correctly, it will automatically identify the games for the next night, or return two arbitrary games instead for compatibility. 

## PrepDataForNBA.R
This code processes raw data pulled from websites and constructs features from them.

## FitStackedModels.R
This code trains base learners including elastic net GLMs, XGBoost tree ensembles, and multivariate adaptive regression splines to model the 5 response variables:
1) Total points scored per 48 minutes (as Gaussian-distributed)
2) Total points scored per 48 minutes (using a quasi-Poisson model)
3) Any overtime as a Bernoulli event 
4) Spread, home team - away team (as Gaussian distributed)
5) Arcsin-square root transformed proportion of points scored by home team (as Gaussian distributed)

Stacked models take on the form of conditional mixed-effects models with random intercepts for team nested within year, with the out-of-fold cross-validated predictions of each base learner serving as "fixed effects". Stacked models are fit with random effects unconstrained, and fixed effect coefficients (the stacked weights) constrained to be non-negative and sum-to-one.

Posterior draws for variance of model error and inherent variance of the data are obtained using Stan.

Bets are only replaced if the redundant set of models for each response are in agreement.

## PostHocAnalysis

Contains variable importance plots and example output emailed to users for making decisions. The decisions are read off such that, given vegas lines, a certain type of bet (i.e. over, under, or home/away) is recommended for over-unders, spreads, and moneylines.

The history of points bet bets placed as used for visualizing profit over time is also available in "PointsBetCleaned.csv". 

# First Time Users

## Step 1
Download the materials from github and save to a local folder on your computer. Update the directories in "MasterScript.R" accordingly. Make sure all software including MachineShop, cmdstanR, nbastatR, and the R version you are working with are implemented and up to date. Make sure you've set up a Google app password that is compatible with the emayili package, and your password is entered in its designated spot (<>) in MasterScript.R. 

## Step 2
Run MasterScript.R skipping the first call of "PredictAndLoadNBA_2024.R". That is, run all code below "## Construct features from the raw data, prepare in a format suitable for model fitting". This is because PredictAndLoadNBA_2024.R depends on stacked models and posterior draws of inferential parameters to be available before being called.

## Step 3
Once all stacked models and posterior draws are available (after running "FitStackedModels.R") PredictAndLoadNBA_2024.R will be ready to be run. MasterScript.R can be run in full.

# Daily Updates

Use Task Scheduler to call the "TrainNBA.bat" batch script each morning. 

If set up correctly, the code will run daily hands-free and send recommended decision rules to users by email daily. 
