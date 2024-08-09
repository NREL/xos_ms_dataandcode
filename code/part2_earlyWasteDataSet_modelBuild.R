# Purpose: build baseline pls-2 model to predict XOS, Solids, and free xylose from Samples from early/waste locations in the process stream using 2100-2450 spectral range

# load packages--------------------

library(tidyverse)
library(pls)
library(prospectr)

# source preprocessing functions--------------------

source("./commoncode/preprocess_spectra.R")


# load data into training and test sets based on previous pca selection--------------------

training <- readRDS("../data/processed/earlyWaste_training.RDS")

testing <- readRDS("../data/processed/earlyWaste_testing.RDS")

# run data training and testing data through overtone specific spectral processing function--------------------

training$spec2 <- preprocess_spectra_overtone(training)
testing$spec2 <- preprocess_spectra_overtone(testing)

# build models- fit a pls2 model to the calibration data--------------------

model <- mvr(WC~ spec2,	
                  data=training,
                  method="oscorespls", 
                  x=TRUE, y=TRUE, 
                  ncomp=20,
                  validation="LOO",
                  center=TRUE, 
                  jackknife = TRUE)

# visually identify number of pcs to use for each outcome to avoid overfitting/tune models

validationplot(model)

# set PCs (tune the models) based on visual inspection of validation plots--------------------

# PCS for monomeric Xylose
xfPC <- 8

# PCs for XOS

xosPC <-11

# PCs for Total Solids

sPC <- 11

# calculate calibration, cross validation, and independent validation predictions; save as columns--------------------

# prediction of calibration and cross validation, saving results into data frame for figure/table building

training <- training %>% 
  mutate(xf_cal = predict(model, training)[,1,xfPC],
         xos_cal = predict(model, training)[,2,xosPC],
         solids_cal = predict(model, training)[,3,sPC],
         xf_cv = model$validation$pred[,1,xfPC],
         xos_cv = model$validation$pred[,2,xosPC],
         solids_cv = model$validation$pred[,3,sPC]
         )

# prediction of independent validation samples, saving results into data frame for figure/table building

testing <- testing %>% 
  mutate(xf_val = predict(model, testing )[,1,xfPC],
         xos_val = predict(model, testing )[,2,xosPC],
         solids_val = predict(model, testing )[,3,sPC])

# save all results as RDS files for figure/table building--------------------

write_rds(training, "../data/processed/part2_earlyWasteDataSet_training.RDS")
write_rds(testing, "../data/processed/part2_earlyWasteDataSet_testing.RDS")
