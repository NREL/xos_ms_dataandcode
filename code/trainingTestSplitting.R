# Purpose: Split samples into training and test sets

# load packages--------------------

library(tidyverse)
library(pls)
library(prospectr)
library(openxlsx)

# source preprocessing functions--------------------

source("./commoncode/preprocess_spectra.R")


# load raw data-------------------- 

data <- read.xlsx("../data/raw/sampleData.xlsx")


# run all data through full spectral processing function, returning processed spectral signal as matrix called 'spec1`--------------------

data$spec1 <- data %>% preprocess_spectra()

# format outcomes of interest for modeling as a matrix object that can be read into PLS model--------------------

data$WC <- data.frame(xylose_free = data$xylose_free, xos = data$xos, solids = data$solids) %>% as.matrix()

# split into early/waste and late groups based on `grouping` variable--------------------
earlyWaste <- data %>% 
  filter(grouping == "early/waste")

late <- data %>% 
  filter(grouping == "late")

# run a PCA on both early and late data groups based on processed NIR full spectra--------------------

pca_ew <- prcomp(earlyWaste$spec1, center = TRUE )
pca_l <- prcomp(late$spec1, center = TRUE)

# use Kennard-Stone on PCA scores to select ~30% of samples to retain as testing set from each group based on PCs that describe 95% of the variance in the data--------------------

ks_ew <- kenStone(pca_ew$x, k = .7*dim(earlyWaste)[1], pc = .95)
ks_l <- kenStone(pca_l$x, k= .7*dim(late)[1], pc = .95) 

# subset early/waste samples into training and testing groups based on Kennard-Stone selection--------------------

ew_train <- earlyWaste[ks_ew$model,]
ew_test <- earlyWaste[ks_ew$test,]

# subset late samples into training and testing group sbased on Kennard-Stone selection--------------------

l_train <- late[ks_l$model,]
l_test <- late[ks_l$test,]

# save all datasets produced as RDS objects that can be used for modeling--------------------

saveRDS(ew_train, "../data/processed/earlyWaste_training.RDS")
saveRDS(ew_test, "../data/processed/earlyWaste_testing.RDS")
saveRDS(l_train, "../data/processed/late_training.RDS")
saveRDS(l_test, "../data/processed/late_testing.RDS")

