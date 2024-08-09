# Code to produce supplementary item 2: summarizing model performance for free xylose prediction across the all model populations

#load packages
library(tidyverse)
library(pls)
 

#load data
part1_fullDataSet_testing <- readRDS("../data/processed/part1_fulldataSet_testing.RDS")
part1_fullDataSet_training<- readRDS("../data/processed/part1_fulldataSet_training.RDS")
part1_earlyWasteDataSet_testing <- readRDS("../data/processed/part1_earlyWasteDataSet_testing.RDS")
part1_earlyWasteDataSet_training<- readRDS("../data/processed/part1_earlyWasteDataSet_training.RDS")
part1_lateDataSet_testing <- readRDS("../data/processed/part1_lateDataSet_testing.RDS")
part1_lateDataSet_training<- readRDS("../data/processed/part1_lateDataSet_training.RDS")
part2_earlyWasteDataSet_testing <- readRDS("../data/processed/part2_earlyWasteDataSet_testing.RDS")
part2_earlyWasteDataSet_training<- readRDS("../data/processed/part2_earlyWasteDataSet_training.RDS")
part2_lateDataSet_testing <- readRDS("../data/processed/part2_lateDataSet_testing.RDS")
part2_lateDataSet_training<- readRDS("../data/processed/part2_lateDataSet_training.RDS")

#need to split the results of the combined model by sampling location type
#set 1 calculations by early and late

#training and cross val
part1_fullDataSet_earlyWasteSubset_training <- 
  part1_fullDataSet_training %>% 
  filter(grouping == "early/waste")

part1_fullDataSet_lateSubset_training <- 
  part1_fullDataSet_training %>% 
  filter(grouping == "late")

#test 
part1_fullDataSet_earlyWasteSubset_testing <- 
  part1_fullDataSet_testing %>% 
  filter(grouping == "early/waste")

part1_fullDataSet_lateSubset_testing <- 
  part1_fullDataSet_testing %>% 
  filter(grouping == "late")

## xylose_free Performance 
# set 1  no splitting model results


# early/waste subset
r1	<- round(caret::R2(pred =part1_fullDataSet_earlyWasteSubset_training$xf_cal, obs = part1_fullDataSet_earlyWasteSubset_training$xylose_free),2)
r2	<- round(caret::RMSE(pred = part1_fullDataSet_earlyWasteSubset_training$xf_cal, obs = part1_fullDataSet_earlyWasteSubset_training$xylose_free),2)
r3	<- round(caret::R2(pred = part1_fullDataSet_earlyWasteSubset_training$xf_cv, obs = part1_fullDataSet_earlyWasteSubset_training$xylose_free),2)
r4	<- round(caret::RMSE(pred = part1_fullDataSet_earlyWasteSubset_training$xf_cv, obs = part1_fullDataSet_earlyWasteSubset_training$xylose_free),2)
r5 <- round(caret::R2(pred = part1_fullDataSet_earlyWasteSubset_testing$xf_val, obs =part1_fullDataSet_earlyWasteSubset_testing$xylose_free),2)
r6 <- round(caret::RMSE(pred = part1_fullDataSet_earlyWasteSubset_testing$xf_val, obs= part1_fullDataSet_earlyWasteSubset_testing$xylose_free),2)


n1 <- length(part1_fullDataSet_earlyWasteSubset_training$xylose_free)
n2 <- length(part1_fullDataSet_earlyWasteSubset_training$xylose_free)
n3 <- length(part1_fullDataSet_earlyWasteSubset_training$xf_cv)
n4 <- length(part1_fullDataSet_earlyWasteSubset_training$xf_cv)
n5 <- length(part1_fullDataSet_earlyWasteSubset_testing$xylose_free)
n6 <- length(part1_fullDataSet_earlyWasteSubset_testing$xylose_free)


c<- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "xylose_free (g/L)") %>% 
  mutate(modelname = "no location splitting") %>% 
  mutate(samplinglocation = "early/waste") %>% 
  select(!test)

c$n <- c(n1,n2,n3,n4,n5,n6)

# late subset 

r1	<- round(caret::R2(pred =part1_fullDataSet_lateSubset_training$xf_cal, obs = part1_fullDataSet_lateSubset_training$xylose_free),2)
r2	<- round(caret::RMSE(pred = part1_fullDataSet_lateSubset_training$xf_cal, obs = part1_fullDataSet_lateSubset_training$xylose_free),2)
r3	<- round(caret::R2(pred = part1_fullDataSet_lateSubset_training$xf_cv, obs = part1_fullDataSet_lateSubset_training$xylose_free),2)
r4	<- round(caret::RMSE(pred = part1_fullDataSet_lateSubset_training$xf_cv, obs = part1_fullDataSet_lateSubset_training$xylose_free),2)
r5 <- round(caret::R2(pred = part1_fullDataSet_lateSubset_testing$xf_val, obs =part1_fullDataSet_lateSubset_testing$xylose_free),2)
r6 <- round(caret::RMSE(pred = part1_fullDataSet_lateSubset_testing$xf_val, obs= part1_fullDataSet_lateSubset_testing$xylose_free),2)


n1 <- length(part1_fullDataSet_lateSubset_training$xylose_free)
n2 <- length(part1_fullDataSet_lateSubset_training$xylose_free)
n3 <- length(part1_fullDataSet_lateSubset_training$xf_cv)
n4 <- length(part1_fullDataSet_lateSubset_training$xf_cv)
n5 <- length(part1_fullDataSet_lateSubset_testing$xylose_free)
n6 <- length(part1_fullDataSet_lateSubset_testing$xylose_free)

d<- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "xylose_free (g/L)") %>% 
  mutate(modelname = "no location splitting") %>% 
  mutate(samplinglocation = "late") %>% 
  select(!test)
d$n <- c(n1,n2,n3,n4,n5,n6)

# sample location split model performance

# early/waste subset
r1	<- round(caret::R2(pred =part1_earlyWasteDataSet_training$xf_cal, obs = part1_earlyWasteDataSet_training$xylose_free),2)
r2	<- round(caret::RMSE(pred = part1_earlyWasteDataSet_training$xf_cal, obs = part1_earlyWasteDataSet_training$xylose_free),2)
r3	<- round(caret::R2(pred = part1_earlyWasteDataSet_training$xf_cv, obs = part1_earlyWasteDataSet_training$xylose_free),2)
r4	<- round(caret::RMSE(pred = part1_earlyWasteDataSet_training$xf_cv, obs = part1_earlyWasteDataSet_training$xylose_free),2)
r5 <- round(caret::R2(pred = part1_earlyWasteDataSet_testing$xf_val, obs =part1_earlyWasteDataSet_testing$xylose_free),2)
r6 <- round(caret::RMSE(pred =part1_earlyWasteDataSet_testing$xf_val, obs= part1_earlyWasteDataSet_testing$xylose_free),2)


n1 <- length(part1_earlyWasteDataSet_training$xylose_free)
n2 <- length(part1_earlyWasteDataSet_training$xylose_free)
n3 <- length(part1_earlyWasteDataSet_training$xf_cv)
n4 <- length(part1_earlyWasteDataSet_training$xf_cv)
n5 <- length(part1_earlyWasteDataSet_testing$xylose_free)
n6 <- length(part1_earlyWasteDataSet_testing$xylose_free)


e<- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "xylose_free (g/L)") %>% 
  mutate(modelname = "location splitting") %>% 
  mutate(samplinglocation = "early/waste") %>% 
  select(!test)

e$n <- c(n1,n2,n3,n4,n5,n6)

# early/waste subset
r1	<- round(caret::R2(pred =part1_lateDataSet_training$xf_cal, obs = part1_lateDataSet_training$xylose_free),2)
r2	<- round(caret::RMSE(pred = part1_lateDataSet_training$xf_cal, obs = part1_lateDataSet_training$xylose_free),2)
r3	<- round(caret::R2(pred = part1_lateDataSet_training$xf_cv, obs = part1_lateDataSet_training$xylose_free),2)
r4	<- round(caret::RMSE(pred = part1_lateDataSet_training$xf_cv, obs = part1_lateDataSet_training$xylose_free),2)
r5 <- round(caret::R2(pred = part1_lateDataSet_testing$xf_val, obs =part1_lateDataSet_testing$xylose_free),2)
r6 <- round(caret::RMSE(pred =part1_lateDataSet_testing$xf_val, obs= part1_lateDataSet_testing$xylose_free),2)


n1 <- length(part1_lateDataSet_training$xylose_free)
n2 <- length(part1_lateDataSet_training$xylose_free)
n3 <- length(part1_lateDataSet_training$xf_cv)
n4 <- length(part1_lateDataSet_training$xf_cv)
n5 <- length(part1_lateDataSet_testing$xylose_free)
n6 <- length(part1_lateDataSet_testing$xylose_free)


f<- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "xylose_free (g/L)") %>% 
  mutate(modelname = "location splitting") %>% 
  mutate(samplinglocation = "late") %>% 
  select(!test)

f$n <- c(n1,n2,n3,n4,n5,n6)

# Performance Results for 2100-2450nm Spectra at both sampling locations

#early/waste
r1	<- round(caret::R2(pred =part2_earlyWasteDataSet_training$xf_cal, obs = part2_earlyWasteDataSet_training$xylose_free),2)
r2	<- round(caret::RMSE(pred = part2_earlyWasteDataSet_training$xf_cal, obs = part2_earlyWasteDataSet_training$xylose_free),2)
r3	<- round(caret::R2(pred = part2_earlyWasteDataSet_training$xf_cv, obs = part2_earlyWasteDataSet_training$xylose_free),2)
r4	<- round(caret::RMSE(pred = part2_earlyWasteDataSet_training$xf_cv, obs = part2_earlyWasteDataSet_training$xylose_free),2)
r5 <- round(caret::R2(pred = part2_earlyWasteDataSet_testing$xf_val, obs =part2_earlyWasteDataSet_testing$xylose_free),2)
r6 <- round(caret::RMSE(pred =part2_earlyWasteDataSet_testing$xf_val, obs= part2_earlyWasteDataSet_testing$xylose_free),2)


n1 <- length(part2_earlyWasteDataSet_training$xylose_free)
n2 <- length(part2_earlyWasteDataSet_training$xylose_free)
n3 <- length(part2_earlyWasteDataSet_training$xf_cv)
n4 <- length(part2_earlyWasteDataSet_training$xf_cv)
n5 <- length(part2_earlyWasteDataSet_testing$xylose_free)
n6 <- length(part2_earlyWasteDataSet_testing$xylose_free)


g<- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "xylose_free (g/L)") %>% 
  mutate(modelname = "2100-2450nm spectra, location splitting") %>% 
  mutate(samplinglocation = "early/waste") %>% 
  select(!test)

g$n <- c(n1,n2,n3,n4,n5,n6)

#late

r1	<- round(caret::R2(pred =part2_lateDataSet_training$xf_cal, obs = part2_lateDataSet_training$xylose_free),2)
r2	<- round(caret::RMSE(pred = part2_lateDataSet_training$xf_cal, obs = part2_lateDataSet_training$xylose_free),2)
r3	<- round(caret::R2(pred = part2_lateDataSet_training$xf_cv, obs = part2_lateDataSet_training$xylose_free),2)
r4	<- round(caret::RMSE(pred = part2_lateDataSet_training$xf_cv, obs = part2_lateDataSet_training$xylose_free),2)
r5 <- round(caret::R2(pred = part2_lateDataSet_testing$xf_val, obs =part2_lateDataSet_testing$xylose_free),2)
r6 <- round(caret::RMSE(pred =part2_lateDataSet_testing$xf_val, obs= part2_lateDataSet_testing$xylose_free),2)


n1 <- length(part2_lateDataSet_training$xylose_free)
n2 <- length(part2_lateDataSet_training$xylose_free)
n3 <- length(part2_lateDataSet_training$xf_cv)
n4 <- length(part2_lateDataSet_training$xf_cv)
n5 <- length(part2_lateDataSet_testing$xylose_free)
n6 <- length(part2_lateDataSet_testing$xylose_free)


h<- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "xylose_free (g/L)") %>% 
  mutate(modelname = "2100-2450nm  spectra, location splitting") %>% 
  mutate(samplinglocation = "late") %>% 
  select(!test)

h$n <- c(n1,n2,n3,n4,n5,n6)


rbind(c,d,e,f,g,h) %>% 
  write.csv("../results/tables/S2.csv")
