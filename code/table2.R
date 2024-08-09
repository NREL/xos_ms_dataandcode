# Code to generate table 2  (comparing model performance for sample location splitting vs no sample location splitting)


#load packages

library(tidyverse)
library(pls)
library(prospectr)
library(ggfortify)
library(patchwork)
library(ggpmisc) 


#load data

part1_fullDataSet_testing <- readRDS("../data/processed/part1_fulldataSet_testing.RDS")
part1_fullDataSet_training <- readRDS("../data/processed/part1_fulldataSet_training.RDS")
part1_earlyWasteDataSet_testing <- readRDS("../data/processed/part1_earlyWasteDataSet_testing.RDS")
part1_earlyWasteDataSet_training <- readRDS("../data/processed/part1_earlyWasteDataSet_training.RDS")
part1_lateDataSet_testing <- readRDS("../data/processed/part1_lateDataSet_testing.RDS")
part1_lateDataSet_training <- readRDS("../data/processed/part1_lateDataSet_training.RDS")



#first,  need to split the results of the combined model by sampling location type
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

## XOS Performance 
# set 1  no splitting model results

# early/waste subset

r1	<- round(caret::R2(pred =part1_fullDataSet_earlyWasteSubset_training$xos_cal, obs = part1_fullDataSet_earlyWasteSubset_training$xos),2)
r2	<- round(caret::RMSE(pred = part1_fullDataSet_earlyWasteSubset_training$xos_cal, obs = part1_fullDataSet_earlyWasteSubset_training$xos),2)
r3	<- round(caret::R2(pred = part1_fullDataSet_earlyWasteSubset_training$xos_cv, obs = part1_fullDataSet_earlyWasteSubset_training$xos),2)
r4	<- round(caret::RMSE(pred = part1_fullDataSet_earlyWasteSubset_training$xos_cv, obs = part1_fullDataSet_earlyWasteSubset_training$xos),2)
r5 <- round(caret::R2(pred = part1_fullDataSet_earlyWasteSubset_testing$xos_val, obs =part1_fullDataSet_earlyWasteSubset_testing$xos),2)
r6 <- round(caret::RMSE(pred = part1_fullDataSet_earlyWasteSubset_testing$xos_val, obs= part1_fullDataSet_earlyWasteSubset_testing$xos),2)


n1 <- length(part1_fullDataSet_earlyWasteSubset_training$xos)
n2 <- length(part1_fullDataSet_earlyWasteSubset_training$xos)
n3 <- length(part1_fullDataSet_earlyWasteSubset_training$xos_cv)
n4 <- length(part1_fullDataSet_earlyWasteSubset_training$xos_cv)
n5 <- length(part1_fullDataSet_earlyWasteSubset_testing$xos)
n6 <- length(part1_fullDataSet_earlyWasteSubset_testing$xos)


c <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "XOS (g/L)") %>% 
  mutate(modelname = "no location splitting") %>% 
  mutate(samplinglocation = "early/waste") %>% 
  select(!test)

c$n <- c(n1,n2,n3,n4,n5,n6)

# late subset 

r1	<- round(caret::R2(pred =part1_fullDataSet_lateSubset_training$xos_cal, obs = part1_fullDataSet_lateSubset_training$xos),2)
r2	<- round(caret::RMSE(pred = part1_fullDataSet_lateSubset_training$xos_cal, obs = part1_fullDataSet_lateSubset_training$xos),2)
r3	<- round(caret::R2(pred = part1_fullDataSet_lateSubset_training$xos_cv, obs = part1_fullDataSet_lateSubset_training$xos),2)
r4	<- round(caret::RMSE(pred = part1_fullDataSet_lateSubset_training$xos_cv, obs = part1_fullDataSet_lateSubset_training$xos),2)
r5 <- round(caret::R2(pred = part1_fullDataSet_lateSubset_testing$xos_val, obs =part1_fullDataSet_lateSubset_testing$xos),2)
r6 <- round(caret::RMSE(pred = part1_fullDataSet_lateSubset_testing$xos_val, obs= part1_fullDataSet_lateSubset_testing$xos),2)


n1 <- length(part1_fullDataSet_lateSubset_training$xos)
n2 <- length(part1_fullDataSet_lateSubset_training$xos)
n3 <- length(part1_fullDataSet_lateSubset_training$xos_cv)
n4 <- length(part1_fullDataSet_lateSubset_training$xos_cv)
n5 <- length(part1_fullDataSet_lateSubset_testing$xos)
n6 <- length(part1_fullDataSet_lateSubset_testing$xos)

d <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "XOS (g/L)") %>% 
  mutate(modelname = "no location splitting") %>% 
  mutate(samplinglocation = "late") %>% 
  select(!test)
d$n <- c(n1,n2,n3,n4,n5,n6)

# sample location split model performance

# early/waste subset
r1	<- round(caret::R2(pred =part1_earlyWasteDataSet_training$xos_cal, obs = part1_earlyWasteDataSet_training$xos),2)
r2	<- round(caret::RMSE(pred = part1_earlyWasteDataSet_training$xos_cal, obs = part1_earlyWasteDataSet_training$xos),2)
r3	<- round(caret::R2(pred = part1_earlyWasteDataSet_training$xos_cv, obs = part1_earlyWasteDataSet_training$xos),2)
r4	<- round(caret::RMSE(pred = part1_earlyWasteDataSet_training$xos_cv, obs = part1_earlyWasteDataSet_training$xos),2)
r5 <- round(caret::R2(pred = part1_earlyWasteDataSet_testing$xos_val, obs =part1_earlyWasteDataSet_testing$xos),2)
r6 <- round(caret::RMSE(pred =part1_earlyWasteDataSet_testing$xos_val, obs= part1_earlyWasteDataSet_testing$xos),2)


n1 <- length(part1_earlyWasteDataSet_training$xos)
n2 <- length(part1_earlyWasteDataSet_training$xos)
n3 <- length(part1_earlyWasteDataSet_training$xos_cv)
n4 <- length(part1_earlyWasteDataSet_training$xos_cv)
n5 <- length(part1_earlyWasteDataSet_testing$xos)
n6 <- length(part1_earlyWasteDataSet_testing$xos)


e <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "XOS (g/L)") %>% 
  mutate(modelname = "location splitting") %>% 
  mutate(samplinglocation = "early/waste") %>% 
  select(!test)

e$n <- c(n1,n2,n3,n4,n5,n6)

# early/waste subset
r1	<- round(caret::R2(pred =part1_lateDataSet_training$xos_cal, obs = part1_lateDataSet_training$xos),2)
r2	<- round(caret::RMSE(pred = part1_lateDataSet_training$xos_cal, obs = part1_lateDataSet_training$xos),2)
r3	<- round(caret::R2(pred = part1_lateDataSet_training$xos_cv, obs = part1_lateDataSet_training$xos),2)
r4	<- round(caret::RMSE(pred = part1_lateDataSet_training$xos_cv, obs = part1_lateDataSet_training$xos),2)
r5 <- round(caret::R2(pred = part1_lateDataSet_testing$xos_val, obs =part1_lateDataSet_testing$xos),2)
r6 <- round(caret::RMSE(pred =part1_lateDataSet_testing$xos_val, obs= part1_lateDataSet_testing$xos),2)


n1 <- length(part1_lateDataSet_training$xos)
n2 <- length(part1_lateDataSet_training$xos)
n3 <- length(part1_lateDataSet_training$xos_cv)
n4 <- length(part1_lateDataSet_training$xos_cv)
n5 <- length(part1_lateDataSet_testing$xos)
n6 <- length(part1_lateDataSet_testing$xos)


f <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "XOS (g/L)") %>% 
  mutate(modelname = "location splitting") %>% 
  mutate(samplinglocation = "late") %>% 
  select(!test)

f$n <- c(n1,n2,n3,n4,n5,n6)


rbind(c,d,e,f) %>% 
  write.csv("../results/tables/table2_xos.csv")


## solids Performance 
# set 1  no splitting model results


# early/waste subset
r1	<- round(caret::R2(pred =part1_fullDataSet_earlyWasteSubset_training$solids_cal, obs = part1_fullDataSet_earlyWasteSubset_training$solids),2)
r2	<- round(caret::RMSE(pred = part1_fullDataSet_earlyWasteSubset_training$solids_cal, obs = part1_fullDataSet_earlyWasteSubset_training$solids),2)
r3	<- round(caret::R2(pred = part1_fullDataSet_earlyWasteSubset_training$solids_cv, obs = part1_fullDataSet_earlyWasteSubset_training$solids),2)
r4	<- round(caret::RMSE(pred = part1_fullDataSet_earlyWasteSubset_training$solids_cv, obs = part1_fullDataSet_earlyWasteSubset_training$solids),2)
r5 <- round(caret::R2(pred = part1_fullDataSet_earlyWasteSubset_testing$solids_val, obs =part1_fullDataSet_earlyWasteSubset_testing$solids),2)
r6 <- round(caret::RMSE(pred = part1_fullDataSet_earlyWasteSubset_testing$solids_val, obs= part1_fullDataSet_earlyWasteSubset_testing$solids),2)


n1 <- length(part1_fullDataSet_earlyWasteSubset_training$solids)
n2 <- length(part1_fullDataSet_earlyWasteSubset_training$solids)
n3 <- length(part1_fullDataSet_earlyWasteSubset_training$solids_cv)
n4 <- length(part1_fullDataSet_earlyWasteSubset_training$solids_cv)
n5 <- length(part1_fullDataSet_earlyWasteSubset_testing$solids)
n6 <- length(part1_fullDataSet_earlyWasteSubset_testing$solids)


c<- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "solids (g/L)") %>% 
  mutate(modelname = "no location splitting") %>% 
  mutate(samplinglocation = "early/waste") %>% 
  select(!test)

c$n <- c(n1,n2,n3,n4,n5,n6)

# late subset 

r1	<- round(caret::R2(pred =part1_fullDataSet_lateSubset_training$solids_cal, obs = part1_fullDataSet_lateSubset_training$solids),2)
r2	<- round(caret::RMSE(pred = part1_fullDataSet_lateSubset_training$solids_cal, obs = part1_fullDataSet_lateSubset_training$solids),2)
r3	<- round(caret::R2(pred = part1_fullDataSet_lateSubset_training$solids_cv, obs = part1_fullDataSet_lateSubset_training$solids),2)
r4	<- round(caret::RMSE(pred = part1_fullDataSet_lateSubset_training$solids_cv, obs = part1_fullDataSet_lateSubset_training$solids),2)
r5 <- round(caret::R2(pred = part1_fullDataSet_lateSubset_testing$solids_val, obs =part1_fullDataSet_lateSubset_testing$solids),2)
r6 <- round(caret::RMSE(pred = part1_fullDataSet_lateSubset_testing$solids_val, obs= part1_fullDataSet_lateSubset_testing$solids),2)


n1 <- length(part1_fullDataSet_lateSubset_training$solids)
n2 <- length(part1_fullDataSet_lateSubset_training$solids)
n3 <- length(part1_fullDataSet_lateSubset_training$solids_cv)
n4 <- length(part1_fullDataSet_lateSubset_training$solids_cv)
n5 <- length(part1_fullDataSet_lateSubset_testing$solids)
n6 <- length(part1_fullDataSet_lateSubset_testing$solids)

d <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "solids (g/L)") %>% 
  mutate(modelname = "no location splitting") %>% 
  mutate(samplinglocation = "late") %>% 
  select(!test)
d$n <- c(n1,n2,n3,n4,n5,n6)

# sample location split model performance

# early/waste subset
r1	<- round(caret::R2(pred =part1_earlyWasteDataSet_training$solids_cal, obs = part1_earlyWasteDataSet_training$solids),2)
r2	<- round(caret::RMSE(pred = part1_earlyWasteDataSet_training$solids_cal, obs = part1_earlyWasteDataSet_training$solids),2)
r3	<- round(caret::R2(pred = part1_earlyWasteDataSet_training$solids_cv, obs = part1_earlyWasteDataSet_training$solids),2)
r4	<- round(caret::RMSE(pred = part1_earlyWasteDataSet_training$solids_cv, obs = part1_earlyWasteDataSet_training$solids),2)
r5 <- round(caret::R2(pred = part1_earlyWasteDataSet_testing$solids_val, obs =part1_earlyWasteDataSet_testing$solids),2)
r6 <- round(caret::RMSE(pred =part1_earlyWasteDataSet_testing$solids_val, obs= part1_earlyWasteDataSet_testing$solids),2)


n1 <- length(part1_earlyWasteDataSet_training$solids)
n2 <- length(part1_earlyWasteDataSet_training$solids)
n3 <- length(part1_earlyWasteDataSet_training$solids_cv)
n4 <- length(part1_earlyWasteDataSet_training$solids_cv)
n5 <- length(part1_earlyWasteDataSet_testing$solids)
n6 <- length(part1_earlyWasteDataSet_testing$solids)


e <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "solids (g/L)") %>% 
  mutate(modelname = "location splitting") %>% 
  mutate(samplinglocation = "early/waste") %>% 
  select(!test)

e$n <- c(n1,n2,n3,n4,n5,n6)

# early/waste subset
r1	<- round(caret::R2(pred =part1_lateDataSet_training$solids_cal, obs = part1_lateDataSet_training$solids),2)
r2	<- round(caret::RMSE(pred = part1_lateDataSet_training$solids_cal, obs = part1_lateDataSet_training$solids),2)
r3	<- round(caret::R2(pred = part1_lateDataSet_training$solids_cv, obs = part1_lateDataSet_training$solids),2)
r4	<- round(caret::RMSE(pred = part1_lateDataSet_training$solids_cv, obs = part1_lateDataSet_training$solids),2)
r5 <- round(caret::R2(pred = part1_lateDataSet_testing$solids_val, obs =part1_lateDataSet_testing$solids),2)
r6 <- round(caret::RMSE(pred =part1_lateDataSet_testing$solids_val, obs= part1_lateDataSet_testing$solids),2)


n1 <- length(part1_lateDataSet_training$solids)
n2 <- length(part1_lateDataSet_training$solids)
n3 <- length(part1_lateDataSet_training$solids_cv)
n4 <- length(part1_lateDataSet_training$solids_cv)
n5 <- length(part1_lateDataSet_testing$solids)
n6 <- length(part1_lateDataSet_testing$solids)


f <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "solids (g/L)") %>% 
  mutate(modelname = "location splitting") %>% 
  mutate(samplinglocation = "late") %>% 
  select(!test)

f$n <- c(n1,n2,n3,n4,n5,n6)


rbind(c,d,e,f) %>% 
  write.csv("../results/tables/table2_solids.csv")
