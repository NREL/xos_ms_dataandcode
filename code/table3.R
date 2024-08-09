# code to produce table 3: comparing the performance statistics between the truncated and full spectra models


#load packages
library(tidyverse)
library(pls)
library(prospectr)
library(ggfortify)
library(patchwork)
library(ggpmisc) 


#load data
part1_earlyWasteDataSet_testing <- readRDS("../data/processed/part1_earlyWasteDataSet_testing.RDS")
part1_earlyWasteDataSet_training<- readRDS("../data/processed/part1_earlyWasteDataSet_training.RDS")
part1_lateDataSet_testing <- readRDS("../data/processed/part1_lateDataSet_testing.RDS")
part1_lateDataSet_training<- readRDS("../data/processed/part1_lateDataSet_training.RDS")

part2_earlyWasteDataSet_testing <- readRDS("../data/processed/part2_earlyWasteDataSet_testing.RDS")
part2_earlyWasteDataSet_training<- readRDS("../data/processed/part2_earlyWasteDataSet_training.RDS")
part2_lateDataSet_testing <- readRDS("../data/processed/part2_lateDataSet_testing.RDS")
part2_lateDataSet_training<- readRDS("../data/processed/part2_lateDataSet_training.RDS")


## XOS 

# Performance Results for Full Spectra at both sampling locations

#early/waste
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


a <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "XOS (g/L)") %>% 
  mutate(modelname = "full spectra, location splitting") %>% 
  mutate(samplinglocation = "early/waste") %>% 
  select(!test)

a$n <- c(n1,n2,n3,n4,n5,n6)

#late

r1 <- round(caret::R2(pred =part1_lateDataSet_training$xos_cal, obs = part1_lateDataSet_training$xos),2)
r2 <- round(caret::RMSE(pred = part1_lateDataSet_training$xos_cal, obs = part1_lateDataSet_training$xos),2)
r3 <- round(caret::R2(pred = part1_lateDataSet_training$xos_cv, obs = part1_lateDataSet_training$xos),2)
r4 <- round(caret::RMSE(pred = part1_lateDataSet_training$xos_cv, obs = part1_lateDataSet_training$xos),2)
r5 <- round(caret::R2(pred = part1_lateDataSet_testing$xos_val, obs =part1_lateDataSet_testing$xos),2)
r6 <- round(caret::RMSE(pred =part1_lateDataSet_testing$xos_val, obs= part1_lateDataSet_testing$xos),2)


n1 <- length(part1_lateDataSet_training$xos)
n2 <- length(part1_lateDataSet_training$xos)
n3 <- length(part1_lateDataSet_training$xos_cv)
n4 <- length(part1_lateDataSet_training$xos_cv)
n5 <- length(part1_lateDataSet_testing$xos)
n6 <- length(part1_lateDataSet_testing$xos)


b <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "XOS (g/L)") %>% 
  mutate(modelname = "full spectra, location splitting") %>% 
  mutate(samplinglocation = "late") %>% 
  select(!test)

b$n <- c(n1,n2,n3,n4,n5,n6)


# Performance Results for 2100-2450nm Spectra at both sampling locations

#early/waste

r1 <- round(caret::R2(pred =part2_earlyWasteDataSet_training$xos_cal, obs = part2_earlyWasteDataSet_training$xos),2)
r2 <- round(caret::RMSE(pred = part2_earlyWasteDataSet_training$xos_cal, obs = part2_earlyWasteDataSet_training$xos),2)
r3 <- round(caret::R2(pred = part2_earlyWasteDataSet_training$xos_cv, obs = part2_earlyWasteDataSet_training$xos),2)
r4 <- round(caret::RMSE(pred = part2_earlyWasteDataSet_training$xos_cv, obs = part2_earlyWasteDataSet_training$xos),2)
r5 <- round(caret::R2(pred = part2_earlyWasteDataSet_testing$xos_val, obs =part2_earlyWasteDataSet_testing$xos),2)
r6 <- round(caret::RMSE(pred =part2_earlyWasteDataSet_testing$xos_val, obs= part2_earlyWasteDataSet_testing$xos),2)


n1 <- length(part2_earlyWasteDataSet_training$xos)
n2 <- length(part2_earlyWasteDataSet_training$xos)
n3 <- length(part2_earlyWasteDataSet_training$xos_cv)
n4 <- length(part2_earlyWasteDataSet_training$xos_cv)
n5 <- length(part2_earlyWasteDataSet_testing$xos)
n6 <- length(part2_earlyWasteDataSet_testing$xos)


c <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "XOS (g/L)") %>% 
  mutate(modelname = "2100-2450nm spectra, location splitting") %>% 
  mutate(samplinglocation = "early/waste") %>% 
  select(!test)

c$n <- c(n1,n2,n3,n4,n5,n6)

#late

r1	<- round(caret::R2(pred =part2_lateDataSet_training$xos_cal, obs = part2_lateDataSet_training$xos),2)
r2	<- round(caret::RMSE(pred = part2_lateDataSet_training$xos_cal, obs = part2_lateDataSet_training$xos),2)
r3	<- round(caret::R2(pred = part2_lateDataSet_training$xos_cv, obs = part2_lateDataSet_training$xos),2)
r4	<- round(caret::RMSE(pred = part2_lateDataSet_training$xos_cv, obs = part2_lateDataSet_training$xos),2)
r5 <- round(caret::R2(pred = part2_lateDataSet_testing$xos_val, obs =part2_lateDataSet_testing$xos),2)
r6 <- round(caret::RMSE(pred =part2_lateDataSet_testing$xos_val, obs= part2_lateDataSet_testing$xos),2)


n1 <- length(part2_lateDataSet_training$xos)
n2 <- length(part2_lateDataSet_training$xos)
n3 <- length(part2_lateDataSet_training$xos_cv)
n4 <- length(part2_lateDataSet_training$xos_cv)
n5 <- length(part2_lateDataSet_testing$xos)
n6 <- length(part2_lateDataSet_testing$xos)


d <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "XOS (g/L)") %>% 
  mutate(modelname = "2100-2450nm  spectra, location splitting") %>% 
  mutate(samplinglocation = "late") %>% 
  select(!test)

d$n <- c(n1,n2,n3,n4,n5,n6)

rbind(a,b,c,d) %>% 
  write.csv("../results/tables/table3_xos.csv")

## Solids

# Performance Results for Full Spectra at both sampling locations

#early/waste

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


a <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "solids (g/L)") %>% 
  mutate(modelname = "full spectra, location splitting") %>% 
  mutate(samplinglocation = "early/waste") %>% 
  select(!test)

a$n <- c(n1,n2,n3,n4,n5,n6)

#late

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


b <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "solids (g/L)") %>% 
  mutate(modelname = "full spectra, location splitting") %>% 
  mutate(samplinglocation = "late") %>% 
  select(!test)

b$n <- c(n1,n2,n3,n4,n5,n6)


# Performance Results for 2100-2450nm Spectra at both sampling locations

#early/waste
r1	<- round(caret::R2(pred =part2_earlyWasteDataSet_training$solids_cal, obs = part2_earlyWasteDataSet_training$solids),2)
r2	<- round(caret::RMSE(pred = part2_earlyWasteDataSet_training$solids_cal, obs = part2_earlyWasteDataSet_training$solids),2)
r3	<- round(caret::R2(pred = part2_earlyWasteDataSet_training$solids_cv, obs = part2_earlyWasteDataSet_training$solids),2)
r4	<- round(caret::RMSE(pred = part2_earlyWasteDataSet_training$solids_cv, obs = part2_earlyWasteDataSet_training$solids),2)
r5 <- round(caret::R2(pred = part2_earlyWasteDataSet_testing$solids_val, obs =part2_earlyWasteDataSet_testing$solids),2)
r6 <- round(caret::RMSE(pred =part2_earlyWasteDataSet_testing$solids_val, obs= part2_earlyWasteDataSet_testing$solids),2)


n1 <- length(part2_earlyWasteDataSet_training$solids)
n2 <- length(part2_earlyWasteDataSet_training$solids)
n3 <- length(part2_earlyWasteDataSet_training$solids_cv)
n4 <- length(part2_earlyWasteDataSet_training$solids_cv)
n5 <- length(part2_earlyWasteDataSet_testing$solids)
n6 <- length(part2_earlyWasteDataSet_testing$solids)


c <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "solids (g/L)") %>% 
  mutate(modelname = "2100-2450nm spectra, location splitting") %>% 
  mutate(samplinglocation = "early/waste") %>% 
  select(!test)

c$n <- c(n1,n2,n3,n4,n5,n6)

#late

r1	<- round(caret::R2(pred =part2_lateDataSet_training$solids_cal, obs = part2_lateDataSet_training$solids),2)
r2	<- round(caret::RMSE(pred = part2_lateDataSet_training$solids_cal, obs = part2_lateDataSet_training$solids),2)
r3	<- round(caret::R2(pred = part2_lateDataSet_training$solids_cv, obs = part2_lateDataSet_training$solids),2)
r4	<- round(caret::RMSE(pred = part2_lateDataSet_training$solids_cv, obs = part2_lateDataSet_training$solids),2)
r5 <- round(caret::R2(pred = part2_lateDataSet_testing$solids_val, obs =part2_lateDataSet_testing$solids),2)
r6 <- round(caret::RMSE(pred =part2_lateDataSet_testing$solids_val, obs= part2_lateDataSet_testing$solids),2)


n1 <- length(part2_lateDataSet_training$solids)
n2 <- length(part2_lateDataSet_training$solids)
n3 <- length(part2_lateDataSet_training$solids_cv)
n4 <- length(part2_lateDataSet_training$solids_cv)
n5 <- length(part2_lateDataSet_testing$solids)
n6 <- length(part2_lateDataSet_testing$solids)


d <- data.frame(test = 1:6) %>%
  mutate(parameters = c("R2-cal", "RMSEC", "R2-CV", "RMSECV", "R2-test", "RMSEP")) %>% 
  mutate(values = c(r1,r2,r3,r4,r5,r6)) %>% 
  mutate(constituent = "solids (g/L)") %>% 
  mutate(modelname = "2100-2450nm  spectra, location splitting") %>% 
  mutate(samplinglocation = "late") %>% 
  select(!test)

d$n <- c(n1,n2,n3,n4,n5,n6)

rbind(a,b,c,d) %>% 
  write.csv("../results/tables/table3_solids.csv")
