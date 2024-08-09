# code to produce supplementary material item 1- distributions of minor components with complete information in each dataset



#load packages
library(tidyverse)
library(openxlsx)


#load data
data <- read.xlsx("../data/raw/sampleData.xlsx") %>% 
  mutate(groupingType = factor(groupingType, levels = c("early","waste","late")))

# summarize minor components

data %>% select(groupingType, glucose_tot , glucose_free, galactose_tot,galactose_free,arabinose_tot,arabinose_free) %>%
  pivot_longer(cols = c(  glucose_tot , glucose_free, galactose_tot,galactose_free,arabinose_tot,arabinose_free), names_to = "Constituent", values_to = "value") %>% 
  group_by(groupingType, Constituent
  ) %>% 
  summarize(n = n(), 
            mean = round(mean(value),1),
            min = round(min(value),1),
            max = round(max(value),1),
            sd = round(sd(value),1),
            median = round(median(value),1),
            q1 = round(quantile(value, .25),1),
            q3 = round(quantile(value, .75),1)) %>% 
  write.csv("../results/tables/S1.csv")

data %>% select(groupingType, glucose_tot , glucose_free, galactose_tot,galactose_free,arabinose_tot,arabinose_free) %>%
  pivot_longer(cols = c(  glucose_tot , glucose_free, galactose_tot,galactose_free,arabinose_tot,arabinose_free), names_to = "Constituent", values_to = "value") %>% 
  group_by( Constituent
  ) %>% 
  summarize(n = n(), 
            mean = round(mean(value),1),
            min = round(min(value),1),
            max = round(max(value),1),
            sd = round(sd(value),1),
            median = round(median(value),1),
            q1 = round(quantile(value, .25),1),
            q3 = round(quantile(value, .75),1)) %>% 
  write.csv("../results/tables/S1_nosamplingsplit.csv")