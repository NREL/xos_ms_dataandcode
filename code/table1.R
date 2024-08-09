# code to produce table 1-  wet chemistry distribution 

# figure 3- wet chemistry distribution 

#load packages
library(tidyverse)
library(pls)
library(prospectr)
library(ggfortify)
library(patchwork)
library(ggpmisc) 

#load data
data <- read.xlsx("../data/raw/sampleData.xlsx") %>% 
  mutate(groupingType = factor(groupingType, levels = c("early","waste","late")))



data %>% select(groupingType, xos, xylose_free,solids ) %>%
  pivot_longer(cols = c( xos, xylose_free,solids ), names_to = "Constituent", values_to = "value") %>% 
  group_by(groupingType, Constituent
           ) %>% 
  summarize(n = n(), 
            mean = round(mean(value),2),
            min = round(min(value),2),
            max = round(max(value),2),
            sd = round(sd(value),2),
            median = round(median(value),2),
            q1 = round(quantile(value, .25),2),
            q3 = round(quantile(value, .75),2)) %>% 
  write.csv("../results/tables/table1.csv")

data %>% select(groupingType, xos, xylose_free,solids ) %>%
  pivot_longer(cols = c( xos, xylose_free,solids ), names_to = "Constituent", values_to = "value") %>% 
  group_by( Constituent
  ) %>% 
  summarize(n = n(), 
            mean = round(mean(value),2),
            min = round(min(value),2),
            max = round(max(value),2),
            sd = round(sd(value),2),
            median = round(median(value),2),
            q1 = round(quantile(value, .25),2),
            q3 = round(quantile(value, .75),2)) %>% 
  write.csv("../results/tables/table1_nosamplingsplit.csv")

