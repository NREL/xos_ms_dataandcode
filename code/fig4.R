# Code for Producing Figure 3, depicting the differences in average raw and transformed NIRS full range NIRS spectra by sample grouping 

#load packages

library(tidyverse)
library(prospectr)
library(openxlsx)
library(ggfortify)
library(ggmagnify)
library(patchwork)
library(ggpmisc) 

# set color pallette

pallette  <- c("#000000", "#DDAA33",  "#BB5566","#004488")

# source spectra processing function

source("./commonCode/preprocess_spectra.R")

rect <- data.frame(xmin =2250, ymin= -0.02, xmax =2450, ymax = 0.006)

# load data

data <- read.xlsx("../data/raw/sampleData.xlsx") %>% 
  mutate(groupingType = factor(groupingType, levels = c("early","waste","late"))) %>% 
  mutate(grouping = factor(grouping, levels = c("early/waste", "late"), 
                           labels =  c("A: Early/Waste Stream Sampling Locations", "B: Late Stream Sampling Locations")))

# get column names to select for plotting

speccols <- colnames(data[which(colnames(data)=="400.00"):which(colnames(data)=="2499.50")] )
transformed_speccols <- colnames(data %>% preprocess_spectra())


# calculate average raw spectra by sample location group

spectra_grouped <- data %>% group_by(groupingType) %>% 
  summarize(across(all_of(speccols), mean)) 

# plot sample location average raw spectra from 1100nm to 2500nm

a <- spectra_grouped %>% 
  pivot_longer(cols = all_of(speccols), names_to = "wavelength", values_to = "absorbance") %>% 
  mutate(wavelength = as.numeric(wavelength)) %>% 
  mutate(absorbance_bump= 
                                 ifelse(groupingType == "early", absorbance, 
                                        ifelse(groupingType == "waste", absorbance + .05, 
                                               ifelse(groupingType == "late", absorbance + .1, absorbance)))) %>% 
  ggplot(aes(x=wavelength, y = absorbance_bump, col = groupingType)) +
  geom_line(size =.8) +
  theme_bw() +
  labs(x = "Wavelength (nm)", y = "Transflectance", tag = "A") +
  scale_color_manual(values = c(pallette[2:4]), 
                     name = "Process Sampling Location") +
  theme(legend.position = c(0.02, .92),
        legend.justification = c("left","top"), 
        legend.text = element_text(size = 11),
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank(),
        legend.title  = element_text(size = 11),
        legend.key.size = unit(1.4, "mm"),
        legend.spacing.y = unit(.2, "mm"),
        axis.title = element_text(size =11), 
        axis.text = element_text(size = 11)) +
  xlim(1350, 2500)

#plot sample location averaged transformed spectra from 1100nm to 2500nm

b <- spectra_grouped%>% preprocess_spectra() %>%  
  cbind(spectra_grouped %>% select(groupingType)) %>% 
  pivot_longer(cols = any_of(transformed_speccols), names_to = "wavelength", values_to = "absorbance") %>% 
  mutate(wavelength = as.numeric(wavelength)) %>%
  mutate(absorbance_bump= 
           ifelse(groupingType== "early", absorbance, 
                  ifelse(groupingType == "waste", absorbance + .0006, 
                         ifelse(groupingType == "late", absorbance + .0012, absorbance)))) %>% 
  ggplot(aes(x=wavelength, y = absorbance_bump, col = groupingType)) +
  geom_line(size = .8) +
  theme_bw() +
  labs(x = "Wavelength (nm)", y = "Transformed Transflectance", tag = "B") +
  scale_color_manual(values = c(pallette[2:4]), name = "Process Sampling Location") +
  theme(legend.position = "none",
        legend.justification = c("left","top"), 
        legend.text = element_text(size = 11),
        legend.title  = element_text(size = 11),
        legend.key.size = unit(1.4, "mm"),
        legend.spacing.y = unit(.2, "mm"),
        axis.title = element_text(size =11), 
        axis.text = element_text(size = 11)) +
  ylim(-0.0025,0.013) +
  xlim(1350, 2500)

# Combine plots

a/b
  
# Save figure as a tiff

ggsave("../results/figures/figure4.tiff", width  = 170, height = 160, units = "mm", compression = "lzw")



