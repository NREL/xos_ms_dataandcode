# Code for producing Figure 6: PCA of all samples colored by sampling location 

#load packages

library(tidyverse)
library(pls)
library(prospectr)
library(ggfortify)
library(patchwork)
library(ggpmisc) 

# set color pallette

pallette  <- c("#000000", "#DDAA33",  "#BB5566","#004488")

#load spectra processing function

source("./commonCode/preprocess_spectra.R")

#load data

data <- read.xlsx("../data/raw/sampleData.xlsx") %>% 
  mutate(groupingType = factor(groupingType, levels = c("early","waste","late"))) %>% 
  mutate(grouping = factor(grouping, levels = c("early/waste", "late"), 
                           labels =  c("A: Early/Waste Stream Sampling Locations", "B: Late Stream Sampling Locations")))

# Build a PCA of the Transformed Spectra

pca <- data %>% preprocess_spectra() %>% prcomp(center = TRUE, scale = FALSE)
pcasum <- summary(pca)

# Create a scores plot of transformed PCA- plot first 2 PCs

a <- data %>% 
  mutate(PC1 = pca$x[,1], 
         PC2 = pca$x[,2]) %>% 
  ggplot(aes(x=PC1, y = PC2, col = groupingType)) +
  geom_point(size = 3, alpha = .8) +
  theme_bw() +
  scale_color_manual(values = c(pallette [2:4]), name = "Process Sampling Location") +
  labs(x=paste0("PC1 (", as.character(round(pcasum$importance[2,1]*100,2)), "% Variance Explained)" ),
       y =paste0("PC2 (", as.character(round(pcasum$importance[2,2]*100,2)), "% Variance Explained)" ), 
       tag = "A") +
  theme(plot.title = element_text(hjust = .5)) +
  theme(legend.position = "none")

# plot PC1 vs XOS concentration

b <- data %>% 
  mutate(PC1 = pca$x[,1], 
         PC2 = pca$x[,2]) %>% 
  ggplot(aes(x=PC1, y = xos, col = groupingType)) +
  geom_point(size = 3, alpha = .8) +
  theme_bw() +
  scale_color_manual(values = c(pallette[2:4]), name = "Process Sampling Location") +
  labs(x=paste0("PC1 (", as.character(round(pcasum$importance[2,1]*100,2)), "% Variance Explained)" ),
       y = "XOS (g/L)", 
       tag = "B")+
  theme(plot.title = element_text(hjust = .5),
        legend.position = "bottom")

# combine plots with formatting

combined <- a + b &
  theme(legend.position = "bottom",
        legend.justification = c("center","top"), 
        legend.text = element_text(size = 8),
        legend.title  = element_text(size = 8),
        legend.key.size = unit(1.22, "mm"),
        legend.spacing.y = unit(.2, "mm"),
        axis.title = element_text(size =7), 
        axis.text = element_text(size = 6))

# combine legends

e <- combined + plot_layout(guides = 'collect')  

e

# save results as a tiff

ggsave("../results/figures/figure6.tiff", width  = 180, height = 100, units = "mm", compression = "lzw")
