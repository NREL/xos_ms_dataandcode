#code to produce figure 5, which show the spectral differences between additions of xos and xylose at varying concentrations in water

#load packages

library(openxlsx)
library(tidyverse)
library(prospectr)
library(ggfortify)
library(patchwork)
library(ggpmisc) 

# set color pallette

pallette  <- c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c")

# load spectra processing function

source("./commonCode/preprocess_spectra.R")

# load real sample data

data <- read.xlsx("../data/raw/standards.xlsx") 

# load control data

controls <- read.xlsx("../data/raw/controls.xlsx") %>% 
  filter(sample == "water") %>% 
  mutate(xylose_free = 0,
         xos = 0) 

#make vectors containing spectral column names for easy plotting

speccols <- colnames(data[which(colnames(data)=="400.00"):which(colnames(data)=="2499.50")] )
transformed_speccols <- colnames(data %>% preprocess_spectra())

# process signal and clean up labels for plotting

transformedSpectra <- data %>% 
  preprocess_spectra() %>% 
  as.data.frame() %>% 
  mutate(xos = round(data$xos,0),
         xylose_free = round(data$xylose_free,0),
         sample = round(data$sample,0))

# plot transformed truncated spectra for samples with xylose additions

a <- transformedSpectra %>% 
  filter(xos == 0) %>% 
  pivot_longer(cols = transformed_speccols, names_to = "wavelength", values_to = "absorbance") %>%
  mutate(absorbance_bump = ifelse(xylose_free  == 1, absorbance+.001, 
                                  ifelse(xylose_free ==5, absorbance+.002,
                                         ifelse(xylose_free ==10, absorbance+.003, 
                                                ifelse(xylose_free ==20, absorbance+.004, 
                                                       ifelse(xylose_free ==0, absorbance,
                                                              NA)))))) %>% 
  mutate(wavelength = as.numeric(wavelength)) %>% 
  mutate(xylose_free = factor(xylose_free, levels = c("0","1","5","10","20"))) %>% 
  ggplot(aes(x=wavelength, y = absorbance_bump, col = xylose_free)) +
  geom_line() +
  geom_line(size = .7) +
  theme_bw() +
  scale_color_manual(name = "Mono Xylose (g/L)", values = pallette) +
  labs(y = "Transformed Transflectance", x = "Wavelength (nm)", tag = "A") +
  theme(plot.title = element_text(size = 11,
                                  face = "bold",
                                  hjust = .5,
                                  vjust = 1),
        axis.title = element_text(size = 11),
        legend.position = c(0.15, .92),
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank(),
        legend.justification = c("left","top"), 
        legend.text = element_text(size = 10),
        legend.title  = element_text(size = 10),
        legend.key.size = unit(1.5, "mm"),
        legend.spacing.y = unit(.2, "mm"),
        strip.background = element_rect(fill = "white",
                                        colour = "white"), 
        axis.text = element_text(size = 11)) +
  xlim(1350,2500) +
  ylim(-0.0025,0.015)

# plot transformed truncated spectra for samples with xos additions

b <- transformedSpectra %>% 
  filter(xylose_free == 0) %>% 
  pivot_longer(cols = transformed_speccols, names_to = "wavelength", values_to = "absorbance") %>%
  mutate(absorbance_bump = ifelse(xos  == 1, absorbance+.001, 
                                  ifelse(xos ==5, absorbance+.002,
                                         ifelse(xos==10, absorbance+.003, 
                                                ifelse(xos ==20, absorbance+.004, 
                                                       ifelse(xos ==0, absorbance,
                                                              NA)))))) %>% 
  mutate(wavelength = as.numeric(wavelength)) %>% 
  mutate(xos = factor(xos, levels = c("0","1","5","10","20"))) %>% 
  ggplot(aes(x=wavelength, y = absorbance_bump, col = xos)) +
  geom_line() +
  geom_line(size = .7) +
  theme_bw() +
  scale_color_manual(name = "XOS (g/L)", values = pallette) +
  labs(y = "Transformed Transflectance", x = "Wavelength (nm)", tag = "B") +
  theme(strip.background = element_rect(fill = "white",
                                        colour = "white"),
        plot.title = element_text(size = 11,
                                  face = "bold",
                                  hjust = .5, 
                                  vjust = 1),
        axis.title = element_text(size = 11),
        legend.position = c(0.15, .92),
        legend.box.background = element_rect(colour = "black"),
        legend.background = element_blank(),
        legend.justification = c("left","top"), 
        legend.text = element_text(size = 10),
        legend.title  = element_text(size = 10),
        legend.key.size = unit(1.5, "mm"),
        legend.spacing.y = unit(.2, "mm"), 
        axis.text = element_text(size = 11)) +
  xlim(1350,2500) +
  ylim(-0.0025,0.015)

a/b


data<- data %>% mutate(
  sample = as.character(sample)
)
data <- bind_rows(data, controls) %>% 
  mutate(xos = ifelse(sample == "water", 0,xos )) %>%
  mutate(xylose_free = ifelse(sample == "water", 0,xylose_free ))

# produce PCA for full spectral region

pca <- data %>% 
  preprocess_spectra() %>% 
  prcomp(center = TRUE)

pcasum <- summary(pca)

# produce a score plot of full spectra PCA results

c <- data %>% 
  mutate(PC1 = pca$x[,1],
         PC2 = pca$x[,2]) %>% 
  mutate(concentration = as.character(round(xos+xylose_free),0)) %>% 
  mutate(concentration  = factor(concentration , levels = c("0","1","5","10","20"))) %>%
  mutate(Type = c("Water", "XOS","XOS","XOS","XOS","Monomeric Xylose","Monomeric Xylose","Monomeric Xylose","Monomeric Xylose",
                  "Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water")) %>%
  mutate(Type = factor(Type, levels = c("Water","Monomeric Xylose", "XOS"))) %>% 
  ggplot(aes(x=PC1, y = PC2, color= concentration, shape = Type)) +
  geom_point(size = 4.5, col = "black") +
  geom_point(size = 4) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x=paste0("PC1 (", as.character(round(pcasum$importance[2,1]*100,2)), "% Variance Explained)" ),
       y =paste0("PC2 (", as.character(round(pcasum$importance[2,2]*100,2)), "% Variance Explained)" ), 
       tag = "C") +
  theme(plot.title = element_text(hjust = .5)) +
  scale_shape_manual(values = c(15, 16, 17)) +
  scale_color_manual(
    name = "Concentration (g/L)",
    values = pallette)

# produce pca for overtone spectra region

pca <- data %>% 
  preprocess_spectra_overtone() %>% 
  prcomp(center = TRUE)

pcasum <- summary(pca)

# produce a score plot for overtone spectra pca results

d <- data %>% 
  mutate(PC1 = pca$x[,1],
         PC2 = pca$x[,2]) %>% 
  mutate(concentration = as.character(round(xos+xylose_free),0)) %>% 
  mutate(concentration  = factor(concentration , levels = c("0","1","5","10","20"))) %>%
  mutate(Type = c("Water", "XOS","XOS","XOS","XOS","Monomeric Xylose","Monomeric Xylose","Monomeric Xylose","Monomeric Xylose",
                  "Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water")) %>%
  mutate(Type = factor(Type, levels = c("Water","Monomeric Xylose", "XOS"))) %>% 
  ggplot(aes(x=PC1, y = PC2, color= concentration, shape = Type)) +
  geom_point(size = 4.5, col = "black") +
  geom_point(size = 4) +
  theme_bw()+
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x=paste0("PC1 (", as.character(round(pcasum$importance[2,1]*100,2)), "% Variance Explained)" ),
                             y =paste0("PC2 (", as.character(round(pcasum$importance[2,2]*100,2)), "% Variance Explained)" ), 
                             tag = "D")+
  theme(plot.title = element_text(hjust = .5)) +
  scale_shape_manual(values = c(15, 16, 17)) +
  scale_color_manual(
    name = "Concentration (g/L)",
    values = pallette)

# Combine PCA score plots

e <- c + d

# format combined pca plots

ee <- e +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.justification = c("left","top"), 
        legend.text = element_text(size = 8),
        legend.title  = element_text(size = 8),
        legend.key.size = unit(1.22, "mm"),
        legend.spacing.y = unit(.2, "mm"),
        axis.title = element_text(size =7), 
        axis.text = element_text(size = 6))


# combine all plots

(a/b)+ee

# Save figure as a tiff

ggsave("../results/figures/figure5.tiff", width  = 180, height = 240, units = "mm", compression = "lzw" )  
