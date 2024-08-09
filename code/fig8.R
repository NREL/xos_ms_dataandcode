# code to produce figure 8

#load packages-----------------------------

library(openxlsx)
library(tidyverse)
library(prospectr)
library(ggfortify)
library(patchwork)
library(ggpmisc) 
library(egg)

# set color pallette-----------------------------

pallette  <- c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c")

#load spectra processing function-----------------------------

source("./commonCode/preprocess_spectra.R")

#load data-----------------------------

standards <- read.xlsx("../data/raw/standards.xlsx")

controls <- read.xlsx("../data/raw/controls.xlsx") 


#make objects containing spectral column names for easy plotting-----------------------------

speccols <- colnames(controls[which(colnames(controls)=="400.00"):which(colnames(controls)=="2499.50")])

transformed_speccols <- colnames(controls %>% preprocess_spectra())

# create a data frame for plotting with the wavelengths vs loadings of the 1st PCA component for water samples-----------------------------

# subset water samples 

water <- controls %>% filter(sample =="water")

#process water samples

water_spec_t <-  water %>% preprocess_spectra()

# run a pca on water sample spectra

water_pca <- prcomp(water_spec_t, center = TRUE) 

# put PC1 and PC2 loadings for water only spectra pca into a data frame

loadings_w <- data.frame(PC1 = water_pca$rotation[,1],
                       PC2 = water_pca$rotation[,2],
                       wavelength = rownames(water_pca$rotation))%>% 
  mutate(Type = "Water Control")

pcasum <- summary(water_pca)

#create a data frame for plotting with the wavelengths vs loadings of the 1st PCA component for monomeric xylose step samples-----------------------------

# filter for the standards that incremental amounts of xylose

xylose <- standards %>% filter(xylose_free != 0)

#process the spectra for the xylose samples

xylose_spec_t <-  xylose %>% preprocess_spectra()

# run a PCA on the xylose sample processed spectra

xylose_pca <- prcomp(xylose_spec_t, center = TRUE) 

# put PC1 and PC2 loadings for xylose standard spectra pca into a data frame

loadings_x <- data.frame(PC1 = xylose_pca$rotation[,1],
                       PC2 = xylose_pca$rotation[,2],
                       wavelength = rownames(xylose_pca$rotation)) %>% 
  mutate(Type = "Monomeric Xylose")

#create a data frame for plotting with the wavelengths vs loadings of the 1st PCA component for xos step samples-----------------------------

# filter for the standards that incremental amounts of xos

xos <- standards %>% filter(xos != 0)

#process xos spectra

xos_spec_t <-  xos %>% preprocess_spectra()

# run PCA on xos sample processed spectra

xos_pca <- prcomp(xos_spec_t, center = TRUE) 

# put PC1 and PC2 loadings for xos standard spectra pca into a data frame

loadings_xos <- data.frame(PC1 = xos_pca$rotation[,1],
                       PC2 = xos_pca$rotation[,2],
                       wavelength = rownames(xos_pca$rotation))%>% 
  mutate(Type = "XOS")

# bind the PC1, PC2, wavelength spectra into one long tidy dataframe 

loadings <- rbind(loadings_w, loadings_x, loadings_xos) %>% 
  mutate(Type = factor(Type, levels = c("Water Control", "Monomeric Xylose","XOS")))

# produce a plot of wavelength vs loadings across each samples set -- water control, monomeric xylose, and xos

c <- loadings %>% 
  mutate(wavelength = as.numeric(wavelength)) %>% 
  ggplot(aes(x=wavelength, y = PC1)) +
  geom_line(linewidth = 1, alpha = .8) +
  theme_bw() +
  labs(x="wavelength (nm)", y = paste0("PC1 Loadings")) +
  facet_wrap(~Type, nrow = 3) +
  geom_vline(xintercept = 2100, col = "red") +
  geom_vline(xintercept = 2450, col = "red") +
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.width = unit(0.5,"mm"), 
        strip.text.y = element_blank(),
        strip.text.x= element_text(colour = "black", size =9, face =2),
        strip.background = element_blank(),
        legend.key.height = unit(1,"mm"),
        panel.background = element_rect(fill = "white",
                                        colour = "black", 
                                        size = 0.5,
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.5,
                                        linetype = 'solid', 
                                        colour = "lightgrey"))

# add tags to the graphs

cc <- tag_facet(c, tag_pool = c("C", "D","E"), open = "", close = "")

# load data to produce pca scores plots

data <- read.xlsx("../data/raw/standards.xlsx") 

controls <- read.xlsx("../data/raw/controls.xlsx") %>% 
  filter(sample == "water") %>% 
  mutate(xylose_free = 0,
         xos = 0) 

data<- data %>% mutate(
  sample = as.character(sample)
)

data <- bind_rows(data, controls) %>% 
  mutate(xos = ifelse(sample == "water", 0,xos )) %>%
  mutate(xylose_free = ifelse(sample == "water", 0,xylose_free ))

# run a full spectra processed pca on the combined data set 

pca <- data %>% 
  preprocess_spectra() %>% 
  prcomp(center = TRUE)

pcasum <- summary(pca)

# produce a scores plot from the full spectra pca results

aa<- data %>% 
  mutate(PC1 = pca$x[,1],
         PC2 = pca$x[,2]) %>% 
  mutate(concentration = as.character(round(xos+xylose_free),0)) %>% 
  mutate(concentration  = factor(concentration , levels = c("0","1","5","10","20"))) %>%
  mutate(Type = c("Water", "XOS","XOS","XOS","XOS","Monomeric Xylose","Monomeric Xylose","Monomeric Xylose","Monomeric Xylose",
                  "Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water")) %>%
  mutate(Type = factor(Type, levels = c("Water","Monomeric Xylose", "XOS"))) %>% 
  ggplot(aes(x=PC1, y = PC2, color= concentration, shape = Type)) +
  geom_point(size = 4, col = "black") +
  geom_point(size = 3.5) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x=paste0("PC1 (", as.character(round(pcasum$importance[2,1]*100,2)), "% Variance Explained)" ),
       y =paste0("PC2 (", as.character(round(pcasum$importance[2,2]*100,2)), "% Variance Explained)" ), 
       tag = "A")+
  theme(plot.title = element_text(hjust = .5)) +
  scale_shape_manual(values = c(15, 16, 17)) +
  scale_color_manual(
    name = "Concentration (g/L)",
    values = pallette)

# run a pca on the combinational region only combined spectra

pca <- data %>% 
  preprocess_spectra_overtone() %>% 
  prcomp(center = TRUE)

pcasum <- summary(pca)

#  produce a scores plot from the combinational region only spectra pca results

bb <- data %>% 
  mutate(PC1 = pca$x[,1],
         PC2 = pca$x[,2]) %>% 
  mutate(concentration = as.character(round(xos+xylose_free),0)) %>% 
  mutate(concentration  = factor(concentration , levels = c("0","1","5","10","20"))) %>%
  mutate(Type = c("Water", "XOS","XOS","XOS","XOS","Monomeric Xylose","Monomeric Xylose","Monomeric Xylose","Monomeric Xylose",
                  "Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water","Water")) %>%
  mutate(Type = factor(Type, levels = c("Water","Monomeric Xylose", "XOS"))) %>% 
  ggplot(aes(x=PC1, y = PC2, color= concentration, shape = Type)) +
  geom_point(size = 4, col = "black") +
  geom_point(size = 3.5) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  labs(x=paste0("PC1 (", as.character(round(pcasum$importance[2,1]*100,2)), "% Variance Explained)" ),
       y =paste0("PC2 (", as.character(round(pcasum$importance[2,2]*100,2)), "% Variance Explained)" ), 
       tag = "B") +
  theme(plot.title = element_text(hjust = .5)) +
  scale_shape_manual(values = c(15, 16, 17)) +
  scale_color_manual(
    name = "Concentration (g/L)",
    values = pallette)

# combine the scores plots

aaa <- aa+bb

# format the combined scores plots

aaa <- aaa+plot_layout(guides = "collect")&
  theme(legend.position = "bottom",legend.justification = c("center","top"), 
        legend.text = element_text(size = 6),
        legend.title  = element_text(size = 6),
        legend.key.size = unit(1.22, "mm"),
        legend.spacing.y = unit(.2, "mm"),
        axis.title = element_text(size =7), 
        axis.text = element_text(size = 6),
        plot.tag = element_text(size = 11, face = "bold"))

# combined the scores plots with the loadings plots

aaa/cc

# save the resulting plot as a tiff

ggsave("../results/figures/figure8.tiff", width  = 180, height = 180, units = "mm", compression = "lzw")


