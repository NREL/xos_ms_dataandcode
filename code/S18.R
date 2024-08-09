# supplemental figure 18- visualizing the control spectra variability

library(openxlsx)
library(tidyverse)
library(prospectr)
library(patchwork)


#load spectra processing function
source("./commonCode/preprocess_spectra.R")

#load data
controls_all <- read.xlsx("../data/raw/controls.xlsx") 

#make objects containing spectral column names for easy plotting
speccols <- colnames(controls_all[which(colnames(controls_all)=="400.00"):which(colnames(controls_all)=="2499.50")] )
transformed_speccols <- colnames(controls_all%>% preprocess_spectra())

controls <- controls_all %>% filter(sample== "water")


a <- controls %>% preprocess_spectra() %>% scale( center = TRUE, scale = FALSE) %>% as.data.frame() %>% 
  cbind(controls %>% select(!any_of(speccols))) %>% 
  pivot_longer(cols = any_of(transformed_speccols), names_to = "wl", values_to = "abs") %>% 
  mutate(wl = as.numeric(wl)) %>% 
  ggplot(aes(x=wl, y = abs, group = spectralId))+geom_line()+
  labs(x="wavelength (nm)", y="Centered Transformed Absorbance",
       title ="Water Control")+theme_bw()+
  theme(legend.position = "bottom",legend.justification = c("center","top"), 
        plot.title = element_text(size = 10, hjust = .5),
                                                 legend.text = element_text(size = 8),
                                                 legend.title  = element_text(size = 8),
                                                 legend.key.size = unit(1.22, "mm"),
                                                 legend.spacing.y = unit(.2, "mm"),
                                                 axis.title = element_text(size =7), 
                                                 axis.text = element_text(size = 6))


controls_matrix <- controls_all %>% filter(sample== "FIL030422-MF-P")


b <- controls_matrix  %>% preprocess_spectra() %>% scale( center = TRUE, scale = FALSE) %>% as.data.frame() %>% 
  cbind(controls_matrix%>% select(!any_of(speccols))) %>% 
  pivot_longer(cols = any_of(transformed_speccols), names_to = "wl", values_to = "abs") %>% 
  mutate(wl = as.numeric(wl)) %>% 
  ggplot(aes(x=wl, y = abs, group = spectralId))+geom_line()+
  labs(x="wavelength (nm)", y="Centered Transformed Absorbance",
       title ="Sample Matrix Control")+theme_bw()+
  theme(legend.position = "bottom",legend.justification = c("center","top"), 
        plot.title = element_text(size = 10, hjust = .5),
        legend.text = element_text(size = 8),
        legend.title  = element_text(size = 8),
        legend.key.size = unit(1.22, "mm"),
        legend.spacing.y = unit(.2, "mm"),
        axis.title = element_text(size =7), 
        axis.text = element_text(size = 6))
a/b
ggsave("../results/figures/S18.tiff", width  = 180, height = 180, units = "mm", compression = "lzw")


