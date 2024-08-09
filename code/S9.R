# Code to develop •	Figure S9 contains predicted vs measured and predicted vs residual plots for the late sample set only full
# spectra model’s measurement of monomeric xylose in the calibration, cross validation, and independent validation sets


#load packages
library(tidyverse)
library(pls)
library(prospectr)
library(ggfortify)
library(patchwork)
library(ggpmisc) 
pallette  <- c("#000000", "#004488")


#load testing and training data from each model
part1_lateDataSet_testing <- readRDS("../data/processed/part1_lateDataSet_testing.RDS")
part1_lateDataSet_training <- readRDS("../data/processed/part1_lateDataSet_training.RDS")



part1_lateDataSet_testing <- part1_lateDataSet_testing %>% 
  mutate(grouping = factor(grouping , levels = c("early/waste", "late"), labels = c("A: Early/Waste Stream Sampling Locations", "B: Late Stream Sampling Locations"))) %>% 
  mutate(groupingType = factor(groupingType, levels = c("early","waste","late")))

part1_lateDataSet_training <- part1_lateDataSet_training %>% 
  mutate(grouping = factor(grouping , levels = c("early/waste", "late"), labels = c("A: Early/Waste Stream Sampling Locations", "B: Late Stream Sampling Locations"))) %>% 
  mutate(groupingType = factor(groupingType, levels = c("early","waste","late")))




a <- part1_lateDataSet_training %>% ggplot(aes(x=xylose_free, y =xf_cal, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_abline(slope = 1)+theme_bw()+
  labs(x="Measured [xylose_free] (g/L)",
       y= "Predicted [xylose_free] (g/L)",
       title = "Calibration")+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        strip.text= element_text(colour = "black", size =9, face =2, hjust = 0),
        strip.background = element_blank(),
        legend.key.height = unit(1,"mm"),
        legend.position = "bottom",
        title = element_text(colour = "black", size =9.5, face =2, hjust = 0.5),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))+
  scale_shape_manual(values = c(1,19,2), name = "Model")+
  expand_limits(x = c(-2, 2), y =  c(-2, 2) )+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")
a
b <- part1_lateDataSet_training %>% ggplot(aes(x=xf_cal, y =xf_cal-xylose_free, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_hline(yintercept = 0)+theme_bw()+
  labs(x="Predicted [xylose_free] (g/L)",
       y= "Predicted - Measured [xylose_free] (g/L)"
  )+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        strip.text= element_text(colour = "black", size =9, face =2, hjust = 0),
        strip.background = element_blank(),
        legend.key.height = unit(1,"mm"),
        legend.position = "bottom",
        title = element_text(colour = "black", size =9.5, face =2, hjust = 0.5),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))+
  scale_shape_manual(values = c(1,19,2), name = "Model")+
  expand_limits(
     x = c(-2, 2), y =  c(-2,2) )+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")

b
a+b
c <- part1_lateDataSet_training %>% ggplot(aes(x=xylose_free, y =xf_cv, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_abline(slope = 1)+theme_bw()+
  labs(x="Measured [xylose_free] (g/L)",
       y= "Predicted [xylose_free] (g/L)",
       title = "Cross Validation")+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        strip.text= element_text(colour = "black", size =9, face =2, hjust = 0),
        strip.background = element_blank(),
        legend.key.height = unit(1,"mm"),
        legend.position = "bottom",
        title = element_text(colour = "black", size =9.5, face =2, hjust = 0.5),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))+
  scale_shape_manual(values = c(1,19,2), name = "Model")+
  expand_limits(
    x = c(-2,2), y =  c(-2, 2) )+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")

d <- part1_lateDataSet_training %>% ggplot(aes(x=xf_cv, y =xf_cv-xylose_free, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_hline(yintercept = 0)+theme_bw()+
  labs(x="Predicted [xylose_free] (g/L)",
       y= "Predicted - Measured [xylose_free] (g/L)"
  )+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        strip.text= element_text(colour = "black", size =9, face =2, hjust = 0),
        strip.background = element_blank(),
        legend.key.height = unit(1,"mm"),
        legend.position = "bottom",
        title = element_text(colour = "black", size =9.5, face =2, hjust = 0.5),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))+
  scale_shape_manual(values = c(1,19,2), name = "Model")+
  expand_limits(
    x = c(-2, 2), y =  c(-2,2) )+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")
d
e <- part1_lateDataSet_testing %>% ggplot(aes(x=xylose_free, y =xf_val, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_abline(slope = 1)+theme_bw()+
  labs(x="Measured [xylose_free] (g/L)",
       y= "Predicted [xylose_free] (g/L)",
       title = "Independent Validation")+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        strip.text= element_text(colour = "black", size =9, face =2, hjust = 0),
        strip.background = element_blank(),
        legend.key.height = unit(1,"mm"),
        legend.position = "bottom",
        title = element_text(colour = "black", size =9.5, face =2, hjust = 0.5),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))+
  scale_shape_manual(values = c(1,19,2), name = "Model")+
  expand_limits(
    x = c(-2, 2), y =  c(-2, 2) )+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")

f<- part1_lateDataSet_testing %>% ggplot(aes(x=xf_val, y =xf_val-xylose_free, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_hline(yintercept = 0)+theme_bw()+
  labs(x="Predicted [xylose_free] (g/L)",
       y= "Predicted - Measured [xylose_free] (g/L)"
  )+
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        strip.text= element_text(colour = "black", size =9, face =2, hjust = 0),
        strip.background = element_blank(),
        legend.key.height = unit(1,"mm"),
        legend.position = "bottom",
        title = element_text(colour = "black", size =9.5, face =2, hjust = 0.5),
        panel.background = element_rect(fill = "white",colour = "black", size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "lightgrey"))+
  scale_shape_manual(values = c(1,19,2), name = "Model")+
  expand_limits(
    x = c(-2, 2), y =  c(-2,2) )+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")

g <- (a+b)/(c+d)/(e+f)

h <- g&theme(legend.position = "bottom")
h+plot_layout(guides = "collect")

ggsave("../results/figures/S9.tiff", width  = 180, height = 240, units = "mm", compression = "lzw")