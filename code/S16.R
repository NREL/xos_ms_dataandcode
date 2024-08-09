# Code to develop •	Figure S16 contains predicted vs measured and predicted vs residual plots for the late sample set only 2100-2450nm
# spectra model’s measurement of xos in the calibration, cross validation, and independent validation sets


#load packages
library(tidyverse)
library(pls)
library(prospectr)
library(ggfortify)
library(patchwork)
library(ggpmisc) 
pallette  <- c("#000000", "#004488")


#load testing and training data from each model
part2_lateDataSet_testing <- readRDS("../data/processed/part2_lateDataSet_testing.RDS")
part2_lateDataSet_training <- readRDS("../data/processed/part2_lateDataSet_training.RDS")



part2_lateDataSet_testing <- part2_lateDataSet_testing %>% 
  mutate(grouping = factor(grouping , levels = c("early/waste", "late"), labels = c("A: Early/Waste Stream Sampling Locations", "B: late Stream Sampling Locations"))) %>% 
  mutate(groupingType = factor(groupingType, levels = c("early","waste","late")))

part2_lateDataSet_training <- part2_lateDataSet_training %>% 
  mutate(grouping = factor(grouping , levels = c("early/waste", "late"), labels = c("A: Early/Waste Stream Sampling Locations", "B: late Stream Sampling Locations"))) %>% 
  mutate(groupingType = factor(groupingType, levels = c("early","waste","late")))




a <- part2_lateDataSet_training %>% ggplot(aes(x=xos, y =xos_cal, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_abline(slope = 1)+theme_bw()+
  labs(x="Measured [xos] (g/L)",
       y= "Predicted [xos] (g/L)",
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
  expand_limits(
    x=c(-1, 260), y =  c(-1,260))+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")
a
b <- part2_lateDataSet_training %>% ggplot(aes(x=xos_cal, y =xos_cal-xos, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_hline(yintercept = 0)+theme_bw()+
  labs(x="Predicted [xos] (g/L)",
       y= "Predicted - Measured [xos] (g/L)"
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
    x= c(-1, 260), y =  c(-20,20))+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")

b
a+b
c <- part2_lateDataSet_training %>% ggplot(aes(x=xos, y =xos_cv, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_abline(slope = 1)+theme_bw()+
  labs(x="Measured [xos] (g/L)",
       y= "Predicted [xos] (g/L)",
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
    x=c(-1, 260), y =  c(-1,260))+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")
c
d <- part2_lateDataSet_training %>% ggplot(aes(x=xos_cv, y =xos_cv-xos, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_hline(yintercept = 0)+theme_bw()+
  labs(x="Predicted [xos] (g/L)",
       y= "Predicted - Measured [xos] (g/L)"
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
    x = c(-1, 260), y =  c(-20,20) )+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")
d
e <- part2_lateDataSet_testing %>% ggplot(aes(x=xos, y =xos_val, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_abline(slope = 1)+theme_bw()+
  labs(x="Measured [xos] (g/L)",
       y= "Predicted [xos] (g/L)",
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
    x=c(-1, 260), y =  c(-1,260))+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")

f<- part2_lateDataSet_testing %>% ggplot(aes(x=xos_val, y =xos_val-xos, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_hline(yintercept = 0)+theme_bw()+
  labs(x="Predicted [xos] (g/L)",
       y= "Predicted - Measured [xos] (g/L)"
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
   x=c(-1, 260), y =  c(-20,20) )+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")

g <- (a+b)/(c+d)/(e+f)

h <- g&theme(legend.position = "bottom")
h+plot_layout(guides = "collect")

ggsave("../results/figures/S16.tiff", width  = 180, height = 240, units = "mm", compression = "lzw")