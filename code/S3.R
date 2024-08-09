# Code to generate supplemental material item 3: predicted vs measured and predicted vs residual plots for the full model's performance across calibration, cross validation, and independent validation for monomeric xylose

#load packages
library(tidyverse)
library(pls)
library(prospectr)
library(ggfortify)
library(patchwork)
library(ggpmisc) 
pallette  <- c("#000000", "#DDAA33",  "#BB5566","#004488")


#load testing and training data from each model
part1_fullDataSet_testing <- readRDS("../data/processed/part1_fullDataSet_testing.RDS")
part1_fullDataSet_training <- readRDS("../data/processed/part1_fullDataSet_training.RDS")



part1_fullDataSet_testing <- part1_fullDataSet_testing %>% 
  mutate(grouping = factor(grouping , levels = c("early/waste", "late"), labels = c("A: Early/Waste Stream Sampling Locations", "B: Late Stream Sampling Locations"))) %>% 
  mutate(groupingType = factor(groupingType, levels = c("early","waste","late")))

part1_fullDataSet_training <- part1_fullDataSet_training %>% 
  mutate(grouping = factor(grouping , levels = c("early/waste", "late"), labels = c("A: Early/Waste Stream Sampling Locations", "B: Late Stream Sampling Locations"))) %>% 
  mutate(groupingType = factor(groupingType, levels = c("early","waste","late")))




a <- part1_fullDataSet_training %>% ggplot(aes(x=xylose_free, y =xf_cal, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_abline(slope = 1)+theme_bw()+
  labs(x="Measured [Free Xylose] (g/L)",
       y= "Predicted [Free Xylose] (g/L)",
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
  expand_limits(grouping = c("A: Early/Waste Stream Sampling Locations",
                             "A: Early/Waste Stream Sampling Locations","B: Late Stream Sampling Locations",
                             "B: Late Stream Sampling Locations"),
                x = c(-1, 1.5, -1, 1.5), y =  c(-1, 1.5, -1, 1.5) )+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")
a
b <- part1_fullDataSet_training %>% ggplot(aes(x=xf_cal, y =xf_cal-xylose_free, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_hline(yintercept = 0)+theme_bw()+
  labs(x="Predicted [Free Xylose] (g/L)",
       y= "Predicted - Measured [Free Xylose] (g/L)"
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
  expand_limits(grouping = c("A: Early/Waste Stream Sampling Locations",
                             "A: Early/Waste Stream Sampling Locations","B: Late Stream Sampling Locations",
                             "B: Late Stream Sampling Locations"),
                x = c(-1, 1, -1, 1), y =  c(-1, 1, -1, 1) )+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")

b
a+b
c <- part1_fullDataSet_training %>% ggplot(aes(x=xylose_free, y =xf_cv, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_abline(slope = 1)+theme_bw()+
  labs(x="Measured [Free Xylose] (g/L)",
       y= "Predicted [Free Xylose] (g/L)",
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
  expand_limits(grouping = c("A: Early/Waste Stream Sampling Locations",
                             "A: Early/Waste Stream Sampling Locations","B: Late Stream Sampling Locations",
                             "B: Late Stream Sampling Locations"),
                x = c(-1, 1.5, -1, 1.5), y =  c(-1, 1.5, -1, 1.5) )+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")
a
d <- part1_fullDataSet_training %>% ggplot(aes(x=xf_cv, y =xf_cv-xylose_free, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_hline(yintercept = 0)+theme_bw()+
  labs(x="Predicted [Free Xylose] (g/L)",
       y= "Predicted - Measured [Free Xylose] (g/L)"
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
  expand_limits(grouping = c("A: Early/Waste Stream Sampling Locations",
                             "A: Early/Waste Stream Sampling Locations","B: Late Stream Sampling Locations",
                             "B: Late Stream Sampling Locations"),
                x = c(-1, 1, -1, 1), y =  c(-1, 1, -1, 1) )+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")

e <- part1_fullDataSet_testing %>% ggplot(aes(x=xylose_free, y =xf_val, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_abline(slope = 1)+theme_bw()+
  labs(x="Measured [Free Xylose] (g/L)",
       y= "Predicted [Free Xylose] (g/L)",
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
  expand_limits(grouping = c("A: Early/Waste Stream Sampling Locations",
                             "A: Early/Waste Stream Sampling Locations","B: Late Stream Sampling Locations",
                             "B: Late Stream Sampling Locations"),
                x = c(-1, 1.5, -1, 1.5), y =  c(-1, 1.5, -1, 1.5) )+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")
a
f<- part1_fullDataSet_testing %>% ggplot(aes(x=xf_val, y =xf_val-xylose_free, col = groupingType))+geom_point(size = 4, alpha = .7)+geom_hline(yintercept = 0)+theme_bw()+
  labs(x="Predicted [Free Xylose] (g/L)",
       y= "Predicted - Measured [Free Xylose] (g/L)"
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
  expand_limits(grouping = c("A: Early/Waste Stream Sampling Locations",
                             "A: Early/Waste Stream Sampling Locations","B: Late Stream Sampling Locations",
                             "B: Late Stream Sampling Locations"),
                x = c(-1, 1, -1, 1), y =  c(-1, 1, -1, 1) )+scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")

g <- (a+b)/(c+d)/(e+f)

h <- g&theme(legend.position = "bottom")
h+plot_layout(guides = "collect")

ggsave("../results/figures/S3.tiff", width  = 180, height = 240, units = "mm", compression = "lzw")
