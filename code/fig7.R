#figure 6 code: independent validation pred vs measured for sample splitting vs not


#load packages

library(tidyverse)
library(prospectr)
library(ggfortify)
library(patchwork)
library(ggpmisc) 

# set color pallette 

pallette  <- c("#000000", "#DDAA33",  "#BB5566","#004488")

#load spectra processing function

source("./commonCode/preprocess_spectra.R")

#load independent validation data from each model

part1_fullDataSet_testing <- readRDS("../data/processed/part1_fullDataSet_testing.RDS")
part1_earlyWasteDataSet_testing <- readRDS("../data/processed/part1_earlyWasteDataSet_testing.RDS")
part1_lateDataSet_testing <- readRDS("../data/processed/part1_lateDataSet_testing.RDS")

# combine data, add labels to results for plotting

data <- part1_fullDataSet_testing %>% 
  mutate(modeltype = "combined") %>% 
  rbind(part1_earlyWasteDataSet_testing %>% 
          mutate(modeltype = "location split")) %>% 
  rbind(part1_lateDataSet_testing %>% 
          mutate(modeltype = "location split")) %>% 
mutate(grouping = factor(grouping , levels = c("early/waste", "late"), labels = c("A: Early/Waste Stream Sampling Locations", "B: Late Stream Sampling Locations"))) %>% 
mutate(groupingType = factor(groupingType, levels = c("early","waste","late")))

# create facet labels for first plot

choice <- c("A: Early/Waste Stream Sampling Locations" = "A",
            "B: Late Stream Sampling Locations"= "B")

# plot predicted vs measured xos

a <- data %>% ggplot(aes(x=xos, y =xos_val, col = groupingType, shape = modeltype)) +
  geom_point(size = 4, alpha = .8) +
  geom_abline(slope = 1) +
  theme_bw() +
  labs(x="Measured XOS (g/L)",
       y= "Predicted XOS (g/L)",
       title = "Early/Waste Stream Sampling Locations             Late Stream Sampling Locations") +
  facet_wrap(~grouping, scales="free", labeller = labeller(grouping = choice)) +
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        strip.text= element_text(colour = "black", 
                                 size =9, 
                                 face =2, 
                                 hjust = 0),
        strip.background = element_blank(),
        legend.key.height = unit(1,"mm"),
        title = element_text(colour = "black", 
                             size =9.5,
                             face =2, 
                             hjust = 0.5),
        panel.background = element_rect(fill = "white",
                                        colour = "black", 
                                        size = 0.5, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.5,
                                        linetype = 'solid',
                                        colour = "lightgrey")) +
  theme(legend.position = "none") +
  scale_shape_manual(values = c(1,19), name = "Model") +
  expand_limits(grouping = c("A: Early/Waste Stream Sampling Locations",
                                 "A: Early/Waste Stream Sampling Locations","B: Late Stream Sampling Locations",
                                 "B: Late Stream Sampling Locations"),
                x = c(0, 10, 0, 250), y =  c(0, 10, 0, 250) ) +
  scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")


# create facet labels for second plot

choice2 <- c("A: Early/Waste Stream Sampling Locations" = "C",
            "B: Late Stream Sampling Locations"= "D")

# plot predicted vs measures total solids

b <-data %>%
  ggplot(aes(x=solids, y =solids_val, col = groupingType, shape = modeltype)) +
  geom_point(size = 4, alpha = .8) +
  geom_abline(slope = 1) +
  theme_bw() +
  labs(x="Measured total solids (g/L)",
       y= "Predicted total solids (g/L)") +
  facet_wrap(~grouping, scales="free", labeller = labeller(grouping = choice2)) +
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        strip.text= element_text(colour = "black",
                                 size =9,
                                 face =2, 
                                 hjust = 0),
        strip.background = element_blank(),
        legend.key.height = unit(1,"mm"),
        title = element_text(colour = "black",
                             size =9.5, 
                             face =2,
                             hjust = 0.5),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.5,
                                        linetype = 'solid', 
                                        colour = "lightgrey")) +
  theme(legend.position = "bottom") +
  scale_shape_manual(values = c(1,19), name = "Model") +
  expand_limits(grouping = c("A: Early/Waste Stream Sampling Locations",
                                 "A: Early/Waste Stream Sampling Locations","B: Late Stream Sampling Locations",
                                 "B: Late Stream Sampling Locations"),
                x = c(0, 10, 0, 250), y =  c(0, 10, 0, 250) ) +
  scale_color_manual(values = pallette[2:4], name = "Process Sampling Location")

#combine plots

a/b

# save plots as a tiff

ggsave("../results/figures/figure7.tiff", width  = 180, height = 200, units = "mm", compression = "lzw")

