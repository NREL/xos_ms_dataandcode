# figure 9 code- predicted vs measured of independent validation for full spectra vs combinational spectra models 


#load packages

library(tidyverse)
library(prospectr)
library(ggfortify)
library(patchwork)
library(ggpmisc) 
pallette  <- c("#000000", "#DDAA33","#004488",  "#BB5566")

#load independent validation data from each model; remove spec1 and spec2 objects, which will mess up pivoting longer 

part1_earlyWasteDataSet_testing <- readRDS("../data/processed/part1_earlyWasteDataSet_testing.RDS") %>% 
  select(!spec1)
part1_lateDataSet_testing <- readRDS("../data/processed/part1_lateDataSet_testing.RDS")%>% 
  select(!spec1)
part2_earlyWasteDataSet_testing <- readRDS("../data/processed/part2_earlyWasteDataSet_testing.RDS")%>% 
  select(!spec1) %>% 
  select(!spec2)
part2_lateDataSet_testing <- readRDS("../data/processed/part2_lateDataSet_testing.RDS")%>% 
  select(!spec1)%>% 
  select(!spec2)

# mutate data to create labels for plotting

data <- 
  part1_earlyWasteDataSet_testing %>% 
  mutate(spectrarange =  "1350-2500nm") %>% 
  rbind(part1_lateDataSet_testing%>% 
          mutate(spectrarange =  "1350-2500nm")) %>% 
  rbind(part2_earlyWasteDataSet_testing %>% 
          mutate(spectrarange =  "2100-2450nm")) %>% 
          rbind(part2_lateDataSet_testing%>% 
                  mutate(spectrarange =  "2100-2450nm"))%>% 
  mutate(grouping = factor(grouping , levels = c("early/waste", "late"), 
                           labels = c("A: Early/Waste Stream Sampling Locations", "B: Late Stream Sampling Locations")))


# make objects for labeling facets/tags

choice1 <- c("A: Early/Waste Stream Sampling Locations" = "A",
            "B: Late Stream Sampling Locations"= "B")

choice2 <- c("A: Early/Waste Stream Sampling Locations" = "C",
             "B: Late Stream Sampling Locations"= "D")

# make a predicted vs measure plot of independent validation data for xos 

a <- data %>%   
  ggplot(aes(x=xos, y =xos_val, col = groupingType,shape = spectrarange)) +
  geom_point(size=4, alpha = .8) +
  geom_abline(slope = 1) +
  theme_bw() +
  labs(x="Measured XOS (g/L)",
       y= "Predicted XOS (g/l)", title = "Early/Waste Stream Sampling Locations             Late Stream Sampling Locations") +
  facet_wrap(~grouping, scales = "free", labeller = labeller(grouping = choice1)) +
  scale_color_manual(values = c(pallette[2:4],pallette[1]), name = "Process Sampling Location") +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
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
        axis.title = element_text(colour = "black",
                                  size =10, 
                                  face =1,
                                  hjust = 0.5),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5, 
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.5,
                                        linetype = 'solid',
                                        colour = "lightgrey"))+
  theme(legend.position = "none")+guides(col = guide_legend(nrow = 2)) +
  scale_shape_manual(values = c(1,19), name = "Model") +
  expand_limits(grouping = c("A: Early/Waste Stream Sampling Locations",
                                 "A: Early/Waste Stream Sampling Locations",
                                 "B: Late Stream Sampling Locations",
                                 "B: Late Stream Sampling Locations"),x = c(0, 10, 0, 250), y =  c(0,10, 0, 250)  )

# make a predicted vs measure plot of independent validation data for total solids

b<- data %>% 
  ggplot(aes(x=solids, y =solids_val, col = groupingType,shape = spectrarange)) +
  geom_point(size=4, alpha = .8) +
  geom_abline(slope = 1) +
  theme_bw() +
  labs(x="Measured total solids (g/L)",
       y= "Predicted total solids (g/l)")+
  facet_wrap(~grouping, scales = "free", labeller = labeller(grouping = choice2)) +
  scale_color_manual(values = c(pallette[2:4], pallette[1]), name = "Process Sampling Location") +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text= element_text(colour = "black",
                                 size =9, 
                                 face =2, 
                                 hjust = 0),
        strip.background = element_blank(),
        legend.key.height = unit(1,"mm"),
        panel.background = element_rect(fill = "white",
                                        colour = "black",
                                        size = 0.5,
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, 
                                        linetype = 'solid',
                                        colour = "lightgrey")) +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(nrow = 2)) +
  scale_shape_manual(values = c(1,19), name = "Model") +
  expand_limits(grouping = c("A: Early/Waste Stream Sampling Locations",
                                 "A: Early/Waste Stream Sampling Locations",
                                 "B: Late Stream Sampling Locations",
                                 "B: Late Stream Sampling Locations"),x = c(0, 10, 0, 250), y =  c(0,10, 0, 250) )

# combine plots

a/b

# save combined plots as a tiff

ggsave("../results/figures/figure9.tiff", width  = 180, height = 200, units = "mm", compression = "lzw" )  

