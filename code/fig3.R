#code to produce figure 2, plotting total solids vs [xos] by sampling location

#load packages
library(openxlsx)
library(tidyverse)
library(ggfortify)
library(patchwork)
library(ggh4x)
library(ggpmisc)

# set color pallette

pallette  <- c("#000000", "#DDAA33","#004488",  "#BB5566")

#load data

data <- read.xlsx("../data/raw/sampleData.xlsx") %>% 
  mutate(groupingType = factor(groupingType, levels = c("early","waste","late"))) %>% 
  mutate(grouping = factor(grouping, levels = c("early/waste", "late"), 
                           labels =  c("A: Early/Waste Stream Sampling Locations", "B: Late Stream Sampling Locations")))

data %>% 
  ggplot(aes(x=solids, y = xos, color=groupingType)) + 
  geom_point(alpha = .7, size = 4) + 
  theme_bw() + 
  labs(x="total solids(g/L)", y = "XOS(g/L)") +
  facet_wrap(~grouping, scales = "free") +
  geom_smooth(method="lm", se=FALSE, aes(group = grouping, col = NULL), col = "black") +
  stat_poly_eq( formula = y ~ x, 
                aes(group = grouping, label = paste(..eq.label.., stat(rr.label), sep = "~~~")), 
                parse = TRUE, size = 4, col = "black") +
  scale_color_manual(values = c(pallette[2:4]) , name = "Process Sampling Location") + 
  guides(col = guide_legend(nrow = 2)) +
  geom_abline(slope = 1, color = "black") +
  facetted_pos_scales(
    x = list(
      grouping == "A: Early/Waste Stream Sampling Locations" ~ scale_x_continuous(limits = c(0,11)),
      grouping == "B: Late Stream Sampling Locations" ~ scale_x_continuous(limits = c(0,250))
      ),
    y = list(
        grouping == "A: Early/Waste Stream Sampling Locations" ~ scale_y_continuous(limits = c(0,11)),
        grouping == "B: Late Stream Sampling Locations" ~ scale_y_continuous(limits = c(0,250))
  )) +
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
                                        linewidth = 0.5, 
                                        linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5,
                                        linetype = 'solid',
                                        colour = "lightgrey"),
        legend.position = "bottom")

# Save figure as a tiff

ggsave("../results/figures/figure3.tiff", width  = 160, height = 150, units = "mm", compression = "lzw" )  



