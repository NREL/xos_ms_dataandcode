# code to produce figure 2 which shows the wet chemistry distribution 

#load packages
library(tidyverse)
library(openxlsx)
library(ggfortify)
library(patchwork)
library(ggpmisc) 


pallette  <- c("#000000", "#DDAA33",  "#BB5566","#004488")


#load data

data <- read.xlsx("../data/raw/sampleData.xlsx") %>% 
mutate(groupingType = factor(groupingType, levels = c("early","waste","late")))

# XOS distribution scatter plot

a <- data %>%
  ggplot(aes( y = xos,x = groupingType, col = groupingType)) +
  geom_jitter(width = .2, alpha = .7, size =2) +
  labs(x= "Sampling Location", y = "XOS (g/L)", tag = "A") +
  theme_bw() +
  scale_color_manual(values = c(pallette[2:4], pallette[1]), name = "Process Sampling Location")+
  theme(legend.position = "bottom",
        legend.justification = c("left","top"), 
        legend.text = element_text(size = 10),
        legend.title  = element_text(size = 10),
        legend.key.size = unit(1.22, "mm"),
        legend.spacing.y = unit(.2, "mm"),
        axis.title = element_text(size =8), 
        axis.text = element_text(size = 8))

# Monomeric distribution scatter plot

b <- data %>%
  ggplot(aes( y = xylose_free,x = groupingType, col =  groupingType)) +
  geom_jitter(width = .2, alpha = .7, size= 2) +
  labs(x= "Sampling Location", y = "Monomeric Xylose (g/L)", tag = "B") +
  theme_bw() +
  scale_color_manual(values = c(pallette[2:4], pallette[1]), name = "Process Sampling Location") +
  theme(legend.position = "bottom",
        legend.justification = c("left","top"), 
        legend.text = element_text(size = 10),
        legend.title  = element_text(size = 10),
        legend.key.size = unit(1.22, "mm"),
        legend.spacing.y = unit(.2, "mm"),
        axis.title = element_text(size =8), 
        axis.text = element_text(size = 8))

# Total solids distribution Scatter plot

c <- data %>%  
  ggplot(aes( y = solids,x = groupingType, col =  groupingType)) +
  geom_jitter(width = .2, alpha = .7, size = 2) +
  labs(x= "Sampling Location", y = "Total Solids (g/L)", tag = "C") +
  theme_bw() +
  scale_color_manual(values = c(pallette[2:4], pallette[1]), name = "Process Sampling Location") +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 10),
        legend.title  = element_text(size = 10),
        legend.key.size = unit(1.22, "mm"),
        legend.spacing.y = unit(.2, "mm"),
        axis.title = element_text(size =8), 
        axis.text = element_text(size = 8))

# Patch all three plots together

(((a+b+c)& 
    theme(legend.position = "bottom")) + 
    plot_layout(guides = "collect"))

# Save figure as a tiff

ggsave("../results/figures/figure2.tiff", width  = 170, height = 85, units = "mm", compression = "lzw")

