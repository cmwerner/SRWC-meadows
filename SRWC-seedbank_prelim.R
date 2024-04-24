library(tidyverse)
library(stringr)
library(ggplot2)

# To Do: Change Colors
color.palatte <- c("goldenrod", "darkorchid4")

# To Do: Add remaining plots to ground cover data sheet, re-save as csv
# Also update column names to remove (%) from column names 
# and update all plot names to match vegetation data sheets

ground.cover <- read.csv('SRWC-seedbank_ground-cover_2023-07-21.csv')

View(ground.cover)
plot.type <- ground.cover$plot %>% 
  str_sub(-1)

ground.cover$plot.type <- ifelse(plot.type == "A", "Actual", "Potential") # change to "a" after updating datasheet

ground.summary <- ground.cover %>% 
  group_by(plot.type) %>%
  dplyr::summarise(
    veg.cover.mean = mean(herbaceous....), # change column name after updating datasheet
    veg.cover.se = sd(herbaceous....)/sqrt(length(herbaceous....)), # change column name after updating datasheet
  )

ground.summary %>% 
  ggplot(aes(x = plot.type, y = veg.cover.mean, 
             ymin = veg.cover.mean - veg.cover.se, ymax = veg.cover.mean + veg.cover.se,
             color = plot.type)) + 
  geom_point(size = 3, col = color.palatte) +
  geom_errorbar(width = 0.5, col = color.palatte) +
  theme_classic() +
  theme(axis.text = element_text(size = 12), text = element_text(size = 12)) +
  xlab('Plot type') +
  ylab('Herbaceous cover')

ggsave('SRWC-seedbank_veg-cover.pdf', width = 7, height = 5, units = 'in')



veg.cover <- read.csv('SRWC-seedbank_plant-survey_2023-07-21.csv')
plot.type <- veg.cover$plot %>% 
  str_sub(-1)

veg.cover$plot.type <- ifelse(plot.type == "a", "Actual", "Potential")

View(veg.cover)

veg.cover.summary <- veg.cover %>% 
  group_by(plot, plot.type) %>% 
  dplyr::summarise(
    species.richness = length(unique(species.code)) 
  )

View(veg.cover.summary)

veg.summary.2 <- veg.cover.summary %>% 
  group_by(plot.type) %>%
  dplyr::summarise(
    richness.mean = mean(species.richness),
    richness.se = sd(species.richness)/sqrt(length(species.richness)),
  )

veg.summary.2 %>% 
  ggplot(aes(x = plot.type, y = richness.mean, 
             ymin = richness.mean - richness.se, ymax = richness.mean + richness.se,
             color = plot.type)) + 
  geom_point(size = 3, col = color.palatte) +
  geom_errorbar(width = 0.5, col = color.palatte) +
  theme_classic() +
  theme(axis.text = element_text(size = 12), text = element_text(size = 12)) +
  xlab('Plot type') +
  ylab('Species richness')

ggsave('SRWC-seedbank_richness.pdf', width = 7, height = 5, units = 'in')
