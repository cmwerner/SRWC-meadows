library(vegan)
library(tidyverse)
library(here)

### Field data --------------
field.data.raw <- read.csv(here("data/SRWC-seedbank_plant-survey_2024-02-26.csv"), header = TRUE)

## Color 
fun.palatte <- c("darkolivegreen","darkgoldenrod")

# removing duplicate rows
dup.1 <- which(field.data.raw$plot == "rf.6.a" & field.data.raw$species.code == "unkfb")[2]
dup.2 <- which(field.data.raw$plot == "rf.4.p" & field.data.raw$species.code == "sidore")[2]
drop <- c(dup.1, dup.2)

field.data <- field.data.raw %>% filter(!row_number() %in% drop)

# switching to wide format for vegan
field.wide <- field.data %>%
pivot_wider(names_from = species.code, values_from = species.count, 
            names_sort = TRUE, values_fill = 0)

view(field.wide)

##Permanova 

# species matrix only
sp.matrix <- field.wide %>% select(abicon:vioadu)

perma.1 <- adonis2(sp.matrix ~ habitat + meadow, 
                    data = field.wide, method ="jaccard")

perma.1



## Code for plotting NMDS

jaccard.dist <- vegdist(sp.matrix, method="jaccard")
nmds <- metaMDS(jaccard.dist, k=2, tol=0.001, maxit=100) # figures out where the points should go

nmdsPlot <- tibble(x=nmds$points[,1], y=nmds$points[,2])
nmdsPlot[,c('meadow','habitat','transect','plot')] <- 
  field.wide[,c('meadow','habitat','transect','plot')]

ggplot(nmdsPlot, aes(x, y, shape=factor(meadow), color=factor(habitat))) +
  geom_point(size=2) + 
  scale_color_manual(values = fun.palatte) +
  theme_classic() +
  xlab('NMDS 1') +
  ylab('NMDS 2')

ggsave(plot = nmdsPlot , filename = "figures/NMDS_species_field.data.png", width = 6, height = 4, units = "in")

## Greenhouse data ---------------
greenhouse.sum.clean 

# using greenhouse.sum.clean data from Rmd file

# switching to wide format for vegan
greenhouse.wide <- greenhouse.sum.clean %>%
  pivot_wider(names_from = species.code, values_from = species.count, 
              names_sort = TRUE, values_fill = 0)

view(field.wide)

##Permanova 

# species matrix only
sp.matrix <- field.wide %>% select(abicon:vioadu)

perma.1 <- adonis2(sp.matrix ~ habitat + meadow, 
                   data = field.wide, method ="jaccard")

perma.1





greenhouse.sum.clean
