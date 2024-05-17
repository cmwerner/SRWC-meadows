library(vegan)
library(tidyverse)

field.wide <- field.data %>%
pivot_wider(names_from = species.code, values_from = species.count)


##Permanova 

perma.1 <- adonis2(#plant community matrix #selecting only plant colloms ~ habitait + medow, 
  #data = wide format data, method = "accard")

summary(perma.1)