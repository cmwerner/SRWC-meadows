library(vegan)
library(tidyverse)
library(here)

field.data <- read.csv(here("data/SRWC-seedbank_plant-survey_2024-02-26.csv"), header = TRUE)

field.wide <- field.data %>%
pivot_wider(values_fill = 0, names_from = species.code, values_from = species.count)

view(field.wide)

##Permanova 

##perma.1 <- adonis2(#plant community matrix #selecting only plant colloms ~ habitait + medow, 
  #data = wide format data, method = "jaccard")

perma.1 <- adonis2(select(darcal:dancal) ~ habitat + meadow, 
                    data = field.wide, method ="jaccard")

summary(perma.1)


##Code for plotting:
 ## jaccard.dist <- vegdist(*whatever you called your just species matrix*, method=‘jaccard’)
## nmds <- metaMDS(jaccard.dist, k=2, tol=0.001, maxit=50)
## plot(nmds, type=‘n’)
## points(speciesmatrix, pch=19, col=colvec[data$habitat])
## ordiellipse(speciesmatrix, data$habitat)