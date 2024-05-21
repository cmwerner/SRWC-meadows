library(vegan)
library(tidyverse)
library(here)

field.data <- read.csv(here("data/SRWC-seedbank_plant-survey_2024-02-26.csv"), header = TRUE)

# removing duplicate rows
dup.1 <- which(field.data$plot == "rf.6.a" & field.data$species.code == "unkfb")[2]
dup.2 <- which(field.data$plot == "rf.4.p" & field.data$species.code == "sidore")[2]
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



##Code for plotting:
 ## jaccard.dist <- vegdist(*whatever you called your just species matrix*, method=‘jaccard’)
## nmds <- metaMDS(jaccard.dist, k=2, tol=0.001, maxit=50)
## plot(nmds, type=‘n’)
## points(speciesmatrix, pch=19, col=colvec[data$habitat])
## ordiellipse(speciesmatrix, data$habitat)