rm(list = ls())
library(tidyverse)
library(lubridate)
library(dplyr)
library(stats)
library(rstatix)
#install.packages("dunn.test")
library(dunn.test)


Cattle_Impact <- read.csv("SRWC_Cattle-Impacts_Cleaned-Small.csv")  %>%
  mutate(
    Restoration_Location_2 = case_when(
    Restoration_Location %in% c("Both", "Inside", "Downstream") ~ "in_or_downstream", 
    TRUE ~ "none_or_upstream"),
    
    HGM = str_trim(HGM),
    
    HGM_2 = case_when(
      HGM %in% c("Riparian_Mid_Gradient", "Riparian_High_Gradient") ~ "Riparian",
      HGM %in% c("Subsurface_Low_Gradient", "Subsurface_Mid_Gradient", "Subsurface_High_Gradient") ~ "Subsurface",
      TRUE ~ HGM),
      
    Hoof_Punch_Present = case_when(
      Percent_Hoof_Punch > 0 ~ "yes",
      TRUE ~ "no")
      
    )

view(Cattle_Impact)

### Analyses and Plots start here-----------------------------

# create a vector for our colors
col.vec <- c("yellow3", "yellow3", "skyblue", "chocolate", "tomato", "darkgoldenrod",
             "royalblue4", "turquoise", "turquoise", "magenta", "magenta", "magenta")

#scatterplot of precent_trample and Closest water distance 
Cattle_Impact %>% 
  ggplot(aes(x = Closest_Water_Distance, y = Percent_Trample, color = Watershed)) +
  geom_point() +
  theme_bw() # no grey background

#massive mess of cattle location preference
pairs(~Percent_Trample+Closest_Water_Distance, data=SRWC_Cattle_Impacts_Cleaned_Small, main="Title") #scatter plot looking at trampling and closest water distance
pairs(~Percent_Trample+Percent_Hoof_Punch+Percent_DC_Grazed+Manure_Count+Percent_Canopy+Percent_Herb+Percent_Shrub+Percent_Bare+Percent_Litter+Percent_Rock+Percent_Woody+Percent_Water+Percent_DC, data=Cattle_Impact, main="Cattle Location Preference") #Matrix of scatter plots

#Scatterplot of closest water distance and precent tarmple 
Cattle_Impact %>% # come back to these maybe at a later time
  ggplot(aes(x = Closest_Water_Distance, y = Percent_Trample)) +
  geom_point() +
  theme_bw()

#Scatterplot of closest water distance and hoof punch 
SRWC_Cattle_Impacts_Cleaned_Small %>% # come back to these maybe at a later time
  ggplot(aes(x = Closest_Water_Distance, y = Percent_Hoof_Punch)) +
  geom_point() +
  theme_bw()

#scatterplot of closest water distrance and percent Dc grazed 
SRWC_Cattle_Impacts_Cleaned_Small %>% # come back to these maybe at a later time
  ggplot(aes(x = Closest_Water_Distance, y = Percent_DC_Grazed)) +
  geom_point() +
  theme_bw()

#scatterplot of closest water distance and manure count 
SRWC_Cattle_Impacts_Cleaned_Small %>% # come back to these maybe at a later time
  ggplot(aes(x = Closest_Water_Distance, y = Manure_Count, color = HGM)) +
  geom_point() +
  theme_bw()

#Scatterplot of hoof_punch and precent_DC

Cattle_Impact %>% 
  ggplot(aes(x = Percent_DC, y = Percent_Hoof_Punch, color = HGM)) +
  geom_point(position = position_jitter()) +   
  theme_bw() +
  scale_color_manual(values = col.vec)

# hypothesis1: more trample in plots with restoration within or downstream of plot

Cattle_Impact %>% 
  ggplot(aes(x = Restoration_Location_2, y = Percent_Trample, fill = Restoration_Location_2)) +
  geom_boxplot() +
  theme_bw()

m.trample.restoration <- aov(Percent_Trample ~ Restoration_Location_2, data = Cattle_Impact)
summary(m.trample.restoration)

RLHoofPunch <- kruskal.test(Percent_Trample ~ Restoration_Location_2, data=Cattle_Impact)
print(RLHoofPunch)

wilcox.test(Percent_Trample ~ Restoration_Location_2, data = Cattle_Impact, exact = FALSE)

summary_stats <- Cattle_Impact %>%
  group_by(Restoration_Location_2) %>%
  summarise(
    count = n(),
    median = median(Percent_Trample, na.rm = TRUE),
    IQR = IQR(Percent_Trample, na.rm = TRUE)
  )
print(summary_stats)

# trying to find SD for trampling 

sigma(m.trample.restoration)

# trampling isn't normally distributed, it's zero-inflated

Cattle_Impact %>%
  ggplot(aes(x = Percent_Trample)) +
  geom_histogram() +
  theme_bw()

# hypothesis1 more hoof punch in plots with restoration within or downstream of plot

Cattle_Impact %>% 
  ggplot(aes(x = Restoration_Location_2, y = Percent_Hoof_Punch, fill = Restoration_Location_2)) +
  geom_boxplot() +
  theme_bw()

m.hoof.restoration <- aov(Percent_Hoof_Punch ~ Restoration_Location_2, data = Cattle_Impact)
summary(m.hoof.restoration)

KHGMHoofPunchRestoration <- kruskal.test(Percent_Hoof_Punch ~ Restoration_Location_2, data=Cattle_Impact)
print(KHGMHoofPunchRestoration)

dunn_result <- dunn.test(x = Cattle_Impact$Percent_Hoof_Punch, g = Cattle_Impact$Restoration_Location_2, method = "bh") # incorrect bc I don't have enough dimensions 
print(dunn_result)

wilcox.test(Percent_Hoof_Punch ~ Restoration_Location_2, data = Cattle_Impact, exact = FALSE)

summary_stats <- Cattle_Impact %>%
  group_by(Restoration_Location_2) %>%
  summarise(
    count = n(),
    median = median(Percent_Hoof_Punch, na.rm = TRUE),
    IQR = IQR(Percent_Hoof_Punch, na.rm = TRUE)
  )
print(summary_stats)


# Hoof Punch isn't normally distributed, it's zero-inflated
Cattle_Impact %>%
  ggplot(aes(x = Percent_Hoof_Punch)) +
  geom_histogram() +
  theme_bw()

# hypothesis1: more manure in plots with restoration within or downstream of plot

Cattle_Impact %>% 
  ggplot(aes(x = Restoration_Location_2, y = Manure_Count, fill = Restoration_Location_2)) +
  geom_boxplot() +
  theme_bw()

m.manure.restoration <- aov(Manure_Count ~ Restoration_Location_2, data = Cattle_Impact)
summary(m.manure.restoration)

KHGMHoofPunchRestoration <- kruskal.test(Manure_Count ~ Restoration_Location_2, data=Cattle_Impact)
print(KHGMHoofPunchRestoration)

wilcox.test(Manure_Count ~ Restoration_Location_2, data = Cattle_Impact, exact = FALSE)

summary_stats <- Cattle_Impact %>%
  group_by(Restoration_Location_2) %>%
  summarise(
    count = n(),
    median = median(Manure_Count, na.rm = TRUE),
    IQR = IQR(Manure_Count, na.rm = TRUE)
  )
print(summary_stats)

# Manure isn't normally distributed, it's zero-inflated

Cattle_Impact %>%
  ggplot(aes(x = Manure_Count)) +
  geom_histogram() +
  theme_bw()

# hypothesis1: more DC Grazing in plots with restoration within or downstream of plot

Cattle_Impact %>% 
  ggplot(aes(x = Restoration_Location_2, y = Percent_DC_Grazed, fill = Restoration_Location_2)) +
  geom_boxplot() +
  theme_bw() +labs(y= "Percent of DC Grazed", x = "Restoration Location")

m.DCGrazed.restoration <- aov(Percent_DC_Grazed ~ Restoration_Location_2, data = Cattle_Impact)
summary(m.DCGrazed.restoration)

KHGMHoofPunchRestoration <- kruskal.test(Percent_DC_Grazed ~ Restoration_Location_2, data=Cattle_Impact)
print(KHGMHoofPunchRestoration)

wilcox.test(Percent_DC_Grazed ~ Restoration_Location_2, data = Cattle_Impact, exact = FALSE)

summary_stats <- Cattle_Impact %>%
  group_by(Restoration_Location_2) %>%
  summarise(
    count = n(),
    median = median(Percent_DC_Grazed, na.rm = TRUE),
    IQR = IQR(Percent_DC_Grazed, na.rm = TRUE)
  )
print(summary_stats)

# DC Grazing isn't normally distributed, it's zero-inflated

Cattle_Impact %>%
  ggplot(aes(x = Percent_DC_Grazed)) +
  geom_histogram() +
  theme_bw()

# hypothesis1: more Grazing in plots with restoration within or downstream of plot

Cattle_Impact %>% # This populated a boxplot but wasn't helpful we dont know wut happened here?
  ggplot(aes(x = Restoration_Location_2, y = Grazing_Intensity, fill = Restoration_Location_2)) +
  geom_boxplot() +
  theme_bw()

KHGMHoofPunchRestoration <- kruskal.test(Grazing_Intensity ~ Restoration_Location_2, data=Cattle_Impact)
print(KHGMHoofPunchRestoration)

# hypothesis2:Plots within meadows with certain hydrogeomorphic (HGM) type classifications will have an increase in Hoof Punch

KHGMHoofPunch <- kruskal.test(Percent_Hoof_Punch ~ HGM_2, data=Cattle_Impact)
print(KHGMHoofPunch)

dunn_result <- dunn_test(Cattle_Impact, Percent_Hoof_Punch ~ HGM_2, p.adjust.method = "fdr")
print(dunn_result)

Cattle_Impact %>% 
  mutate(HGM_2 = gsub("_", " ", HGM_2)) %>%
  ggplot(aes(x = HGM_2, y = Percent_Hoof_Punch, fill = HGM_2)) +
  geom_boxplot() +  
  theme_classic() +
  theme (axis.text.x = element_text(angle = 45,hjust = 1, vjust = 1))+
  labs(y= "Hoof Punch", x = "Meadow HGM Type") +  #how to make the horizontal X-axis labels slanted and change axis names 
  scale_fill_discrete(name = "HGM Type") #legend title


# hypothesis2: Plots within meadows with certain hydrogeomorphic (HGM) type classifications will have an increase in trampling 

KHGMTrample <- kruskal.test(Percent_Trample ~ HGM_2, data=Cattle_Impact)
print(KHGMTrample)

dunn_result <- dunn_test(Cattle_Impact, Percent_Trample ~ HGM_2, p.adjust.method = "holm")
print(dunn_result)

Cattle_Impact %>% 
  mutate(HGM_2 = gsub("_", " ", HGM_2)) %>%
  ggplot(aes(x = HGM_2, y = Percent_Trample, fill = HGM_2)) +
  geom_boxplot() +
  theme_classic() +
  theme (axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +labs(y= "Trampling", x = "Meadow HGM Type") +  #how to make the horizontal X-axis labels slanted and change axis names
  scale_fill_discrete(name = "HGM Type") #legend title

#hypothesis2: plots WITHING CERTAIN MEADOW TYPES AND manure count 

HGMManure <- aov(Manure_Count~HGM_2, data=Cattle_Impact)
summary(HGMManure)

Cattle_Impact %>% 
  mutate(HGM_2 = gsub("_", " ", HGM_2)) %>%
  ggplot(aes(x = HGM_2, y = Manure_Count, fill = HGM_2)) +
  geom_boxplot() +
  theme_classic() +
  theme (axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +labs(y= "Manure", x = "Meadow HGM Type") +  #how to make the horizontal X-axis labels slanted and change axis names
  scale_fill_discrete(name = "HGM Type") #legend title

KHGMManure <- kruskal.test(Manure_Count ~ HGM_2, data=Cattle_Impact)
print(KHGMManure)

dunn_result <- dunn_test(Cattle_Impact, Manure_Count ~ HGM_2, p.adjust.method = "holm")
print(dunn_result)

# hypothesis3: Darlingtonia presence has a positive correlation to precent hoof punch 

Cattle_Impact %>% 
  ggplot(aes(x = Hoof_Punch_Present, y = Percent_DC, fill = Hoof_Punch_Present)) +
  geom_boxplot() +
  theme_bw() +
  theme_bw() +labs(y= "Percent DC", x = "Hoof Punch")

Cattle_Impact %>%
  ggplot(aes(x = Percent_Hoof_Punch, y = Percent_DC,)) + 
  geom_point(position = position_jitter(0.3), alpha = 0.6, aes(color = HGM_2)) +
  geom_smooth(method = "lm", formula = (y ~ exp(-x)), color = 'darkblue', se = FALSE) +
  geom_smooth(method = "lm", color = 'grey50', linetype = 2) +
  theme_bw()

# only where DC is present
Cattle_Impact %>%
  filter(Percent_DC > 0) %>%
  ggplot(aes(x = Percent_Hoof_Punch, y = Percent_DC,)) + 
  geom_point(position = position_jitter(0.3), alpha = 0.6, aes(color = HGM_2)) +
  geom_smooth(method = "lm", color = 'grey50', linetype = 2) +
  theme_bw()

m.hoof.DC <- aov(Percent_DC ~ Hoof_Punch_Present, data = Cattle_Impact)
summary(m.hoof.DC)

KHGMHoofDC <- kruskal.test(Percent_DC ~ Hoof_Punch_Present, data=Cattle_Impact)
print(KHGMHoofDC)

# hypothesis3: Darlingtonia presence has a negative correlation to manure count (not significant)

Cattle_Impact %>%
  ggplot(aes(x = Manure_Count, y = Percent_DC,)) + 
  geom_point(position = position_jitter(0.3), alpha = 0.6, aes(color = HGM_2)) +
  geom_smooth(method = "lm", formula = (y ~ exp(-x)), color = 'darkblue', se = FALSE) +
  geom_smooth(method = "lm", color = 'grey50', se = FALSE, linetype = 2) +
  theme_bw()

# only in plots where DC is present
Cattle_Impact %>%
  filter(Percent_DC > 0) %>% 
  ggplot(aes(x = Manure_Count, y = Percent_DC,)) + 
  geom_point(position = position_jitter(0.3), alpha = 0.6, aes(color = HGM_2)) +
  geom_smooth(method = "lm", color = 'grey50', linetype = 2) +
  theme_bw()

KHGMManureDC <- kruskal.test(Percent_DC ~ Manure_Count, data=Cattle_Impact)
print(KHGMManureDC)


#Question:Where there is a high amount of grazing is there more DC grazed?  

Cattle_Impact %>% 
  ggplot(aes(x = Grazing_Intensity, y = Percent_DC_Grazed, fill = Grazing_Intensity)) +
  geom_boxplot() +
  theme_bw() +labs(y= "Percent DC Grazed", x = "Grazing Intensity")

m.Grazing.DC <- aov(Percent_DC_Grazed ~ Grazing_Intensity, data = Cattle_Impact)
summary(m.Grazing.DC)

KHGMManure <- kruskal.test(Percent_DC_Grazed ~ Grazing_Intensity, data=Cattle_Impact)
print(KHGMManure)

#question: looking at how many restoration structures are present up and downstream 

Cattle_Impact.chi <- chisq.test(Cattle_Impact) 

contingency_table <-table(Cattle_Impact$Incised, Cattle_Impact$Restoration_Location_2)
print(contingency_table)

contingency_table <-table(Cattle_Impact$Bank_Sloughing, Cattle_Impact$Restoration_Location_2)
print(contingency_table)

write.csv(Cattle_Impact, file = "output.csv", row.names = FALSE)
getwd()

# Fixing spelling 

Cattle_Impact[Cattle_Impact == "Depressional_Perenial"] <- "Depressional_Perennial"

# trying to get SD 

sd(m.trample.restoration)

Cattle_Impact %>%
  group_by(Percent_Trample) %>% # Group by the 'Zone' variable
  summarise(
    mean_CI = mean(MaxCI, na.rm = TRUE), # Calculate mean
    sd_CI = sd(MaxCI, na.rm = TRUE), # Calculate standard deviation
    min_CI = min(MaxCI, na.rm = TRUE), # Calculate minimum
    max_CI = max(MaxCI, na.rm = TRUE), # Calculate maximum
    n_count = n(), # Count the number of observations in each group
    .groups = "drop")

#shapiro.test on all cattle impacts 

shapiro.test(Cattle_Impact$Percent_Trample)

shapiro.test(Cattle_Impact$Percent_Hoof_Punch)

shapiro.test(Cattle_Impact$Manure_Count)

shapiro.test(Cattle_Impact$Percent_DC_Grazed)

#Hoof punch is most likely to occur when there’s water <5m away from the plot. 

Cattle_Impact %>%
  ggplot(aes(x = Closest_Water_Distance, y = Percent_Hoof_Punch)) + 
  geom_line()

KHGMWaterHoof <- kruskal.test(Closest_Water_Distance ~ Percent_Hoof_Punch, data=Cattle_Impact) #would this work here? probz not 
print(KHGMWaterHoof)

#hoof punch and DC presence - line graph (I kind of already tested this with the box and whisker plot huh :/ )

Cattle_Impact %>%
  ggplot(aes(x = Percent_Hoof_Punch, y = Percent_DC)) + 
  geom_line()

KHGMDCHoof <- kruskal.test(Percent_Hoof_Punch ~ Percent_DC, data=Cattle_Impact) #I kind of already tested this with the box and whisker plot huh :/ 
print(KHGMDCHoof)

#stream restoration type and incised streams (confusing maybe this doesn't work?)

Cattle_Impact %>% 
  ggplot(aes(x = Stream_Restoration_Type, y = Incised, fill = Stream_Restoration_Type)) +
  geom_boxplot() +
  theme_classic() +
  theme (axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +labs(y= "Incised", x = "Stream Restoration type") +  #how to make the horizontal X-axis labels slanted and change axis names
  scale_fill_discrete(name = "Stream Restoration") #legend title
