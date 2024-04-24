actual.v.pot.sum <- combined.summary %>% 
  group_by(habitat) %>% 
  dplyr::summarise(
    field.rich.mean = mean(field.richness, na.rm = TRUE),
    green.rich.mean = mean(greenhouse.richness, na.rm = TRUE),
    field.rich.sd = sd(field.richness, na.rm = TRUE),
    green.rich.sd = sd(greenhouse.richness, na.rm = TRUE),
    n = length(field.richness))
View(actual.v.pot.sum)
