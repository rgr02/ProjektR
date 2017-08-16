source("loadingData.R")

library(dplyr)
library(ggplot2)

# no missing values
sum(is.na(ecoregion$Biome.name.plot))

table4 <- select(ecoregion, Biome.name.plot, n_Plots, n_Trees) %>%
  group_by(Biome.name.plot) %>% summarise(Plots = sum(n_Plots, na.rm = T),
                                       Trees = sum(n_Trees, na.rm = T))

View(table4)
write.csv2(table4, file = "table4.csv")

ggplot(data = table4) +
  geom_col(aes(table4$Biome.name.plot, table4$Plots), alpha = 0.3,
           colour = table4$Biome.name.plot, size = 1.2) +
  geom_col(aes(table4$Biome.name.plot, table4$Trees), alpha = 0.3,
           colour = table4$Biome.name.plot, linetype = "dotted",
           size = 1.2) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=270, hjust=1, size = 10,
                                 face = "bold", family = "Arial"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "Biome", y = "Nr of Counts", title = "Visualisation of Table 4")
  
