source("loadingData.R")

library(ggplot2)

# As mentioned in the Graph
biome <- c("Tropical moist","Tropical dry","Temperate mixed","Temperate coniferous","Boreal forest",
  "Temperate savannas","Flooded savannas","Montane grasslands","Tundra","Mediterranean",
  "Xeric shrublands")

# Stringsplit for RegEx
strings <- as.data.frame(strsplit(biome," "))
strings

# Binome name for aggregate
# Using Regular Expressions to determine Binome name for aggregation
# I am not spending time on looping this procedure...
plot.merge.ecoregion$Binome.name2 <- ""
plot.merge.ecoregion[grep(paste0(strings[1,1],".","+",strings[2,1]), plot.merge.ecoregion$Biome.name, ignore.case = T),]$Binome.name2 <- "Tropical moist"
plot.merge.ecoregion[grep(paste0(strings[1,2],".","+",strings[2,2]), plot.merge.ecoregion$Biome.name, ignore.case = T),]$Binome.name2 <- "Tropical dry"
plot.merge.ecoregion[grep(paste0(strings[1,3],".","+",strings[2,3]), plot.merge.ecoregion$Biome.name, ignore.case = T),]$Binome.name2 <- "Temperate mixed"
plot.merge.ecoregion[grep(paste0(strings[1,4],".","+",strings[2,4]), plot.merge.ecoregion$Biome.name, ignore.case = T),]$Binome.name2 <- "Temperate coniferous"
plot.merge.ecoregion[grep(paste0(strings[1,5],".","+",strings[2,5]), plot.merge.ecoregion$Biome.name, ignore.case = T),]$Binome.name2 <- "Boreal forest"
plot.merge.ecoregion[grep(paste0(strings[1,6],".","+",strings[2,6]), plot.merge.ecoregion$Biome.name, ignore.case = T),]$Binome.name2 <-  "Temperate savannas"
plot.merge.ecoregion[grep(paste0(strings[1,7],".","+",strings[2,7]), plot.merge.ecoregion$Biome.name, ignore.case = T),]$Binome.name2 <- "Flooded savannas"
plot.merge.ecoregion[grep(paste0(strings[1,8],".","+",strings[2,8]), plot.merge.ecoregion$Biome.name, ignore.case = T),]$Binome.name2 <- "Montane grasslands"
plot.merge.ecoregion[grep(paste0(strings[1,9]), plot.merge.ecoregion$Biome.name, ignore.case = T),]$Binome.name2 <- "Tundra"
plot.merge.ecoregion[grep(paste0(strings[1,10]), plot.merge.ecoregion$Biome.name, ignore.case = T),]$Binome.name2 <- "Mediterranean"
plot.merge.ecoregion[grep(paste0(strings[1,11],".","+",strings[2,11]), plot.merge.ecoregion$Biome.name, ignore.case = T),]$Binome.name2 <- "Xeric shrublands"


# Biomass has 4 components as mentioned in page 1
# live trees
# understory
# green forest floor
# coarse woody debris

# Generating above ground biomass variable
plot.merge.ecoregion$Biomass.above <- plot.merge.ecoregion$Stem...bark + plot.merge.ecoregion$Bark + plot.merge.ecoregion$Wood...bark.of.crown 
  + plot.merge.ecoregion$Leaves...needles + plot.merge.ecoregion$Stump...Roots # Living trees 
  + plot.merge.ecoregion$Undergrowth...Bushes # Understory 
  + plot.merge.ecoregion$Green.forest.floor # Green forest floor 
  + plot.merge.ecoregion$Snags...logs + plot.merge.ecoregion$Dead.branches # coarse woody debris

summary(plot.merge.ecoregion$Biomass.above)



ggplot(data = plot.merge.ecoregion) +
  geom_jitter(aes(plot.merge.ecoregion$Binome.name2, plot.merge.ecoregion$Biomass.above),
              width = 0.2,
              alpha = 0.05) +
  geom_boxplot(aes(plot.merge.ecoregion$Binome.name2, plot.merge.ecoregion$Biomass.above), 
               fill = 1:length(biome),
               alpha = 0.5,
               na.rm = T,
               show.legend = T) +
  stat_summary(aes(plot.merge.ecoregion$Binome.name2, plot.merge.ecoregion$Biomass.above),
               fun.y = median, geom = "point", 
               position = position_dodge(width = .9),
               size = 1.5, shape = 2,
               show.legend = F,
               colour = "red") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=90),plot.title = element_text(hjust = 0.5)) +
  labs(x = "Ecoregion", y = "Above Ground Biomass, t/ha", title = "Replication of Figure 3") +
  ylim(0,600)


