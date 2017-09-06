# Clear workspace
rm(list = ls())
setwd("C:/Users/Rudi/Dropbox/RCoding/ProjektR")

library(dplyr)

# 9685 + 9686 have additional column entries after Reference -> deletion
# Na rows at the end of the set. 
plot.db <- read.csv2("Biomass_plot_DB.csv", header = T)
plot.db <- plot.db[,1:44]
plot.db <- plot.db[which(!is.na(plot.db$ID)),]
head(plot.db)


# Na's at the end
tree.db <- read.csv2("Biomass_tree_DB.csv", header = T)
tree.db <- tree.db[which(!is.na(tree.db$ID)),]

sum(is.na(tree.db$ID_Plot))
sum(is.na(plot.db$ID))

# How many plot id's are in tree but not in plot dataset vice versa
sum(!(tree.db$ID_Plot %in% plot.db$ID)) 
sum((plot.db$ID %in% tree.db$ID_Plot)) 

# All = true equals full outer join because of many non matching id's
db.merge <- merge(tree.db, plot.db, 
              by.x = "ID_Plot", by.y = "ID",
              all.y = T, 
              suffixes = c(".tree", ".plot"))

db.merge2 <- merge(tree.db, plot.db, 
                   by.x = "ID_Plot", by.y = "ID",
                   suffixes = c(".tree", ".plot"))


# Generating columns for common variables in plot and tree

db.merge$Species2 <- db.merge$Species
ifelse(is.na(db.merge$Tree.species), db.merge$Species <- db.merge$Species, db.merge$Species <- db.merge$Tree.species)
sum(is.na(db.merge$Species))

ifelse(is.na(db.merge$Latitude.tree), db.merge$Latitude <- db.merge$Latitude.tree, db.merge$Latitude <- db.merge$Latitude.plot)
sum(is.na(db.merge$Latitude))

ifelse(is.na(db.merge$Longitude.tree), db.merge$Longitude <- db.merge$Longitude.tree, db.merge$Longitude <- db.merge$Longitude.plot)
sum(is.na(db.merge$Longitude))

ifelse(is.na(db.merge$Altitude..m.s.l..), db.merge$Latitude <- db.merge$Altitude..m.s.l.., db.merge$Latitude <- db.merge$ALT_m)
sum(is.na(db.merge$Altitude))
levels(db.merge$Altitude)

ifelse(is.na(db.merge$Country.tree), db.merge$Country <- db.merge$Country.tree, db.merge$Country <- db.merge$Country.plot)
sum(is.na(db.merge$Country))

ifelse(is.na(db.merge$Ecoregion.tree), db.merge$Ecoregion <- db.merge$Ecoregion.tree, db.merge$Ecoregion <- db.merge$Ecoregion.plot)
sum(is.na(db.merge$Ecoregion))

# Ecoregions Tree
ecoreg.tree <- read.csv2("ecoregion_tree.csv", header = T)
ecoreg.tree <- ecoreg.tree[,1:5]
ecoreg.tree <- na.omit(ecoreg.tree)

# Ecoregions Plot
ecoreg.plot <- read.csv2("ecoregion_plot.csv", header = T)
ecoreg.plot <- ecoreg.plot[,1:5]
ecoreg.plot <- na.omit(ecoreg.plot)

# Every Ecoregion in tree is in plot but not otherwise
sum(!(ecoreg.tree$Ecoreg_ID %in% ecoreg.plot$Ecoreg_ID)) # 0
sum(!(ecoreg.plot$Ecoreg_ID %in% ecoreg.tree$Ecoreg_ID)) # 75

# Right outer join because of plot contains Ecoregions not included in tree
ecoregion <- merge(x = ecoreg.tree, y = ecoreg.plot, by = "Ecoreg_ID", all.y = T,suffixes = c(".tree", ".plot") )

# Common Variables in Datasets
ifelse(is.na(ecoregion$Ecorefion_name.tree), ecoregion$Ecoregion.name <- ecoregion$Ecorefion_name.tree, ecoregion$Ecoregion.name <- ecoregion$Ecorefion_name.plot)
sum(is.na(ecoregion$Ecoregion))

ifelse(is.na(ecoregion$Biome_ID.tree), ecoregion$Biome.ID <- ecoregion$Biome_ID.tree, ecoregion$Biome.ID <- ecoregion$Biome_ID.plot)
sum(is.na(ecoregion$Biome.ID))

ifelse(is.na(ecoregion$Biome_name.tree), ecoregion$Biome.name <- ecoregion$Biome_name.tree, ecoregion$Biome.name <- ecoregion$Biome_name.plot)
sum(is.na(ecoregion$Biome.name))

# Every ID in Ecoregion is available in data dataset but not vice versa
F %in% (ecoregion$Ecoreg_ID %in% db.merge$Ecoregion)
F %in% (db.merge$Ecoregion %in% ecoregion$Ecoreg_ID)

# Right outer join data, ecoregion on the Id column
final <- merge(db.merge, ecoregion, by.x = "Ecoregion", by.y = "Ecoreg_ID", all.x = T,
            suffixes = c(".data", ".eco"))
