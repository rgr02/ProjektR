setwd("C:/Users/Rudi/Dropbox/RCoding/ProjektR")

library(dplyr)

# Tedious read-in procedures tested like XLConnect and xlsx
# Java Heap Memory Error
# Deleted the code

# Databases
plot.db <- read.csv2("plot_db.csv", header = T)
plot.db <- plot.db[,-1]
sum(is.na(plot.db$ID)) # No missing key Variable


columns <- which(sapply(plot.db, is.numeric)) # selecting all numeric columns
sapply(plot.db[,columns], summary) # summary stats for numeric variables

# Imputing 0 for NA

# Unnecessary Loop where I replaced NA Values with 0 because 0 is a neutral
# Element in Addition (for mean function)
# Calculating the median is biased with this imputation
# In Databases, null values are an undifined state which should be avoided
# and I thought it might be good use in this case too

# for(i in columns){
#   logic <- is.na(plot.db[,i])
#   plot.db[which(logic),i] <- 0
# }


tree.db <- read.csv2("tree_db.csv", header = T)
tree.db <- tree.db[,-1]
sum(is.na(tree.db$ID_Plot)) 
# Missing Values in Foreign Key ID_Plot
# No match possible -> deleting observations
# tree.db <- tree.db[which(!is.na(tree.db$ID_Plot)),]

# Ecoregions
ecoreg.plot <- read.csv2("ecoreg_plot.csv", header = T)
ecoreg.plot <- ecoreg.plot[,2:6]
ecoreg.plot <- na.omit(ecoreg.plot)

# T %in% sapply(ecoreg.plot, is.na) # ecoreg.plot complete dataset
# Correction
any(sapply(ecoreg.plot, is.na))
names(ecoreg.plot) <- c("Ecoreg.ID", "Ecoregion.name", "Biome.ID", "Biome.name","n_Plots")

ecoreg.tree <- read.csv2("ecoreg_tree.csv", header = T)
ecoreg.tree <- ecoreg.tree[,2:6]
ecoreg.tree <- na.omit(ecoreg.tree)
# Correction also here
any(sapply(ecoreg.tree, is.na)) # ecoreg.tree complete dataset
names(ecoreg.tree) <- c("Ecoreg.ID", "Ecoregion.name", "Biome.ID", "Biome.name","n_Trees")

# Every Ecoregion in tree is in plot but not otherwise
sum(!(ecoreg.plot$Ecoreg.ID %in% ecoreg.tree$Ecoreg.ID)) # 48 in, 75 not in
sum(!(ecoreg.tree$Ecoreg.ID %in% ecoreg.plot$Ecoreg.ID)) # 48, 0


# Joins
db.merge <- full_join(tree.db, plot.db, 
                      by = c("ID_Plot" = "ID"),
                      suffix = c(".tree", ".plot"))

ecoregion <- right_join(ecoreg.tree, ecoreg.plot,
                        by = "Ecoreg.ID",
                        suffix = c(".tree", ".plot"))

sum((plot.db$Ecoregion %in% ecoreg.plot$Ecoreg.ID)) # 10351 in
sum((ecoreg.plot$Ecoreg.ID %in% plot.db$Ecoregion)) # 123 in 

plot.merge.ecoregion <- left_join(plot.db, ecoreg.plot,
                                  by = c("Ecoregion" = "Ecoreg.ID"))


sum((tree.db$Ecoregion %in% ecoreg.tree$Ecoreg.ID)) # 9613
sum((ecoreg.tree$Ecoreg.ID %in% tree.db$Ecoregion)) # 48

tree.merge.ecoregion <- left_join(tree.db, ecoreg.tree,
                                  by = c("Ecoregion" = "Ecoreg.ID"))

