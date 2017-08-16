source("loadingData.R")

library(ggplot2)

species <- c("Abies","Acacia","Acer","Alnus","Betula","Carpinus","Castanopsis","Chamaecyparis","Chosenia","Cryptomeria","Cunninghamia",
             "Cupressus","Cyclobalanopsis","Eucalyptus","Fagus","Fokienia","Fraxinus","Larix","Picea","Pinus","Populus","Pseudotsuga",
             "Quercus","Robinia","Salix","Tilia") 

plot.db$Species2 <- "Other.Species"
tree.db$Species2 <- "Other.Species"

# Carpinus was missing in Species2 because it was overwritten by Pinus
# ignore.case = T caused the Error!!


for(i in 1:length(species)) {
   plot.db[grep(paste0(species[i]),plot.db$Tree.species),]$Species2 <- paste0(species[i])
   #tree.db[grep(paste0(species[i]),tree.db$Species, ignore.case = T),]$Species2 <- paste0(species[i])  
   print(paste0(species[i]))
}

# Species not in plot
# "Sorbus"

species <- c("Abies","Acer","Alnus","Betula","Carpinus","Chamaecyparis","Chosenia","Cryptomeria",
             "Fagus","Fraxinus","Larix","Picea","Pinus","Populus","Pseudotsuga",
             "Quercus","Robinia","Salix","Sorbus","Tilia") 

for(i in 1:length(species)) {
  #plot.db[grep(paste0(species[i]),plot.db$Tree.species, ignore.case = T),]$Species2 <- paste0(species[i])
  tree.db[grep(paste0(species[i]),tree.db$Species),]$Species2 <- paste0(species[i])  
  print(paste0(species[i]))
}

# Species not in tree
# "Castanopsis","Cunninghamia", "Cupressus", "Cyclobalanopsis", "Eucalyptus",
# "Fokienia", "Acacia",


tmp1 <- as.data.frame(table(plot.db$Species2))
tmp2 <- as.data.frame(table(tree.db$Species2))

table5 <- full_join(tmp1, tmp2, by = "Var1")
table5 <- as.data.frame(table5)
names(table5) <- c("Tree genus", "Sample plots", "Sample trees")

View(table5)
write.csv2(table5, file = "table5.csv")

nrow(table5)

ggplot(data = table5) +
  geom_col(aes(table5$`Tree genus`,table5$`Sample plots`), alpha = 0.3,
           colour = 1:27,size = 1,linetype = "solid") +
  geom_col(aes(table5$`Tree genus`, table5$`Sample trees`), alpha = 0.3,
           size = 1,linetype = "dotted") +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size = 10,
                                 face = "bold", family = "Arial"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "Nr of Counts", title = "Visualisation of Table 5")



