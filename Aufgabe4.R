source("loadingData.R")

# No dependencies to other scripts except loading data

library(dplyr)
library(ggplot2)


tree <- select(tree.db, Country)
plot <- select(plot.db, Country)


plot$Country.name <- "Other Countries"
tree$Country.name <- "Other Countries"


try(
  
  {
  # Creating Column with Countrynames for Table 3
  # Tedious renaming procedure
  plot[which(plot$Country == "BEL"), ]$Country.name <- "Belgium"
  plot[which(plot$Country == "BLR"), ]$Country.name <- "Belarus"
  plot[which(plot$Country == "BUL"), ]$Country.name <- "Bulgaria"
  plot[which(plot$Country == "CHN"), ]$Country.name <- "China"
  plot[which(plot$Country == "CZE"), ]$Country.name <- "Czech Republic"
  plot[which(plot$Country == "DEN"), ]$Country.name <- "Denmark"
  plot[which(plot$Country == "FIN"), ]$Country.name <- "Finnland"
  plot[which(plot$Country == "FRA"), ]$Country.name <- "France"
  plot[which(plot$Country == "GBR"), ]$Country.name <- "United Kingdom"
  plot[which(plot$Country == "GER"), ]$Country.name <- "Germany"
  plot[which(plot$Country == "HUN"), ]$Country.name <- "Hungary"
  plot[which(plot$Country == "IND"), ]$Country.name <- "India"
  plot[which(plot$Country == "ITA"), ]$Country.name <- "Italy"
  plot[which(plot$Country == "JPN"), ]$Country.name <- "Japan"
  plot[which(plot$Country == "KAZ"), ]$Country.name <- "Kazakhstan"
  plot[which(plot$Country == "LTU"), ]$Country.name <- "Lithuania"
  plot[which(plot$Country == "MGL"), ]$Country.name <- "Mongolia"
  plot[which(plot$Country == "RUS"), ]$Country.name <- "Russian Federation"
  plot[which(plot$Country == "SWE"), ]$Country.name <- "Sweden"
  plot[which(plot$Country == "SWZ"), ]$Country.name <- "Switzerland"
  plot[which(plot$Country == "UKR"), ]$Country.name <- "Ukraine"
  
  # Tree
  tree[which(tree$Country == "BEL"), ]$Country.name <- "Belgium"
  tree[which(tree$Country == "BLR"), ]$Country.name <- "Belarus"
  tree[which(tree$Country == "BUL"), ]$Country.name <- "Bulgaria"
  tree[which(tree$Country == "CHN"), ]$Country.name <- "China"
  tree[which(tree$Country == "CZE"), ]$Country.name <- "Czech Republic"
  tree[which(tree$Country == "DEN"), ]$Country.name <- "Denmark"
  tree[which(tree$Country == "FIN"), ]$Country.name <- "Finnland"
  tree[which(tree$Country == "FRA"), ]$Country.name <- "France"
  tree[which(tree$Country == "GBR"), ]$Country.name <- "United Kingdom"
  tree[which(tree$Country == "GER"), ]$Country.name <- "Germany"
  tree[which(tree$Country == "HUN"), ]$Country.name <- "Hungary"
  tree[which(tree$Country == "IND"), ]$Country.name <- "India"
  tree[which(tree$Country == "ITA"), ]$Country.name <- "Italy"
  tree[which(tree$Country == "JPN"), ]$Country.name <- "Japan"
  tree[which(tree$Country == "KAZ"), ]$Country.name <- "Kazakhstan"
  tree[which(tree$Country == "LTU"), ]$Country.name <- "Lithuania"
  tree[which(tree$Country == "MGL"), ]$Country.name <- "Mongolia"
  tree[which(tree$Country == "RUS"), ]$Country.name <- "Russian Federation"
  tree[which(tree$Country == "SWE"), ]$Country.name <- "Sweden"
  tree[which(tree$Country == "SWZ"), ]$Country.name <- "Switzerland"
  tree[which(tree$Country == "UKR"), ]$Country.name <- "Ukraine"
  },
  
  silent = T
  
  # Warning message for trycatch commented
  # warning = warning("Dieses Land befand sich nicht im Datensatz")
)



# Nonsense attempt! table command does the job...
# table3 <- select(plot, Country.name) %>%
#   group_by(Country.name) %>% 
#   summarise(Plots = n()) %>%
#   summarise(tree$Country.name, Trees = n())

plot.count <- as.data.frame(table(plot$Country.name))
plot.count
tree.count <- as.data.frame(table(tree$Country.name))
tree.count

table3 <- merge(plot.count, tree.count, by = "Var1", all.x = T)
names(table3) <- c("Country.Name", "Plots", "Trees")

View(table3)
write.csv2(table3, file = "table3.csv")

ggplot(data = table3) +
  geom_col(aes(table3$Country.Name, table3$Plots), alpha = 0.3,
           colour = table3$Country.Name, size = 1,linetype = "dotted") +
  geom_col(aes(table3$Country.Name, table3$Trees), alpha = 0.3,
           size = 1) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, hjust=1, size = 10,
                                 face = "bold", family = "Arial"),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "", y = "Nr of Counts", title = "Visualisation of Table 3")
