
source("loadingData.R")

# 1.1 Beobachtungen Variablen
# Dimensions
dim.plot <- dim(plot.db)
dim.tree <- dim(tree.db)

dim.plot
dim.tree

# Nr Observations from Aut
plot.aut <- subset(plot.db, plot.db$Country == "AUT")
tree.aut <- subset(tree.db, tree.db$Country == "AUT")

nrow(plot.aut)
nrow(tree.aut) # 0 observations for Aut


# 1.2 Plots
library(ggmap)
library(maps)

mapWorld <- borders("world", colour="gray", fill="gray", alpha = 0.3)
world.map <- ggplot() + 
  mapWorld +
  geom_point(aes(plot.db$Longitude, plot.db$Latitude), col = "green", alpha = 0.3) +
  geom_point(aes(tree.db$Longitude, tree.db$Latitude), col = "red", alpha = 0.3) +
  ggtitle("Location of Sample Trees and Plots") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal() +
  labs(caption = paste0("red - Trees", "\n", "blue - Plots")) +
  theme(plot.title = element_text(hjust = 0.5))


aut <- borders('world', 'Austria', colour="gray", fill="gray", alpha = 0.3)
aut.map <- ggplot() +
  aut +
  geom_point(aes(plot.aut$Longitude, plot.aut$Latitude), col = "blue", alpha = 0.3) +
  ggtitle("Location of Sample Plots in Austria") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_minimal() +
  labs(caption = paste0("blue - Plots")) +
  theme(plot.title = element_text(hjust = 0.5))

world.map
aut.map

