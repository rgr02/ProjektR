source("loadingData.R")
source("multiplot.R")



a <- ggplot(tree.db, aes(tree.db$DBH, tree.db$H.tree)) +
  geom_point( col = "blue",
              alpha = 0.3) +
  geom_smooth(method = "auto",
              na.rm = T,
              se = F,
              col = "red",
              alpha = 0.5) +
  xlab("DBH, cm") +
  ylab("Height, m") +
  xlim(0,80) +
  ylim(0,45) +
  theme_minimal()

# Assuming crown is leaves and bark
stem <- tree.db$Pst # Stamm und Rinde
root <- tree.db$Proot # Wurzeln und Stumpf
leaves <- tree.db$Pf # Blätter
bark <- tree.db$Pbr # Zweige
crown <- leaves + bark
volume <- tree.db$Vtot  # Volume cm3


b <- ggplot() +
  geom_point(aes(volume, stem), col = "red", alpha = 0.3) +
  geom_smooth(data = as.data.frame(cbind(volume, stem)),
              aes(volume, stem), method = "lm", col = "red", show.legend = T) +
  geom_point(aes(volume, root), col = "green", alpha = 0.3) +
  geom_smooth(data = as.data.frame(cbind(volume, root)),
              aes(volume, root), method = "lm", col = "green", show.legend = T) +
  geom_point(aes(volume, crown), col = "blue", alpha = 0.3) +
  geom_smooth(data = as.data.frame(cbind(volume, crown)),
              aes(volume, crown), method = "lm", col = "blue", show.legend = T) +
  xlab("Stem Volume, cm3") +
  ylab("Biomass, kg") +
  xlim(0,7000) +
  ylim(0,3500) +
  labs(caption = paste0("red - Stem", "\n", "green - Root", "\n", "blue - Crown")) +
  theme_minimal()

multiplot(a,b,cols = 2)