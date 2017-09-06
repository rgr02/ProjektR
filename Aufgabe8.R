
source("loadingData.R")
source("util.R")

# Requirements
# Nr of trees per plot
# mean, median, sd, iqr
# ... Argument for na.rm specification


str(plot.db)
plot.db$Tree.number
plot.db$Basal.area
plot.db$Number.of.sampled.trees
str(plot.db$Number.of.sampled.trees)

# Assumption: Number of trees for destructive sampling is the target variable
library(ggplot2)

# Die Funktion soll einen übergebenen Buchstabenvektor in einen numerischen Vektor umwandeln.
# Numerische Werte sind 1:1 zu übernehmen.
# Für Intervalle der Form x-y ist ein Repräsentant zu ermitteln.
# Die Auswahl des Repräsentant ist möglich durch
# - Mittelwert
# - Min
# - Max
# - gleichverteilte, zufällige Auswahl aus dem Intervall
# 
# Defaultmässig werden fehlende Werte entfernt

str(plot.db$Number.of.sampled.trees)
# Factor w/ 36 levels "1","10","10-15",..: NA 12 12 NA NA NA NA NA NA NA ...

levels(plot.db$Number.of.sampled.trees)
# [1] "1"     "10"    "10-15" "10-20" "102"   "11"    "12"   
# [8] "14"    "15"    "17"    "18"    "2"     "2-3"   "21"   
# [15] "23"    "24"    "26"    "3"     "3-5"   "34"    "4"    
# [22] "49"    "5"     "5-10"  "5-15"  "5-8"   "5-9"   "6"    
# [29] "6-7"   "6-8"   "60"    "7"     "8"     "9"     "9-12" 
# [36] "9-30" 

# Outlying value: 102 trees/ha ??


x <- string2numeric(plot.db$Number.of.sampled.trees, method = "uniform" )
summary(x)

# Type could be a dataframe here too
result <- matrix(nrow = 4, ncol = 2)
rownames(result) <- c("mean", "median", "sd", "iqr")
colnames(result) <- c("Data with Representatives", "Data without Representatives")

# Erste Spalte
result[1,1] <- simple.summary(x,method = "mean", na.rm = T)
result[2,1] <- simple.summary(x,method = "median", na.rm = T)
result[3,1] <- simple.summary(x,method = "sd", na.rm = T)
result[4,1] <- simple.summary(x,method = "iqr", na.rm = T)

#x <- gsub(paste0("^[:digit:]-[:digit:]$"),NA,plot.db$Number.of.sampled.trees)
y <- gsub("-",NA,plot.db$Number.of.sampled.trees) # Extracting intervalls and replacing values with NA
y <- as.numeric(y)

# Zweite Spalte
result[1,2] <- simple.summary(y,method = "mean", na.rm = T)
result[2,2] <- simple.summary(y,method = "median", na.rm = T)
result[3,2] <- simple.summary(y,method = "sd", na.rm = T)
result[4,2] <- simple.summary(y,method = "iqr", na.rm = T)

result <- as.data.frame(result)
View(result)

ggplot() +
  geom_density(aes(result$`Data with Representatives`), fill = "red", alpha = 0.3) +
  geom_density(aes(result$`Data without Representatives`), fill = "green", alpha = 0.3) +
  theme_minimal() +
  labs(x = "Nr. of Trees per Plot", y = "Density", title = "Aufgabe 8 - Vergleichender Graph")+
  theme(plot.title = element_text(hjust = 0.5))
