
source("loadingData.R")

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

library(tidyr)

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

# Implementation of the converting function
string2numeric <- function(vec = vector(), method = c("mean", "min", "max", "uniform")) {
  
  ### Checking Inputs ###############################################
  if(missing(vec) || missing(method)){
    stop("Der Funktion müssen ein Vektor und eine Methodenspezifikation übergeben werden")
  }
  
 
  
  # Datatype == vector?
  # Parsing
  if(is.vector(vec) == F){
    warning("Der Datentyp des Parameters entspricht nicht einem Vektor /n
            Es wird versucht den Input zu parsen")
    vec <- as.vector(vec)
  }
  
  
  # Vector of letters?
  if(F %in% sapply(vec, is.character)){
    warning("Der übergebene Vektor enthält Einträge, die keine Buchstaben sind!")
    vec <- as.character(vec)
  }
  
  
  if (length(vec) == 1L){  
    stop("Der Funktion muss ein Vektor als Parameter übergeben werden")
  }
  
  
  # Missings?
  if(any(is.na(vec))){
    warning("Der Vektor enthält NA Werte")
  }
  
  # Allowable methods specified in Parameter?
  methods <- c("mean", "min", "max", "uniform")
  if(!tolower(method) %in% methods){
    stop("Sie haben eine Berechnungsmethode für den Repräsentanten spezifiziert, die in dieser
         Funktion nicht implementiert ist!")
  }
  
  print("methods")
  
  ### Program Logic ###########################################
  
  
  # Initializing Vector for the parsed values 
  returnvalues <<- vector(mode = "numeric", length = length(vec))
  
  # Notes:
  # Es muss identifiziert werden, ob ein Eintrag atomar oder Vector ist
  # Atomare Werte in numeric parsen
  # Vector Splitten in Min und Max Value
  # Selektion eines Repräsentanten gemäß der Auswahl
  
  # Splitting intervals
  vec <- as.data.frame(vec)
  names(vec) <- c("Numbers")
  vec <- separate(vec,Numbers,
                   into = c("Min", "Max"), sep = "-", convert = T,
                   remove = F)
  
  # Single value: Min = Entry, Max = NA
  # Intervall: Min = Entry, Max = Entry
  
  method <- match.arg(method)
  
  for(i in 1:nrow(vec)){
    
    # Entry for Intervalls
    if(is.na(vec[i,]$Max) == F){
      
      # Representative according to method
      if(method == "mean"){
        returnvalues[i] <- (vec[i,]$Min + vec[i,]$Max)/2
      }
      
      if (method == "min"){
        returnvalues[i] <- vec[i,]$Min
      }
      
      if (method == "max"){
        returnvalues[i] <- vec[i,]$Max
      }
      
      if (method == "uniform"){
        returnvalues[i] <- runif(1, min = vec[i,]$Min, max = vec[i,]$Max)
      }
      
      print(i)
      
    }else{
      # assigning non intervall
      returnvalues[i] <- vec[i,]$Min
      print(i)
    }
  }# for
  return(as.numeric(returnvalues))
  } # Function


x <- string2numeric(plot.db$Number.of.sampled.trees, method = "uniform" )
x 
summary(x)

