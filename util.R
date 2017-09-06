# Utility script where auxiliary functions are implemented

library(tidyr)


# function for summary statistics
# necessary for task 2 and 3

summary.stats <- function(vec) {
  # does the function contain a parameter
  if (missing(vec)) {
    stop("Der Funktion muss ein Vektor als Parameter übergeben werden")
  }
  
  # parsing Parameter to a vector object for sapply
  vec <- as.vector(vec)
  
  # vector of length 1 is a scalar
  if (length(vec) < 2L) {
    stop("Der Funktion muss ein Vektor als Parameter übergeben werden")
  }
  
  # 2 seperate if'S for checking 1 parameter is a
  # clumsy coding style but it does the job
  
  # stats <- vector(mode = "numeric", length = 5) # Initialising return vector
  
  # Correction to numeric vector
  stats <- vector(mode = "numeric",length = 5) # Initialising return vector
  #stats <- as.vector(stats, mode = "any") # any for mixed content
  
  # warning about missings
  # if numeric variables contain missings, na.rm must be specified
  # additionally for calculations
  
  if (any(is.na(vec))) {
    warning("Der Übergebene Vektor enthält NA Werte")
  }
  
  # Counting non NA values
  stats[1] <- sum(!is.na(vec))
  
  
  # debug because
  # apply is not executing the if branch
  # worked fine after switching to sapply
  
  if (is.numeric(vec)) {
    
    stats[2] <- NA
    stats[3] <- round(median(vec, na.rm = T), 2)
    stats[4] <- round(min(vec, na.rm = T), 2)
    stats[5] <- round(max(vec, na.rm = T), 2)
  }
  else {
    
    stats[2] <- length(unique(vec))
    stats[3] <- NA
    stats[4] <- NA
    stats[5] <- NA
  }
  
  return(stats)
} # summary.stats




# Implementation of the converting function for task 8
string2numeric <-
  function(vec = vector(),
           method = "mean") {
    
    
    ### Checking Inputs ###############################################
    if (missing(vec)) {
      stop("Der Funktion muss ein Vektor übergeben werden")
    }
    
    # Datatype == vector?
    # Parsing
    if (is.vector(vec) == F) {
      warning(
        "Der Datentyp des Parameters entspricht nicht einem Vektor /n
        Es wird versucht den Input zu parsen"
      )
      vec <- as.vector(vec)
    }
    
    
    # Vector of letters?
    # Unnecessary because a vector only contains objects of the same type
    
    # if (F %in% sapply(vec, is.character)) {
    #   warning("Der übergebene Vektor enthält Einträge, die keine Buchstaben sind!")
    #   vec <- as.character(vec)
    # }
    
    
    # Vector of length 1 is a scalar
    if (length(vec) == 1L) {
      stop("Der Funktion muss ein Vektor als Parameter übergeben werden")
    }
    
    
    
    # Missings?
    if (any(is.na(vec))) {
      warning("Der Vektor enthält NA Werte")
    }
    
   
    
    # Allowable methods specified in Parameter?
    
    # Correction: Not necesarry if match.arg is used. -> valid entries in choices area
    # methods <- c("mean", "min", "max", "uniform")
    # if (!tolower(method) %in% methods) {
    #   stop(
    #     "Sie haben eine Berechnungsmethode für den Repräsentanten spezifiziert, die in dieser
    #     Funktion nicht implementiert ist!"
    #   )
    # }
    
    
    
    ### Program Logic ###########################################
    
    
    # Initializing Vector for the parsed values
    returnvalues <- vector(mode = "numeric", length = length(vec))
    
    # Notes:
    # Es muss identifiziert werden, ob ein Eintrag atomar oder Vector ist
    # Atomare Werte in numeric parsen
    # Vector Splitten in Min und Max Value
    # Selektion eines Repräsentanten gemäß der Auswahl
    
    # Splitting intervals
    vec <- as.data.frame(vec)
    names(vec) <- c("Numbers")
    vec <- separate(
      vec,
      Numbers,
      into = c("Min", "Max"),
      sep = "-",
      convert = T,
      remove = F
    )
    
    # Single value: Min = Entry, Max = NA
    # Intervall: Min = Entry, Max = Entry
    
    method <- match.arg(method,
                        c("mean", "min", "max", "uniform"))
    
    # Correction vectorized
    # apply(vec,1,function(vec){
    #   # Entry for Intervalls
    #   if (is.na(vec[i, ]$Max) == F) {
    #     # Representative according to method
    #     if (method == "mean") {
    #       returnvalues[i] <- (vec[i, ]$Min + vec[i, ]$Max) / 2
    #     }
    #     
    #     if (method == "min") {
    #       returnvalues[i] <- vec[i, ]$Min
    #     }
    #     
    #     if (method == "max") {
    #       returnvalues[i] <- vec[i, ]$Max
    #     }
    #     
    #     if (method == "uniform") {
    #       returnvalues[i] <- runif(1, min = vec[i, ]$Min, max = vec[i, ]$Max)
    #     }
    #     
    #   } else{
    #     # assigning non intervall
    #     returnvalues[i] <- vec[i, ]$Min
    #     
    #   } 
    # })
    
    
    
    # Old
     for (i in 1:nrow(vec)) {
      # Entry for Intervalls
      if (is.na(vec[i, ]$Max) == F) {
        # Representative according to method
        if (method == "mean") {
          returnvalues[i] <- (vec[i, ]$Min + vec[i, ]$Max) / 2
        }

        if (method == "min") {
          returnvalues[i] <- vec[i, ]$Min
        }

        if (method == "max") {
          returnvalues[i] <- vec[i, ]$Max
        }

        if (method == "uniform") {
          returnvalues[i] <- runif(1, min = vec[i, ]$Min, max = vec[i, ]$Max)
        }


      } else{
        # assigning non intervall
        returnvalues[i] <- vec[i, ]$Min

      }
  }
    return(as.numeric(returnvalues))
  } # string2numeric


# Function for key figures of task 8 
simple.summary <-
  function(vec = vector(),
           method = c("mean", "median", "sd", "iqr"),
           ...) {
    ### Checking Inputs ###############################################
    if (missing(vec) || missing(method)) {
      stop("Der Funktion müssen ein Vektor und eine Methodenspezifikation übergeben werden")
    }
    
    # Datatype == vector?
    # Parsing
    if (is.vector(vec) == F) {
      warning(
        "Der Datentyp des Parameters entspricht nicht einem Vektor /n
        Es wird versucht den Input zu parsen"
      )
      vec <- as.vector(vec)
    }
    
    # Vector of length 1 is a scalar
    if (length(vec) == 1L) {
      stop("Der Funktion muss ein Vektor als Parameter übergeben werden")
    }
    
    # Missings?
    if (any(is.na(vec))) {
      warning("Der Vektor enthält NA Werte")
    }
    
    # Allowable methods specified in Parameter?
    
    # Commenting because of match.arg
    # methods <- c("mean", "median", "sd", "iqr")
    # if (!tolower(method) %in% methods) {
    #   stop(
    #     "Sie haben eine Berechnungsmethode für den Repräsentanten spezifiziert, die in dieser
    #     Funktion nicht implementiert ist!"
    #   )
    # }
    
    
    ### Program Logic ###########################################
    
    method <- match.arg(method,
                        c("mean", "median", "sd", "iqr"))
    
    # switch statement more beautiful here
    if (method == "mean") {
      return(mean(vec, na.rm = T))
    }
    
    if (method == "median") {
      return(median(vec, na.rm = T))
    }
    
    if (method == "sd") {
      return(sd(vec, na.rm = T))
    }
    
    if (method == "iqr") {
      return(IQR(vec, na.rm = T))
    }
    
  } # simple.summary