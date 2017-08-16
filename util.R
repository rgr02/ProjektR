


# function for summary statistics
# necessary for task 2 and 3
summary.stats <- function(vec,...){
  
  # does the function contain a parameter
  if (missing(vec)){  
    stop("Der Funktion muss ein Vektor als Parameter übergeben werden")
  }
  
  # parsing Parameter to a vector object for sapply
  vec <- as.vector(vec) 
  
  # vector of length 1 is a scalar
  if (length(vec) < 2){  
    stop("Der Funktion muss ein Vektor als Parameter übergeben werden")
  }
  
  # 2 seperate if'S for checking 1 parameter is a
  # clumsy coding style but it does the job
  
  stats <- vector(length = 5) # Initialising return vector
  stats <- as.vector(stats, mode = "any") # any for mixed content
  
  # warning about missings 
  # if numeric variables contain missings, na.rm must be specified
  # additionally for calculations
  
  if (any(is.na(vec))){
    warning("Der Übergebene Vektor enthält NA Werte")
  }
  
  # Counting non NA values
  stats[1] <- sum(!is.na(vec))
  
  print("ich bin vor is.numeric") 
  # debug because
  # apply is not executing the if branch
  # worked fine after switching to sapply
  
  if (is.numeric(vec)){
    print("ich bin nach is.numeric")
    stats[2] <- NA
    stats[3] <- round(median(vec, na.rm = T),2)
    stats[4] <- round(min(vec, na.rm = T),2)
    stats[5] <- round(max(vec, na.rm = T),2)
  } 
  else {
    print("Ich bin im else")
    stats[2] <- length(unique(vec))
    stats[3] <- NA
    stats[4] <- NA
    stats[5] <- NA
  }
  print("ich gebe stats zurück")
  return(stats)
}