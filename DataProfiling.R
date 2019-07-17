# This function provides the moden of a column, if the data is numerical, then it's rounded to 2 decimals.
my_mode <- function(x){
  if (class(x) %in% c("character", "factor")) {
    table(x) %>%
      which.max() %>%
      names()
  }
  else {
    table(round(x, 2)) %>%
      which.max() %>%
      names()
  }
}

#This function makes data profiling of a data set per column, depending on wheter they're numerical or categorical data.
profiling <- function(frame, type = "other"){
  # Cardinality
  uniques <- data.frame(uniques = sapply(frame, function(x) unique(x) %>% length()))
  
  # Search for NA values
  nan <- data.frame(nan = sapply(frame, function(x) sum(is.na(x))))
  
  if (type == "categorical"){
    
    # Get the mode
    mode <- data.frame(mode = sapply(frame, function(x) my_mode(x)))
    profiling_df <- cbind(uniques, nan, mode)
    profiling_df
  }
  else {
    profiling_df <- cbind(uniques, nan)
    profiling_df
  }
}