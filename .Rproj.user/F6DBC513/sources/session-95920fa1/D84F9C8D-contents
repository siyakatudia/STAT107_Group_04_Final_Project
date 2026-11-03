# this file defines functions for data cleaning 

# function for checking missing values 

check_missing <- function(data) {
  colSums(is.na(data))
}



# function for checking duplicate observations

check_duplicates <- function(data, identifying_columns) {
  sum(duplicated(data[identifying_columns]))
}


#functions to check all the unique categories 

check_categories <- function(data) {
  unique(data$Category)
}


## Creates new dataset selecting indexed observations.
data_subset <- function(data, index) {
  data_red <- data[index, ]
  return(data_red)
}

