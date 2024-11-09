### This file contains all the self-written functions used in the project. ###

### List of functions ###
# 1. remove_NA
# TODO

# Function 1: remove_NA, deletes columns or rows if they are all NAs.
remove_NA <- function(df) {
  assertDataFrame(df)
  require(dplyr)
  df <- df[rowSums(is.na(df)) < ncol(df), ] %>%
    select_if(~ !all(is.na(.)))
  return(df)
}