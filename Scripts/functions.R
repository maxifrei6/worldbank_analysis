### This file contains all the self-written functions used in the project. ###


### List of functions ###
# 1. remove_NA
# 2. group_data


# FUNCTION 1: remove_NA; Deletes columns or rows if they are all NAs.
# 
# Inputs:
# - df: A data frame of no further specification.
#
# Output: 
# - A data frame containing the same non-NA values as before only with all rows and columns,
# that consist of NAs only, having been removed.
remove_NA <- function(df) {
  require(checkmate)
  assertDataFrame(df)
  require(dplyr)
  df <- df[rowSums(is.na(df)) < ncol(df), ] %>%
    select_if(~ !all(is.na(.)))
  return(df)
}


# FUNCTION 2: group_data; Group a data frame according to certain indicator.
#
# The 'group_data' function groups the data frame values according to a certain indicator by
# comparing the indicator's average value with self-defined quantile intervals and assigning
# the corresponding self-defined group labels.
#
# Inputs: 
# - df: A data frame containing at least the columns 'Country Name', 'Country Code', 'Series Name',
# 'Series Code' and 'average', further columns are possible.
# - grouping_by: A single string being an entry of the column 'Series Code' specifying the indicator
# to group the averages by.
# - column_new: A single string naming the new column, that is added to the end of the minimal data frame
# containing the columns specified in input 'df'. The resulting column name is created according to scheme:
# 'cat_' + column_new (e.g. column_new = 'population' --> 'cat_population').
# - quantile_labels: A character vector with no missing values defining the category labels, the grouped
# data shall be assigned into. Supposed to be inserted in ascending intensity of occurrence for it to be
# rightfully allocated.
#
# Output:
# - A data frame of the columns 'Country Name', 'Country Code' and the newly created column 'cat_...',
# where the entries of the column are an ordered factor displaying in which quantile the countries' values
# were assigned according to the chosen grouping indicator.
group_data <- function(df, grouping_by, column_new, quantile_labels) {
  
  # Check for valid input
  require(checkmate)
  assertDataFrame(df)
  assertSubset(c("Country Name", "Country Code", "Series Name", "Series Code", "average"), colnames(df))
  assertString(grouping_by)
  assertChoice(grouping_by, df$`Series Code`)
  assertString(column_new)
  assertCharacter(quantile_labels, any.missing = FALSE)
  
  # Select columns and filter the relevant rows with information to interested grouping variable
  require(dplyr)
  df <- df %>% select(`Country Name`, `Country Code`, `Series Name`, `Series Code`, average) %>%
    filter(`Series Code` == grouping_by)
  
  # Calculate the n_quantiles of the 'average' column
  n_quantile <- length(quantile_labels)
  quantile_probs <- seq(from = 0, to = 1, by = 1 / n_quantile)
  quantile_data <- quantile(df$average, probs = quantile_probs)
  
  # Dynamically create the new column name 'column_new' 
  column_new <- paste0("cat_", column_new)
  
  df <- df %>%
    mutate(
      # Add values for the new, dynamically-created column according to case_when assignment
      {{ column_new }} := quantile_labels[findInterval(df$average, quantile_data, rightmost.closed = TRUE)],
      
      # Convert the new, dynamically-created column to an ordered factor
      {{ column_new }} := factor(.data[[column_new]], levels = rev(quantile_labels), ordered = TRUE)
    )
  
  # Select the relevant columns
  df <- df %>%
    select(`Country Name`, `Country Code`, all_of(column_new))

  return(df)
}
