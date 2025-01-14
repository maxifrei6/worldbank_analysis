### This file contains all the self-written functions used in the project. ###


### List of functions ###
#' 1. remove_NA
#' 2.1. group_data
#' 2.2. group_data_year
#' 3. align_colnames
#' 4. remove_metadata
#' 5. translate
#' 6. prepare_join
#' 7. kl_div_from_dnorm
#' 8. source_rmd


#' FUNCTION 1:
#' @description
#' The function 'remove_NA' deletes all columns or rows if they fully consist of NAs.
#'
#' Inputs:
#' @param ... One or more data frames of no further specification.
#'
#' Output:
#' @returns A list of the inputted data frames containing the same non-NA values as
#' before, only with all rows and columns that consist of NAs only having been removed.
#' Can then be used to assign them as variables in a specified environment, using the
#' list's names as variable names.
remove_NA <- function(...) {
  # Capture all data frames passed to the function via '...'
  dfs <- list(...)

  # Check for valid input
  require(checkmate)
  lapply(dfs, assertDataFrame)

  # Automatically collect the names of input arguments in '...' via
  # 'substitute(list(...))', then remove the list information of the substitute object
  # via '[-1]' in order to get the names of the data frames alone and lastly convert them
  # to character strings via 'deparse'
  dfs_names <- sapply(substitute(list(...))[-1], deparse)

  # Filter out all rows and columns containing only NAs
  require(dplyr)
  dfs_cleaned <- lapply(dfs, function(df) {
    df <- df[rowSums(is.na(df)) < ncol(df), ] %>%
      select_if(~ !all(is.na(.)))
    return(df)
  })

  # Assign original data frame names to the cleaned data sets
  names(dfs_cleaned) <- dfs_names

  # Return the adjusted data frame
  return(dfs_cleaned)
}


#' FUNCTION 2.1:
#' @description
#' The function 'group_data' groups the data frame values according to a certain indicator
#' by comparing the indicator's average value with self-defined quantile intervals and
#' assigning the corresponding self-defined group labels.
#'
#' Inputs:
#' @param df A data frame containing at least the columns 'Country Name', 'Country Code',
#' 'Series Name', 'Series Code' and 'average', further columns are possible.
#' @param grouping_by A single string being an entry of the column 'Series Code'
#' specifying the indicator to group the averages by.
#' @param column_new A single string naming the new column, that is added to the end of
#' the minimal data frame containing the columns specified in input 'df'. The resulting
#' column name is created according to scheme: 'cat_' + column_new (e.g. column_new =
#' population' --> 'cat_population').
#' @param quantile_labels A character vector with no missing values defining the category
#' labels, the grouped data shall be assigned into. Supposed to be inserted in ascending
#' intensity of occurrence for it to be rightfully allocated.
#'
#' Output:
#' @returns A data frame of the columns 'Country Name', 'Country Code' and the newly
#' created column cat_...', where the entries of the column are an ordered factor
#' displaying in which quantile the countries' values were assigned according to the
#' chosen grouping indicator.
group_data <- function(df, grouping_by, column_new, quantile_labels) {
  # Check for valid input
  require(checkmate)
  assertDataFrame(df)
  assertSubset(c("Country Name",
                 "Country Code",
                 "Series Name",
                 "Series Code",
                 "average"),
               colnames(df))
  assertString(grouping_by)
  assertChoice(grouping_by, df$`Series Code`)
  assertString(column_new)
  assertCharacter(quantile_labels, any.missing = FALSE)

  # Select columns and filter the relevant rows with information to interested grouping
  # variable
  require(dplyr)
  df <- df %>%
    select(`Country Name`, `Country Code`, `Series Name`, `Series Code`, average) %>%
    filter(`Series Code` == grouping_by)

  # Calculate the n_quantiles of the 'average' column
  n_quantile <- length(quantile_labels)
  quantile_probs <- seq(from = 0, to = 1, by = 1 / n_quantile)
  quantile_data <- quantile(df$average, probs = quantile_probs, na.rm = TRUE)

  # Dynamically create the new column name 'column_new'
  column_new <- paste0("cat_", column_new)

  df <- df %>%
    mutate(
      # Add values for the new, dynamically-created column according to case_when
      # assignment
      {{ column_new }} := quantile_labels[findInterval(df$average,
                                                       quantile_data,
                                                       rightmost.closed = TRUE)],

      # Convert the new, dynamically-created column to an ordered factor
      {{ column_new }} := factor(.data[[column_new]],
                                 levels = quantile_labels, ordered = TRUE)
    )

  # Select the relevant columns
  df <- df %>%
    select(`Country Name`, `Country Code`, all_of(column_new))

  # Return the adjusted data frame
  return(df)
}


#' FUNCTION 2.2:
#' @description
#' The function 'group_data_year' groups the data frame values according to a certain
#' indicator per year by comparing the indicator's values of a specific year with
#' self-defined quantile intervals and assigning the corresponding self-defined group
#' labels.
#'
#' Inputs:
#' @param df A data frame containing at least the columns 'Country Name', 'Country Code'
#' the Series Codes (or at least the one we want to group by) and 'Year', further columns
#' are possible.
#' @param grouping_by A single string being an column of type 'Series Code' specifying the
#' indicator to group the values by.
#' @param year A single string containing the year to group the data for.
#' @param column_new A single string naming the new column, that is added to the end of
#' the minimal data frame containing the columns specified in input 'df'. The resulting
#' column name is created according to scheme: 'cat_' + column_new + "_" + year (e.g.
#' column_new = 'population', '2020' --> 'cat_population_2020').
#' @param quantile_labels A character vector with no missing values defining the category
#' labels, the grouped data shall be assigned into. Supposed to be inserted in ascending
#' intensity of occurrence for it to be rightfully allocated.
#'
#' Output:
#' @returns A data frame of the columns 'Country Name', 'Country Code', 'Year' and the
#' newly created column cat_...', where the entries of the column are an ordered factor
#' displaying in which quantile the countries' values were assigned according to the
#' chosen grouping indicator.
group_data_year <- function(df, grouping_by, year, column_new, quantile_labels) {
  # Check for valid input
  require(checkmate)
  assertDataFrame(df)
  assertSubset(c("Country Name",
                 "Country Code",
                 "Year",
                 grouping_by),
               colnames(df))
  assertString(grouping_by)
  assertString(column_new)
  assertCharacter(quantile_labels, any.missing = FALSE)

  # Select columns and filter the relevant rows with information to interested grouping
  # variable
  require(dplyr)
  df <- df %>% select(`Country Name`, `Country Code`, all_of(grouping_by), Year) %>%
    filter(Year == year)
  
  # Calculate the n_quantiles of the variable column
  n_quantile <- length(quantile_labels)
  quantile_probs <- seq(from = 0, to = 1, by = 1 / n_quantile)
  quantile_data <- quantile(df[[grouping_by]], probs = quantile_probs, na.rm = TRUE)
  
  # Dynamically create the new column name 'column_new'
  column_new <- paste0("cat_", column_new)
  df <- df %>%
    mutate(
      # Add values for the new, dynamically-created column according to case_when assignment
      {{ column_new }} := quantile_labels[findInterval(df[[grouping_by]],
                                                       quantile_data,
                                                       rightmost.closed = TRUE)],

      # Convert the new, dynamically-created column to an ordered factor
      {{ column_new }} := factor(.data[[column_new]],
                                 levels = quantile_labels, ordered = TRUE)
    )

  # Select the relevant columns
  df <- df %>%
    select(`Country Name`, `Country Code`, all_of(column_new))

  # Return the adjusted data frame
  return(df)
}


#' FUNCTION 3:
#' @description
#' The function 'align_colnames' aligns the column names of several data frames by finding
#' character column names containing single dots and replacing the dots by single spaces.
#'
#' Inputs:
#' @param ... One or more data frames of no further specification.
#'
#' Output:
#' @returns A list of the inputted data frames with the same columns as before only with
#' new column names for all character columns containing single dots, with the single dots
#' being replaced by a space. Can then be used to assign them as variables in a
#' specified environment, using the list's names as the variable names.
align_colnames <- function(...) {
  # Capture all data frames passed to the function via '...'
  dfs <- list(...)

  # Check for valid input
  require(checkmate)
  lapply(dfs, assertDataFrame)

  # Automatically collect the names of input arguments in '...' via
  # 'substitute(list(...))', then remove the list information of the substitute object
  # via '[-1]' in order to get the names of the data frames alone and lastly convert them
  # to character strings via 'deparse'
  dfs_names <- sapply(substitute(list(...))[-1], deparse)

  # Align the data frames column names by replacing dot between words with space
  require(dplyr)
  dfs_cleaned <- lapply(dfs, function(df) {
    df <- df %>%
      mutate(across(where(is.character))) %>%
      rename_with(~ gsub(pattern = "\\.", replacement = " ", .))
    return(df)
  })

  # Assign original data frame names to the cleaned data sets
  names(dfs_cleaned) <- dfs_names

  # Return the adjusted data frame
  return(dfs_cleaned)
}


#' FUNCTION 4:
#' @description
#' The function 'remove_metadata' searches a data frame for inputted metadata in the
#' 'Country Name' column and deletes the metadatas' rows.
#'
#' Inputs:
#' @param metadata_source A single string with the source of the data set, that is being
#' edited.
#' @param metadata_time A single string with the information of the last changes done to
#' the data set, that is being edited.
#' @param ... One or more data frames containing column 'Country Name', other columns are
#' possible.
#'
#' Output:
#' @returns A list of the inputted data frames after being adjusted. Can then be used to
#' assign them as variables in a specified environment, using the list's names as the
#' variable names.
remove_metadata <- function(metadata_source, metadata_time, ...) {
  # Capture all data frames passed to the function via '...'
  dfs <- list(...)

  # Check for valid input
  require(checkmate)
  assertString(metadata_source, na.ok = FALSE, null.ok = FALSE)
  assertString(metadata_time, na.ok = FALSE, null.ok = FALSE)
  lapply(dfs, function(df) {
    assertDataFrame(df)
    assertSubset("Country Name", colnames(df))
  })

  # Automatically collect the names of input arguments in '...' via
  # 'substitute(list(...))', then remove the list information of the substitute object
  # via '[-1]' in order to get the names of the data frames alone and lastly convert them
  # to character strings via 'deparse'
  dfs_names <- sapply(substitute(list(...))[-1], deparse)

  # Filter rows for metadata to inputted data frame and remove those rows
  require(dplyr)
  dfs_cleaned <- lapply(dfs, function(df) {
    df <- df[!(df$`Country Name` %in% c(metadata_source, metadata_time)), ]
    return(df)
  })

  # Assign original data frame names to the cleaned data sets
  names(dfs_cleaned) <- dfs_names

  # Return the adjusted data frame
  return(dfs_cleaned)
}


#' FUNCTION 5:
#' @description
#' The function 'translate' takes data frames and applies a self-defined translation
#' mapping for entries in a selected column and changes those entries according to the
#' mapping.
#'
#' Inputs:
#' @param mapping A named character vector of any length describing the translation that
#' is supposed to happen. The names are the entries that will be checked for existence in
#' the selected column. In case of existence, the character entry by that name will be
#' inserted instead of the name.
#' @param column A single string naming the column in which the translation mapping shall
#' be done.
#' @param ... One or more data frames containing column 'column', other columns are
#' possible.
#'
#' Output:
#' @returns A list of the inputted data frames with the translated values for the mapping
#' names that could be found in the selected column. Can then be used to assign them as
#' variables in a specified environment, using the list's names as the variable names.
translate <- function(mapping, column, ...) {
  # Capture all data frames passed to the function via '...'
  dfs <- list(...)

  # Check for valid input
  require(checkmate)
  assertCharacter(mapping)
  assertString(column)
  lapply(dfs, function(df) {
    assertDataFrame(df)
    assertSubset(column, colnames(df))
  })

  # Automatically collect the names of input arguments in '...' via
  # 'substitute(list(...))', then remove the list information of the substitute object
  # via '[-1]' in order to get the names of the data frames alone and lastly convert them
  # to character strings via 'deparse'
  dfs_names <- sapply(substitute(list(...))[-1], deparse)

  # Translate the column according to inputted mapping.
  require(dplyr)
  dfs_cleaned <- lapply(dfs, function(df) {
    df[[column]] <- ifelse(df[[column]] %in% names(mapping),
                           mapping[df[[column]]], 
                           df[[column]])
    return(df)
  })

  # Assign original data frame names to the cleaned data sets
  names(dfs_cleaned) <- dfs_names

  # Return the adjusted data frame
  return(dfs_cleaned)
}


#' FUNCTION 6:
#' @description
#' The function 'prepare_join' takes data frames and aligns their columns in several
#' steps in order to simplify the aspired joining of the data frames afterwards. First it
#' removes the columns average' and 'Series Name' from each data frame and then converts
#' columns with column names in form of year format (i.e. YYYY) to numeric.
#'
#' Inputs:
#' @param ... One or more data frames containing - among others - the columns 'average'
#' and 'Series Name' as well as several columns with names representing different years
#' in the format 'YYYY'.
#'
#' Output:
#' @returns A list of the inputted data frames without the columns 'average' and 'Series
#' Name' and columns of year format (i.e. YYYY) of numeric type. Can then be used to
#' assign them as variables in a specified environment, using the list's names as the
#' variable names.
prepare_join <- function(...) {
  # Capture all data frames passed to the function via '...'
  dfs <- list(...)

  # Check for valid input
  require(checkmate)
  lapply(dfs, function(df) {
    assertDataFrame(df)
    assertSubset(c("Series Name","average"), colnames(df))
  })

  # Automatically collect the names of input arguments in '...' via
  # 'substitute(list(...))', then remove the list information of the substitute object
  # via '[-1]' in order to get the names of the data frames alone and lastly convert them
  # to character strings via 'deparse'
  dfs_names <- sapply(substitute(list(...))[-1], deparse)

  # Delete the column 'average' and 'Series Name' and ensure that all year columns are
  # numeric to to avoid data type issues while merging
  require(dplyr)
  dfs_cleaned <- lapply(dfs, function(df) {
    df <- df %>% 
      select(-c("average", "Series Name"))
    df <- df %>%
      mutate(across(matches("^[0-9]{4}$"), as.numeric))
    return(df)
  })

  # Assign original data frame names to the cleaned data sets
  names(dfs_cleaned) <- dfs_names

  # Return the adjusted data frame
  return(dfs_cleaned)
}


#' FUNCTION 7:
#' @description
#' The function 'kl_div_from_dnorm' takes a single data frame with a column 'Year'
#' and calculates the Kullback-Leibler divergence of the observed data of 'variable' from
#' the normal distribution with parameters based on the observed data for each year. 
#'
#' Inputs:
#' @param df One data frame containing containing the column 'Year' and one named
#' according to the input 'variable'.
#' @param variable A single string telling the function for which column it shall compute
#' the KL divergence as well as which variable's mean and standard deviation to use for
#' the calculation of the compared normal distribution.
#' @param kernel_bw A single integer used as the bandwidth for estimation of the kernel
#' density of the observed data. Will further be used to name the column according to the
#' following concept: 'KLD (BW = kernel_bw)' (e.g. kernel_bw = 2 --> 'KLD (BW = 2)').
#'
#' Output:
#' @returns A data frame of two columns, the column 'Year' with unique entries and another
#' one with the calculated Kullback-Leibler divergence for each of those years.
kl_div_from_dnorm <- function(df, variable, kernel_bw) {
  # Check for valid input
  require(checkmate)
  assertDataFrame(df)
  assertString(variable)
  assertSubset(c("Year", variable), colnames(df))
  assertNumber(kernel_bw)
  
  require(dplyr)
  df_adjusted <- df %>%
    group_by(Year) %>%
    summarize(kl_divergence = {
      # Extract right column to work with using '.data[[]]' to retain grouping structure
      column <- .data[[variable]]

      # Calculate the KDE values, i.e. the empirical distribution, for the observed data
      # points for each year
      kde <- density(column, bw = kernel_bw)

      # Calculate the normal distribution values, i.e. the PDF, based on mean and standard
      # deviation of the observed data for each year
      norm_distr <- dnorm(kde$x, mean = mean(column), sd = sd(column))

      # Compute the Kullback-Leibler divergence between the observed data and the normal
      # distribution based on the observed data to determine the accuracy of the normal
      # distribution to describe the observed data
      sum(log(kde$y / norm_distr) * kde$y)
    })

  # Adjust the column name of the Kullback-Leibler divergences
  colnames(df_adjusted)[2] <- paste0("KLD (BW = ", kernel_bw, ")")

  # Return the adjusted data frame
  return(df_adjusted)
}


#' FUNCTION 8:
#' @description
#' The function 'source_rmd' takes a file path to a .Rmd file and sources the file by
#' generating a temporary .R file from the .Rmd, sources this temporary file into the
#' by argument specified environment - by default the global environment. Afterwards it
#' deletes the temporarily generated .R file, so no changes to the files are done.
#' However, the code chunks inside the inputted .Rmd (via its file path) are now saved in
#' the specified environment and can be accessed outside the .Rmd file.
#'
#' Inputs:
#' @param file A single string stating the file path to .Rmd file, that shall be sourced.
#' @param envir A valid environment to save the assigned variables from the .Rmd file in.
#' If left empty, the default case of global environment is used.
#'
#' Output:
#' @returns Nothing per se. All assigned variables are sourced and save in the assigned
#' environment. 
source_rmd <- function(file, envir = globalenv()) {
  
  # Generate a temporary .R file from the .Rmd stored at the location, the inputted file
  # path is referring to
  require(knitr)
  temp_r_file <- knitr::purl(file, quiet = TRUE)
  
  # Source the temporarily generated .R file into the specified environment, so the in the
  # .Rmd file assigned variables are accessible afterwards
  source(temp_r_file, local = envir)
  
  # Delete the temporary .R file to clean up and not leave changes to the file structure
  # from before the function call
  unlink(temp_r_file)
}
