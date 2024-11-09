### This program cleans the original data sets and saves modified data sets for future utilization. ###

## OUTLINE
# Data preparation
  # Load data
  # Initial NA removal
  # Data overview

# Data modification

# TODO

# ...

# Final steps 
  # Save cleaned datasets for future use

library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(checkmate)
source("Scripts/functions.R")

# Read the csv file from Data/Raw folder
geodata <- read.csv("Data/Raw/Worldbank2.csv", header = TRUE, sep = ",", na.strings = "")

# Read the xlsx file from Data/Row folder
sociometrics <- read_excel("Data/Raw/Worldbank1.xlsx", sheet = 1, col_names = TRUE, na =)

# Delete columns or rows if they are all NAs.
# Apply the remove_NAs function to all interested datasets
geodata <- remove_NA(geodata)
sociometrics <- remove_NA(sociometrics)

# Get an overview of the datasets
str(geodata)
summary(geodata)
str(sociometrics)
summary(sociometrics)