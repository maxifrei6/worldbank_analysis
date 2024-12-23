### Sourcing this file can help set up the environment needed for this project. ###

## Warning: The `interval` package requires supporting packages that do not belong
## to CRAN. It may cause problems due to different R versions.
## In the project, we use R 4.4.1.

# Install and load the `devtools` package
if (!require("devtools")) install.packages("devtools")
library(devtools)

# List of GitHub packages
github_packages <- list(
  "cran/Icens" = "Icens"           # Icens package from GitHub
)

# Install or update GitHub packages
for (repo in names(github_packages)) {
  pkg <- github_packages[[repo]]
  
  # Check if the package is installed
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Installing GitHub package:", pkg))
    devtools::install_github(repo, upgrade = "always")
    library(pkg, character.only = TRUE)
  }
}


# List of CRAN packages
packages <- c("readxl",  # for reading .xlsx files
              "tidyr",  # for pivoting data frames
              "dplyr",  # for cleansing purposes
              "stringr",
              "checkmate",  # for input validation checks
              "lubridate",
              "ggplot2",  # for plotting
              "purrr",
              "knitr",  # for globally setting up .html knitting
              "rnaturalearth",  # for worldmap design
              "rnaturalearthdata",  # for worldmap design
              "sf",  # for worldmap design
              "pROC",
              "rpart",
              "survival",
              "ggsurvfit",
              "survminer",
              "moments",
              "zoo",
              "kableExtra",
              "readr",
              "patchwork",  # for more options in assembling plots
              "epitools",
              "ggmosaic",
              "e1071",
              "RColorBrewer",  # for colorblind-friendly visualizations
              "icenReg",
              "interval",
              "scales",  # for re-scaling data to percentage scale within data barriers
              "ggthemes",
              "ggradar",  # for radar charts - TODO: GITHUB CONNECTION TO RICARDO BION FOR DOWNLOAD
              "here") ## add more packages if needed

# Install CRAN packages if missing
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Installing CRAN package:", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Set project root and load data
library(here)
message("Project root directory set by `here()`: ", here())

folder_path <- here("Data", "Processed")  # Relative path to the data folder
rds_files <- list.files(path = folder_path, pattern = ".RDS$", full.names = TRUE, ignore.case = TRUE)

for (file in rds_files) {
  var_name <- gsub(".RDS", "", file, ignore.case = TRUE)
  var_name <- gsub(paste0(folder_path, "/"), "", var_name)
  data <- readRDS(file)
  assign(var_name, data)
}

# Source project functions using `here()`
source(here("Scripts", "functions.R"))

# Clean up unnecessary variables
rm(folder_path, rds_files, file, var_name, data, pkg, packages, github_packages, repo)

# Inform the user to restart R if needed
message("If you experience any issues with loaded packages, please restart R and re-run this script.")
