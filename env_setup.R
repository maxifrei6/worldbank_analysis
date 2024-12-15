### Sourcing this file can help set up the environment needed for this project. ###

## Warning: The `interval` package requires supporting packages that do not belong
## to CRAN. It may cause problem due to different R versions.
## In the project, we use R 4.4.1.

## If problems with installing Icens and Interval occur, please check R version.

# Setting working directory
install.packages("rstudioapi")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Loading necessary packages
packages <- c("readxl",  # for reading .xlsx files
              "tidyr",
              "dplyr",
              "stringr",
              "checkmate",
              "lubridate",
              "ggplot2",
              "purrr",
              "knitr",
              "rnaturalearth",  # for worldmap design
              "rnaturalearthdata",  # for worldmap design
              "sf",  # for worldmap design
              "devtools",  # for loading packages outside of CRAN, e.g. GitHub
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
              "scales",  # for rescaling data to percentage scale within data barriers
              "ggthemes") ## add more packages if needed

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

##########################################################################################################
# TODO: HOW TO HANDLE MASS AND GRID AS THEY ARE BASE PACKAGE BUT HAVE TO BE LOADED - IN THE RMDs DIRECTLY?
##########################################################################################################
# TODO: HOW TO LOAD devtools::install_github("ricardo-bion/ggradar") AS IT COMES FROM GITHUB?
##########################################################################################################

# Loading data
folder_path <- "Data/Processed/"
rds_files <- list.files(path = folder_path, pattern = ".RDS$", full.names = TRUE, ignore.case = TRUE)
for (file in rds_files) {
  var_name <- gsub(".RDS", "", file, ignore.case = TRUE)
  var_name <- gsub("Data/Processed/", "", var_name)
  data <- readRDS(file)
  assign(var_name, data)
}

# If variables named with "/" at first, which often occurs for Mac users, rename them
for (var in ls()) {
  if (str_detect(var, "/")) {
    new_var <- gsub("/", "", var)
    assign(new_var, get(var))
    rm(var)
  }
}

# Remove unnecessary variables
rm(folder_path, rds_files, file, var_name, data, pkg, packages)

# Source functions
source("Scripts/functions.R")