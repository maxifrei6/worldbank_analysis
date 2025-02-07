### Sourcing this file can help set up the environment needed for this project. ###

# Install and load the 'devtools' package
if (!require("devtools")) install.packages("devtools")
library(devtools)

# Vector of GitHub packages
github_packages <- c("ricardo-bion/ggradar")

# Install or update GitHub packages
for (packages in github_packages) {
  # Extract package name from the repository string
  pkg <- sub(".*/", "", packages)

  # Check if the package is installed
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Installing GitHub package:", pkg))
    devtools::install_github(packages, upgrade = "always")
    library(pkg, character.only = TRUE)
  }
}

# List of CRAN packages
packages <- c(
  # Install packages for rendering Quarto/R Markdowns
  "base64enc",
  "bslib",
  "cli",
  "digest",
  "evaluate",
  "fastmap",
  "fontawesome",
  "glue",
  "highr",
  "htmltools",
  "jquerylib",
  "jsonlite",
  "knitr",
  "lifecycle",
  "magrittr",
  "mime",
  "rlang",
  "rmarkdown",
  "stringi",
  "stringr",
  "tinytex",
  "vctrs",
  "xfun",
  "yaml",
  # Install package for input validation checks
  "checkmate",
  # Install package for cleansing purposes and pipelines
  "dplyr",
  # Install package for plotting
  "ggplot2",
  # Install package for converting tables to graphical objects
  "gridExtra",
  # Install package for relative path usage
  "here",
  # Install package for more options in assembling plots
  "patchwork",
  # Install package for colorblind-friendly visualizations
  "RColorBrewer",
  # Install package for reading .xlsx files
  "readxl",
  # Install package for re-scaling data to percentage scale within data barriers
  "scales",
  # Install package for pivoting data frames
  "tidyr"
  )

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

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
rds_files <-
  list.files(path = folder_path, pattern = ".RDS$", full.names = TRUE, ignore.case = TRUE)

for (file in rds_files) {
  var_name <- gsub(".RDS", "", file, ignore.case = TRUE)
  var_name <- gsub(paste0(folder_path, "/"), "", var_name)
  data <- readRDS(file)
  assign(var_name, data)
}

# Source project functions using 'here()'
source(here("Scripts", "functions.R"))

# Clean up unnecessary variables
rm(folder_path, rds_files, file, var_name, data, pkg, packages, github_packages)

# Inform the user to restart R if needed
message("If you experience any issues with loaded packages,
        please restart R and re-run this script.")
