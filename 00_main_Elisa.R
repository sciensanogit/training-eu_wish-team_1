############################################################################### #
# Aim ----
#| generating wastewater reports
# Requires: 
# NOTES:
#| git cheat: git status, git add -A, git commit -m "", git push, git pull, git restore
#|
############################################################################### #

# Load packages ----
# select packages
pkgs <- c("dplyr", "ggplot2")
# install packages
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))


# Mission 1.1 ----

# Commennted out to keep the code but not to run it
# files_to_source <- list.files(pattern = "mission1", full.names = TRUE)

# for (this_file in files_to_source) {
  source(this_file)
# }

# Session 3
# Correct filenames edited later

# Data prep 1
source("01_data_prep-solution.R")

# Visuals 2
source("02_visuals.R")

# Table 3
source("03_tbl.R")

# Quatro 4
source("04_quarto.R")
