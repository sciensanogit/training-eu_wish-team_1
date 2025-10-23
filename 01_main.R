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

# set dates ----
date_start <- "2024-06-10"
date_graph_end <- "2026-08-25"
date_switch <- "2025-09-01"


# Source member 1 script ----
source("02_test_member_1.R")

# Source member 2 script ----
source("02_test_member_2.R")



