############################################################################### #
# Aim ----
#| generating wastewater reports
# NOTES:
#| git cheat: git status, git add -A, git commit -m "", git push, git pull, git restore
############################################################################### #

# Load packages ----
pacman::p_load(
  dplyr,
  ggplot2,
  flextable,
  quarto
)

## Establishing parameters (or these are going to be pulled in from somewhere?)

# set dates
date_reporting <- as.Date("2025-09-01", format = "%Y-%m-%d")


# load epi assessment
main_text <- read.csv("epi_assessment_text.csv")

# Render weekly sub report ----
# save.image(".RData")

# settings
output_name <- paste0("Report-", format(date_reporting, "%G-W%V"))
format_output <- c("html")
# format_output <- c("html","docx")

quarto_render(input = "04_quarto.qmd",
              output_file = output_name,
              output_format = format_output)

cat("- Success : quarto render \n")
