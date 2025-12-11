############################################################################### #
# Aim ----
#| produce tables
# NOTES:
#| git cheat: git status, git add -A, git commit -m "", git push, git pull, git restore
#| list of things to do...
############################################################################### #

# Load packages ----
install.packages("pacman")
pacman::p_load(
  dplyr,
  ggplot2,
  flextable,
  quatro
)

# load data
# COMEBACK - to be updated to file created by Patrick
df_nation <- read.table(file = "./Belgium_export-nation.csv", sep = ";", dec = ".", header = T)

# Save table ----
# tbl_nation <- 
df_nation %>%
  select(siteName, date, value_pmmv) %>%
  mutate(date = as.Date(date)) %>%
  filter(
    # note that date_reporting is specified in 01_data_prep.R
    date <= date_reporting &
    # displaying only sampling days which are Mondays
    is.na(value_pmmv) == FALSE
  ) %>% 
  arrange(desc(date)) %>% 
  # selecting last 10 data points
  slice_head(n = 10) %>%
  flextable() %>%
  fontsize(part = "body",size = 10) %>%
  fontsize(part = "header",size = 10) %>%
  autofit() %>% 
  theme_vanilla() %>%
  set_header_labels(
    siteName = "Site Name",
    date = "Date",
    value_pmmv = "Viral ratio (SARS/PMMV)"
  ) %>%
  # Australian CDC style
  theme_vanilla() %>%
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 9) %>%
  bg(bg = "#00DCA1", part = "header") %>%
  color(color = "#033636", part = "all")
  
# tbl_nation

# display msg
cat("- Success : tables saved \n")
