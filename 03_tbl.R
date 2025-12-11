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
df_nation <- read.table(file = "./Belgium_export-nation.csv", sep = ";", dec = ".", header = T)

# Save table ----
tbl_nation <- df_nation %>%
  select(siteName, date, value_pmmv) %>%
  arrange(desc(date)) %>% 
  slice_head(n = 10) %>%
  flextable() %>%
  fontsize(part = "body",size = 10) %>%
  fontsize(part = "header",size = 10) %>%
  autofit() %>% theme_vanilla()

tbl_nation

# display msg
cat("- Success : tables saved \n")
