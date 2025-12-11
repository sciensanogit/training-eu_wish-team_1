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
  flextable
)

# load data
# COMEBACK - to be updated to file created by Patrick
df_nation <- read.table(file = "./Belgium_export-nation.csv", sep = ";", dec = ".", header = T)

# Save table ----
tbl_nation <- df_nation %>%
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
  # make scientific notation
  mutate(value_pmmv = format(value_pmmv, scientific = TRUE, digits = 3)) %>%
  # make flextable
  flextable() %>%
  autofit() %>% 
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
  color(color = "#033636", part = "all") %>%
  hline(border = fp_border_default(color = "#00DCA1")) %>%
  hline_top(border = fp_border_default(color = "#00DCA1"), part = "header") %>%
  hline_bottom(border = fp_border_default(color = "#00DCA1"), part = "header") %>%
  set_table_properties(layout = "autofit", width = 1) %>%
  align(align = "left", part = "all") %>%
  align(align = "right", j = -c(1), part = "body")
  
tbl_nation

saveRDS(tbl_nation, "tbl_nation.rds")

# display msg
cat("- Success : tables saved \n")
