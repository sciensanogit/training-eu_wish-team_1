############################################################################### #
# Aim ----
#| produce tables
# NOTES:
#| git cheat: git status, git add -A, git commit -m "", git push, git pull, git restore
#| list of things to do...
############################################################################### #

# Load packages ----
# install.packages("pacman")
pacman::p_load(
  dplyr,
  ggplot2,
  flextable
)

# load data
df_nation <- read.table(file = "./Belgium_export-nation-sc_flu_rsv-fake.csv", sep = ";", dec = ".", header = T)

# Save table ----
df_nation_0 <- df_nation %>%
  select(date, value_pmmv) %>%
  mutate(date = as.Date(date)) %>%
  filter(
    # note that date_reporting is specified in 01_data_prep.R
    date <= date_reporting &
    # displaying only sampling days which are Mondays
    is.na(value_pmmv) == FALSE
  ) %>% 
  arrange(desc(date)) %>% 
  # selecting last 10 data points
  slice_head(n = 10)

tbl_nation <- df_nation_0  %>%
  # make scientific notation
  mutate(value_pmmv = format(value_pmmv, scientific = TRUE, digits = 3),
         date = format(date, "%d %b %Y")) %>%
  # make flextable
  flextable() %>%
  autofit() %>% 
  set_header_labels(
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

saveRDS(tbl_nation, "tbl_nation.rds")

# display msg
cat("- Success : tables saved \n")

#Repeat for RSV -- SESSION 4
df_rsv <- df_nation %>%
  filter(measure == "RSV") %>% 
  ungroup() %>%
  select(date, value_pmmv) %>%
  mutate(date = as.Date(date)) %>%
  filter(
    # note that date_reporting is specified in 01_data_prep.R
    date <= date_reporting &
      # displaying only sampling days which are Mondays
      is.na(value_pmmv) == FALSE
  ) %>%
  arrange(desc(date)) %>% 
  # selecting last 10 data points
  slice_head(n = 10)

tbl_rsv <- df_rsv  %>%
  # make scientific notation
  mutate(value_pmmv = format(value_pmmv, scientific = TRUE, digits = 3),
         date = format(date, "%d %b %Y")) %>%
  # make flextable
  flextable() %>%
  autofit() %>% 
  set_header_labels(
    date = "Date",
    value_pmmv = "Viral ratio (RSV/PMMV)"
  ) %>%
  # Australia CDC style rip of with THL colors
  theme_vanilla() %>%
  font(fontname = "Work Sans", part = "all") %>%
  fontsize(size = 9) %>%
  bg(bg = "#005A1E", part = "header") %>%
  color(color = "#1E1E1E", part = "all") %>%
  hline(border = fp_border_default(color = "#005A1E")) %>%
  hline_top(border = fp_border_default(color = "#005A1E"), part = "header") %>%
  hline_bottom(border = fp_border_default(color = "#005A1E"), part = "header") %>%
  set_table_properties(layout = "autofit", width = 1) %>%
  align(align = "left", part = "all") %>%
  align(align = "right", j = -c(1), part = "body")

saveRDS(tbl_rsv, "tbl_rsv.rds")

#Repeat for INFLUENZA -- SESSION 4
df_flu <- df_nation %>%
  filter(measure == "Influenza") %>% 
  ungroup() %>%
  select(date, value_pmmv) %>%
  mutate(date = as.Date(date)) %>%
  filter(
    # note that date_reporting is specified in 01_data_prep.R
    date <= date_reporting &
      # displaying only sampling days which are Mondays
      is.na(value_pmmv) == FALSE
  ) %>%
  arrange(desc(date)) %>% 
  # selecting last 10 data points
  slice_head(n = 10)

tbl_flu <- df_flu  %>%
  # make scientific notation
  mutate(value_pmmv = format(value_pmmv, scientific = TRUE, digits = 3),
         date = format(date, "%d %b %Y")) %>%
  # make flextable
  flextable() %>%
  autofit() %>% 
  set_header_labels(
    date = "Date",
    value_pmmv = "Viral ratio (influenza/PMMV)"
  ) %>%
  # Australia CDC style rip of with THL colors
  theme_vanilla() %>%
  font(fontname = "Work Sans", part = "all") %>%
  fontsize(size = 9) %>%
  bg(bg = "#005A1E", part = "header") %>%
  color(color = "#1E1E1E", part = "all") %>%
  hline(border = fp_border_default(color = "#005A1E")) %>%
  hline_top(border = fp_border_default(color = "#005A1E"), part = "header") %>%
  hline_bottom(border = fp_border_default(color = "#005A1E"), part = "header") %>%
  set_table_properties(layout = "autofit", width = 1) %>%
  align(align = "left", part = "all") %>%
  align(align = "right", j = -c(1), part = "body")

saveRDS(tbl_flu, "tbl_flu.rds")

#Repeat for only SARS -- SESSION 4
df_sars <- df_nation %>%
  filter(measure == "SARS-CoV-2") %>% 
  ungroup() %>%
  select(date, value_pmmv) %>%
  mutate(date = as.Date(date)) %>%
  filter(
    # note that date_reporting is specified in 01_data_prep.R
    date <= date_reporting &
      # displaying only sampling days which are Mondays
      is.na(value_pmmv) == FALSE
  ) %>%
  arrange(desc(date)) %>% 
  # selecting last 10 data points
  slice_head(n = 10)

tbl_sars <- df_sars  %>%
  # make scientific notation
  mutate(value_pmmv = format(value_pmmv, scientific = TRUE, digits = 3),
         date = format(date, "%d %b %Y")) %>%
  # make flextable
  flextable() %>%
  autofit() %>% 
  set_header_labels(
    date = "Date",
    value_pmmv = "Viral ratio (SARS-2/PMMV)"
  ) %>%
  # Australia CDC style rip of with THL colors
  theme_vanilla() %>%
  font(fontname = "Work Sans", part = "all") %>%
  fontsize(size = 9) %>%
  bg(bg = "#005A1E", part = "header") %>%
  color(color = "#1E1E1E", part = "all") %>%
  hline(border = fp_border_default(color = "#005A1E")) %>%
  hline_top(border = fp_border_default(color = "#005A1E"), part = "header") %>%
  hline_bottom(border = fp_border_default(color = "#005A1E"), part = "header") %>%
  set_table_properties(layout = "autofit", width = 1) %>%
  align(align = "left", part = "all") %>%
  align(align = "right", j = -c(1), part = "body")

saveRDS(tbl_sars, "tbl_sars.rds")

# Repeat for a specific site -- SESSION 3
df_site_0 <- df_site %>%
  filter(siteName == "Brussels-North") %>%
  ungroup() %>%
  select(date, value_pmmv) %>%
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
  rename(this_value_pmmv = value_pmmv) %>%
  left_join(df_nation_0) %>%
  rename(national_value_pmmv = value_pmmv) %>%
  filter(is.na(national_value_pmmv) == FALSE) %>%
  mutate(diff = this_value_pmmv - national_value_pmmv)

df_site_flextable <- df_site_0 %>%
  mutate(value_pmmv = format(this_value_pmmv, scientific = TRUE, digits = 3),
         diff = format(diff, scientific = TRUE, digits = 3),
         date = format(date, "%d %b %Y")) %>%
  select(date, value_pmmv, diff) %>%
  # make flextable
  flextable() %>%
  autofit() %>% 
  set_header_labels(
    date = "Date",
    value_pmmv = "Viral ratio (SARS/PMMV)",
    diff = "Difference from national"
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


saveRDS(df_site_flextable, "site_specific_table.rds")
