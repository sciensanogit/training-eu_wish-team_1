############################################################################### #
# Aim ----
#| load, clean and save data
# NOTES:
#| git cheat: git status, git add -A, git commit -m "", git push, git pull, git restore
#| list of things to do...
############################################################################### #

# Load packages ----
# select packages
pkgs <- c("dplyr", "tidyr", "zoo", "writexl", "ggplot2", "stringr")
# install packages
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

# load data ----
# Belgian data are available here https://www.geo.be/catalog/details/9eec5acf-a2df-11ed-9952-186571a04de2?l=en
#| Metadata
#| siteName is the name of the treatment plant
#| collDTStart is the date of sampling
#| labName is the name of the lab analysing the sample
#| labProtocolID is the protocol used to analyse the dample
#| flowRate is the flow rate measured at the inlet of the treatment plant during sampling
#| popServ is the population covered by the treatment plant
#| measure is the target measured
#| value is the result

# sars-cov-2 data
df_sc <- read.csv("https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantscovid&outputFormat=csv")

# pmmv data
df_pmmv <- read.csv("https://data.geo.be/ws/sciensano/wfs?SERVICE=WFS&REQUEST=GetFeature&VERSION=2.0.0&TYPENAMES=sciensano:wastewatertreatmentplantspmmv&outputFormat=csv")

# join both
df_combine <- df_sc %>%
  rbind(df_pmmv)

# clean data
df <- df_combine %>%
  select(siteName, collDTStart, labName, labProtocolID, flowRate, popServ, quality, measure, value) %>% 
  rename(date = collDTStart) %>% 
  mutate(date = as.Date(date)) #%>% 
  # mutate(across(where(is.character), as.factor))

# format date
# df$date <- as.Date(df$date)

# set and subset dates
date_reporting <- as.Date("2025-09-01", format = "%Y-%m-%d")
date_graph_start <- as.Date("2024-09-01", format = "%Y-%m-%d")
date_graph_end <- as.Date("2025-12-01", format = "%Y-%m-%d")

# subset sars and pmmv data based on labProtocolID used betwen date_start and date_end

data <- df %>% 
  # filter(str_detect(labProtocolID, "COV")) %>% 
  filter(between(date, date_graph_start, date_graph_end)) %>% 

# display existing labProtocolID
# unique(df$labProtocolID)
# rename measures
  mutate(measure = if_else(str_detect(labProtocolID, "COV"), "SARS", "PPMV")) %>% 
# diplay existing measure
# unique(df$measure)

# translate siteName to english
# ("Bruxelles-Sud" to "Brussels-South", "Bruxelles-Nord" to "Brussels-North")
  mutate(siteName = str_replace(siteName, "Bruxelles-Sud", "Brussels-South"),
         siteName = str_replace(siteName, "Bruxelles-Nord", "Brussels-North"),
         # apply LOQ provided by the lab         
         value = if_else(measure == "SARS" & value < 8, NA, value),
         value = if_else(measure == "PPMV" & value < 250, NA, value),
# remove outliers
# remove outlier based on the variable named "Quality". Set samples with "Quality concerns" to NA
         value = if_else(quality == "Quality concerns", NA, value)) #%>% 
  # filter(!is.na(value))

# compute mean of replicated analysis of each measure
# - compute mean of replicated analysis of each measure using group_by() and summarize()
means <- data %>% 
  group_by(date, siteName, labProtocolID) %>% 
  summarise(mean_value = mean(value, na.rm = T))

# Merge this back in and replace on the values
  # - compute viral ratio (SARS/PMMV) in a variable named "value_pmmv" using pivot_longer()
  # - compute moving average on past 14 days
  # - aggregate data at national level by computing weighted mean with factor being the popServed by each site
calculations <- means %>% 
  left_join(data %>% select(-value, -labName)) %>% 
  distinct() %>% # still get a "duplication" of rows from the mean calc for some reason
  select(-labProtocolID) %>%
  pivot_wider(
    names_from = measure,
    values_from = mean_value
  )



# compute viral ratio

# compute moving average on past 14 days

# natinoal aggregation: compute weighted mean with factor being the population served by each site

# export data ----
# create folder if not existing

# export as csv

# export as xls

# export as rds


# display msg
cat("- Success : data prep \n")

