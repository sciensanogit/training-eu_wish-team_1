############################################################################### #
# Aim ----
#| load data and produce first graph
# NOTES:
#| list of things to do
#|
############################################################################### #

# Load packages ----
# select packages
pkgs <- c("dplyr", "ggplot2", "zoo")
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
df <- df_sc %>%
  rbind(df_pmmv)

# clean data
df <- df %>%
  select(siteName, collDTStart, labName, labProtocolID, flowRate, popServ, measure, value)

# format date
df$date <- as.POSIXct(df$collDTStart, format = "%Y-%m-%dT%H:%M:%S")
df$date <- format(df$date, "%Y-%m-%d")

# compute viral ratio
# unique(df$measure) ...

# graph
plot_data <- df %>%
  filter(labProtocolID == "SC_COV_4.1") %>%
  filter(measure == "SARS-CoV-2 E gene") %>%
  filter(date > "2024-09-01" & date < "2025-09-01") %>%
  filter(siteName %in% c("Aalst", "Oostende"))


plot_data <- plot_data %>% 
  group_by(siteName) %>% 
  mutate(moving_avg = rollmean(value, 14, align = "left", na.pad = T))

# rollmean(plot_data$value, 14)

plot <- plot_data %>% 
  ggplot(aes(x = date, y = value, group = siteName, color = siteName)) +
  geom_point(na.rm = T) +
  geom_line(na.rm = T) +
  geom_line(aes(x = date, y = moving_avg, group = siteName, color = siteName), linetype = "dotted") + 
  labs(x = NULL, y = "SARS-CoV-2 viral to faecal ratio \n (10e-6 copies/copies)")

plot

# save
ggsave(file="./plot/graph_oostende_aalst.png",
       plot, width = 21, height = 12, dpi = 200)
