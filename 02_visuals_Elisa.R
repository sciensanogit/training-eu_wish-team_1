############################################################################### #
# Aim ----
#| produce visuals
# NOTES:
#| git cheat: git status, git add -A, git commit -m "", git push, git pull, git restore
#| list of things to do...
############################################################################### #

# Load packages ----
# select packages
pkgs <- c("dplyr", "tidyr", "zoo", "writexl", "ggplot2")
# install packages
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

# load data
df_be <- read.csv("data/Belgium_export-nation.csv", sep = ";", header = TRUE)

# create folder if not existing
dir.create("plot")

#### Additional nice plot theme to apply later
theme_custom <- theme(
  plot.title = element_text(color = "darkgreen", size = 16, hjust = 0.5),
  
  # Backgrounds
  plot.background  = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = NA),
  
  # Axis titles and labels in light grey
  axis.title = element_text(color = "grey50"),
  axis.text  = element_text(color = "grey50"),
  
  # Axis lines
  axis.line = element_line(color = "grey80"),
  
)

# graph at national level

# Change dates
df_be$date <- as.Date(df_be$date)

# Define thresholds and labels to use in sometime in the future
level_values <- c(1e+12, 2e+12, 4e+12)                   # y-axis positions of lines
level_labels <- c("Moderate level", "High level", "Very high level")  # labels

plot <- df_be %>% 
  ggplot(aes(x = date, y = value_load)) + 
  geom_point(color = "darkgreen") +
  geom_line(data = df_be, aes(x = date, y = value_load_avg14d_past), color = "green", na.rm = TRUE) + 
  ggtitle("SARS-Cov-2") +
  labs(x = NULL, y = "SARS-CoV-2 viral to faecal ratio \n (10e-6 copies/copies)") + 
  theme_custom

plot

# save graph
ggsave("plot/graph_oostend_aalst.png")

# display msg
cat("- Success : visuals saved \n")
