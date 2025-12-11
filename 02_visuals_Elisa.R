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
df_be <- read.csv("Belgium_export-nation.csv", sep = ";", header = TRUE)

# create folder if not existing
dir.create("plot")

# graph at national level
plot <- df_be %>% 
  ggplot(aes(x = date, y = value_load)) + 
  geom_point() + 
  labs(x = NULL, y = "SARS-CoV-2 viral to faecal ratio \n (10e-6 copies/copies)") + 
  scale_x_date(labels = function(x) {
    paste0(format(as.Date(x), "%d/%m/%y"), 
           " (W", 
           epiweek(as.Date(x)),
           ")")
  }) + 
  theme_bw()

plot

# save graph
ggsave("plot/graph-viral_ratio-nation.png")

# display msg
cat("- Success : visuals saved \n")
