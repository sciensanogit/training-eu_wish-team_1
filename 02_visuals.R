############################################################################### #
# Aim ----
#| produce visuals
# NOTES:
#| git cheat: git status, git add -A, git commit -m "", git push, git pull, git restore
#| list of things to do...


## Adapt 02_visuals.R code to
#|- load data saved in .data/Belgium_export-nation.csv. This csv will be created by running 01_data_prep-solution.R. If you cannot run 01_data_prep-solution.R, you can load the ./Belgium_expert-nation.csv
#|- create ./plot folder if not existing
#|- create a figure with the viral ratio (variable called value_pmmv) at the national level (inspiration from mission2.R is welcome)
#|- save graph as ./plot/graph-viral_ratio-nation.png
#|- Display nice xaxis, yaxis
#|- Add a past two weeks moving average line from the variable  (variable called value_pmmv_avg14d_past)

############################################################################### #

# Load packages ----
# select packages
pkgs <- c("dplyr", "tidyr", "zoo", "writexl", "ggplot2","readr", "lubridate")
# install packages
# install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

# load data
df <- read_delim("data/Belgium_export-nation.csv")


df <- df %>%
  mutate(
    date = as_date(date),  # convert to Date (handles "YYYY-MM-DD")
    value_pmmv = as.numeric(value_pmmv),
    value_pmmvavg14d_past = as.numeric(value_pmmv_avg14d_past)
  )

# create folder if not existing
if (!dir.exists("./plot")) dir.create("./plot", recursive = TRUE)

# graph at national level


# Label function: decimal numbers, comma thousands
decimal_labels <- function(x) format(x, big.mark = ",", scientific = FALSE, nsmall = 2)

p <- ggplot(df %>% filter(date >= date_graph_start & date <= date_graph_end), aes(x = date)) +
  # PMMV points 
  geom_point( aes(y = value_pmmv, colour = "pmmv"), size = 1.8, shape = 16, alpha = 0.7, na.rm = TRUE ) +
  
  # PMMV 14-day past average line 
  geom_line( aes(y = value_pmmvavg14d_past, colour = "pmmv14"), linewidth = 1.1, na.rm = TRUE ) + 
  
  # Colour scale with matching keys 
  scale_colour_manual( name = NULL, values = c( pmmv = "#1f77b4", pmmv14 = "#ff7f0e" ), labels = c( pmmv = "PMMV", pmmv14 = "PMMV (14-day avg, past)" ) ) +
  
  scale_x_date( date_breaks = "4 weeks", date_labels = "%d/%m/%y" , limits = c(date_graph_start, date_graph_end)) +
  
  scale_y_continuous(labels = decimal_labels) +   # <--- decimals, no scientific
  
  labs(title = "SARS-CoV-2", x = "Date", y = "Viral ratio") +
  
  theme_minimal(base_size = 12) +
  
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 10),
    axis.title.x = element_text(margin = margin(t = 15))
  )


# save graph


ggsave(
  filename = "./plot/graph-viral_ratio-nation.png",
  plot = p,
  width = 9, height = 5, dpi = 300,
  bg = "white"             # <-- forces a white image background
)


# display msg
cat("- Success : visuals saved \n")
