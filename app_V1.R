# load packages ----
pkgs <- c("shiny", "plotly", "ggplot2", "dplyr")
install.packages(setdiff(pkgs, rownames(installed.packages())))
invisible(lapply(pkgs, FUN = library, character.only = TRUE))

# load data ----
# load nation data
df <- df_nation <- read.csv2("./data/Belgium_export-nation.csv", sep = ";") %>%
  select(-value_avg14d_center, -value_load_avg14d_center, -value_pmmv_avg14d_center) %>%
  mutate(date = as.Date(date))

# load the data by site and select appropriate variables
df_site <- read.csv2("./data/Belgium_export-site.csv", sep = ";") %>%
  select(any_of(colnames(df_nation))) %>%
  mutate(date = as.Date(date),
         across(contains("value"), ~ as.numeric(.x)))

# bind nation and site data
df <- rbind(df_nation, df_site)

# clean data
df$date <- as.POSIXct(df$date)
df$value_pmmv <- as.numeric(df$value_pmmv)*1000
df$value_pmmv_avg14d_past <- as.numeric(df$value_pmmv_avg14d_past)*1000

df <- df %>%
  filter(measure == "SARS")

# ui ----
ui <- navbarPage(
  title = "Wastewater surveillance",
  
  tabPanel(
    "About",
    # Custom CSS for green header bar
    tags$style(HTML("
    .top-bar {
      background-color: #e8f5e9;
      padding: 15px;
      margin-bottom: 20px;
      border-radius: 5px;
      color: white;
    }
    .top-bar label {
      color: white;
      font-weight: bold;
    }
    .info-box {
      background-color: #4CAF50;
      padding: 15px;
      border-radius: 5px;
      text-align: center;
      font-weight: bold;
      border: 1px solid #c8e6c9;
      margin-bottom: 20px;
    }
  ")),
    
    # Bottom acknowledgment box
    div(class = "info-box",
        "Welcome to this nice page describing our wastewater surveillance program." ),
    
    p("Hello, welcome to Group 1's Shiny. We are learning how to present data so please be gentle in your feedback.")
  ),
  
  
  tabPanel(
    "SARS-CoV-2",
    # Bottom acknowledgment box
    div(class = "info-box",
        "Our SARS-CoV-2 surveillance program identifies changes in community level transmission of SARS-CoV-2, the pathogen causing COVID-19." )
    ,
    
    # Three horizontal info boxes
    fluidRow(
      column(4, div(class = "info-box", "Number of site = 30")),
      column(4, div(class = "info-box", "Next sampling date = Wednesay")),
      column(4, div(class = "info-box", "Next dashboard update = Monday")
      )
    ),
    
    # Top bar with dropdown
    div(class = "top-bar",
        selectInput(
          inputId = "site",
          label = "Select sampling site",
          choices = unique(df$siteName),
          selected = "Belgium"
        )
    ),
    
    # Main content
    plotlyOutput("viralPlot")
    ,
    
    #textOutput("test_text"),
    #tableOutput("test_df"),
    
    # Bottom acknowledgment box
    div(class = "info-box",
        "Acknowledgment: Group 1 of the EU WISH training consists of Elisa Salmivirta, Sophie-Berencie Wilmes, Kim Nguyen, and Gizem Bilgin" )
    
  ),
  
  tabPanel(
    "Influenza",
    div(class = "info-box",
        "We will visualise influenza data once it becomes available in 2027." )
  )
  
  
)

# server ----
server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df %>% 
      filter(siteName == input$site &
                    is.na(date) == FALSE)
  })
  
  output$test_text <- renderText({
    this_data <- filtered_data()
    nrow(this_data)
  })
  
  output$test_df <- renderTable({
    filtered_data()
  })
  
  output$viralPlot <- renderPlotly({
    
    this_data <- filtered_data()
    
    p <- ggplot(this_data, aes(x = date)) +
      geom_point(
        aes(y = value_pmmv),
        colour = "#92D050",
        size = 1,
        na.rm = TRUE
      ) +
      geom_line(
        aes(y = value_pmmv_avg14d_past),
        colour = "#BCCF00FF",
        linewidth = 0.9,
        na.rm = TRUE
      ) +
      scale_y_continuous(
        limits = c(0, NA),
        expand = expansion(mult = c(0, 0.05))
      ) +
      scale_x_date(
        date_breaks = "3 months",
        date_labels = "%b %Y",
        expand = expansion(mult = c(0.01, 0.01))
      ) +
      labs(
        title = input$site,
        x = "",
        y = "Concentration (c/c)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.line = element_line(colour = "black", linewidth = 0.6)
      )
    
    ggplotly(p)
  })
}

# shinyApp ----
shinyApp(ui, server)