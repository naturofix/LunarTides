#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  #titlePanel("Moon vs Tide — Simon’s Town"), 

    
      uiOutput("visible_slider"),
      
     
                           #div(style="margin: 8px 0;", textOutput("summary")),
                          #h4("Time Series"),
                           plotOutput("plot_ts", height = "420px"),
                           #tags$hr(),
                           #h4("Lag: High tide vs nearest Moon max/min"),
                           plotlyOutput("plot_high_moonlag"),
                           #stags$hr(),
                           # h4("Scatter"),
                           # plotlyOutput("plot_scatter", height = "300px"),
                           
                           helpText("Cached previous lunar cycle is used by default. Click Refresh to re-download."),
                           checkboxInput("use_cache", "Use cached previous lunar cycle (if available)", value = TRUE),
                           actionButton("refresh_cache", "Update)"),
                           tags$hr(),
                           
                           numericInput("interval", "Sampling interval (minutes, heights TS):", value = 30, min = 5, max = 120, step = 5),
                           selectInput("source", "Tide data source:",
                                       c("WorldTides API" = "worldtides", "Upload CSV (time,height)" = "csv")),
                           conditionalPanel(
                             "input.source == 'worldtides'",
                             textInput("wt_key", "WorldTides API key:", value = Sys.getenv("WORLDTIDES_KEY", DEFAULT_WORLDTIDES_KEY)),
                             helpText("Heights + extremes cost ~2 credits per 7-day chunk.")
                           ),
                           actionButton('debug','Debug')
  #     tabsetPanel(
  #                 tabPanel("Data",
  #                          p("Computed series at selected interval; local time (", strong(TZ), ")."),
  #                          DT::dataTableOutput("data_table")
  #                 ),
  #                 tabPanel("Events & Lags",
  #                          p("Clock‑time differences (minutes). Positive = tide occurs AFTER the lunar event."),
  #                          DT::dataTableOutput("lags_table")
  #                 ),
  #                 tabPanel("Spring vs Phase",
  #                          p("Daily tide range with dashed lines on days nearest to new/full Moon. Summary lag printed below."),
  #                          plotlyOutput("plot_spring", height = "360px"),
  #                          verbatimTextOutput("spring_summary")
  #                 )
  #    
  # )
)
