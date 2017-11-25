# Made by Riley Waters, Nov 2017 for COSC406 Environmentrics
# Calgary Environmetrics App


# Automatically makes sure all required packages are installed and attached
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, DT, ggplot2)

# Read in the cleaned csv
calgaryAll <- read.csv("calgary_all_CLEANED.csv", header = TRUE)
calgaryAll$Date <- as.Date(calgaryAll$Date)
calgaryAll$Month <- factor(calgaryAll$Month,
                           levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           labels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
                           );

#=====================================================================UI=====================================================================
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Calgary Weather"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("question-circle-o")),
        menuItem("Data Explorer", tabName = "dataTable", icon = icon("table")),
        menuItem("Visualizations", tabName = "visualizations", icon = icon("bar-chart")),
        menuItem("Prediction Models", tabName = "predictionModels", icon = icon("eye")),
        menuItem("Forecast", tabName = "forecast", icon = icon("line-chart")),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("desktop"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML('.main-header .logo {
              font-family: "Source Sans Pro";
              font-weight: bold;
              font-size: 24px;'
        )
      )
    ),
    tabItems(
      
      # About content
      tabItem(tabName = "about",
              fluidRow(
                h1("Calgary", 
                 span("Environmetrics App", style = "font-weight: 300"), 
                 style = "font-family: 'Source Sans Pro';
                          color: #fff;
                          text-align: center;
                          background-image: url('texturebg.png');
                          padding: 20px"
                ),
                br()
              ),
              fluidRow(
                column(6, offset = 3,
                 p(style = "font-family: 'Source Sans Pro'; font-size: 20px",
                   "This is a web application for interacting with historical weather data from Calgary, Alberta. 
                   All data was collected at the",
                   a("CALGARY INT'L CS ALBERTA", href = "http://climate.weather.gc.ca/"),
                   "weather station. For detailed information about each variable, please visit the",
                   a("glossary", href = "http://climate.weather.gc.ca/glossary_e.html"), ".",
                   br(),br(),
                   "By clicking the tabs on the left, you can:"
                 ),
                 tags$ul(style = "font-family: 'Source Sans Pro'; font-size: 18px",
                   tags$li("Sort and filter the data table"), 
                   tags$li("Interact with data visualizations"), 
                   tags$li("Make predictions for future metrics")
                 ),
                 br(),
                 tags$i(style = "font-family: 'Source Sans Pro'; font-size: 20px",
                        "App created by Riley Waters for COSC406 Environmentrics.",
                        br(),
                        "Data last updated on November 23, 2017."
                 )
                )
              ),
              br(),br(),
              div(img(src = "skyline.jpg"), style="text-align: center;")
      ),
      
      # Visualizations tab content
      tabItem(tabName = "dataTable",
              fluidRow(
                column(4, selectInput("year","Year:", c("All",unique(as.character(calgaryAll$Year))))
                ),
                column(4,selectInput("month","Month:", c("All",unique(as.character(calgaryAll$Month))))
                ),
                column(4,selectInput("day","Day:", c("All",unique(as.character(calgaryAll$Day))))
                )
              ),
              fluidRow(
                DT::dataTableOutput("explorerOut")
              ),
              br(),br(),
              fluidRow(
                tags$i(style = "font-family: 'Source Sans Pro'; font-size: 16px",
                       "MaxTemp, MinTemp, MeanTemp, HeatDegDays, CoolDegDays:", tags$strong("Celcius"),
                       br(),
                       "SnowOnGrnd, TotalSnow:", tags$strong("cm"),
                       br(),
                       "TotalRain, TotalPrecip:", tags$strong("mm"),
                       br(),
                       "DirOfMaxGust:", tags$strong("Tens of degrees"),
                       br(),
                       "SpdOfMaxGust:", tags$strong("km/h")
                )
              )
              
      ),
      
      # Visualizations tab content
      tabItem(tabName = "visualizations"
          
      ),
      
      # Prediction model tab content
      tabItem(tabName = "predictionModels"
      
      ),
      
      # Forecasting tab content
      tabItem(tabName = "forecast"
        
      ),
      
      # Dashboard content
      tabItem(tabName = "dashboard"
              
      )
      
    )
  )
)


#=====================================================================SERVER=====================================================================
server <- function(input, output) {
  
  
  
  # Summary components
  output$explorerOut <- DT::renderDataTable(DT::datatable(
    options = list(pageLength = 12),
    {tableData <- calgaryAll
    if (input$year != "All") {
      tableData <- tableData[tableData$Year == input$year,]
    }
    if (input$month != "All") {
      tableData <- tableData[tableData$Month == input$month,]
    }
    if (input$day != "All") {
      tableData <- tableData[tableData$Day == input$day,]
    }
    tableData
    }
  ))
  
  
}

shinyApp(ui, server)