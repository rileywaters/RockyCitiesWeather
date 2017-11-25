# Made by Riley Waters, Nov 2017 for COSC406 Environmentrics
# Calgary Environmetrics App


# Automatically makes sure all required packages are installed and attached
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, DT, ggplot2)

# Read in the cleaned csvs
vancouverAll <- read.csv("vancouver_all_CLEANED.csv", header = TRUE)
vancouverAll$Date.Time <- as.Date(vancouverAll$Date.Time)
vancouverAll$Month <- factor(vancouverAll$Month,
                             levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                             labels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
);
kelownaAll <- read.csv("kelowna_all_CLEANED.csv", header = TRUE)
kelownaAll$Date.Time <- as.Date(kelownaAll$Date.Time)
kelownaAll$Month <- factor(kelownaAll$Month,
                           levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           labels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
);
calgaryAll <- read.csv("calgary_all_CLEANED.csv", header = TRUE)
calgaryAll$Date.Time <- as.Date(calgaryAll$Date.Time)
calgaryAll$Month <- factor(calgaryAll$Month,
                           levels = c(1,2,3,4,5,6,7,8,9,10,11,12),
                           labels = c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
);

#=====================================================================UI=====================================================================
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Rocky Cities App"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("question-circle-o")),
        menuItem("Data Explorer", tabName = "dataTable", icon = icon("table"),
                 menuSubItem("Vancouver", tabName = "vancouverTable"),
                 menuSubItem("Kelowna", tabName = "kelownaTable"),
                 menuSubItem("Calgary", tabName = "calgaryTable")
                 
        ),
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
              font-size: 24px;'
        )
      )
    ),
    tabItems(
      
      # About content
      tabItem(tabName = "about",
              fluidRow(
                h1("Rocky Cities", 
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
                 p(style = "font-family: 'Source Sans Pro'; font-size: 18px",
                   "This is an application for interacting with historical weather data from Vancouver BC, Kelowna BC, and Calgary AB. 
                   Despite being close in location, these cities experience unique climates due their positions relative to the Pacific Ocean, Okanagan Valley, Rocky Mountains, and Canadian Praries.
                   Data was collected by",
                   a("Climate Canada", href = "http://climate.weather.gc.ca/"),
                   "at VANCOUVER INTL, KELOWNA, and CALGARY INTL weather stations. For detailed information about each variable, please visit the",
                   a("glossary", href = "http://climate.weather.gc.ca/glossary_e.html"), ".",
                   br(),br(),
                   "By clicking the tabs on the left, you can:"
                 ),
                 tags$ul(style = "font-family: 'Source Sans Pro'; font-size: 16px",
                   tags$li("Sort and filter the data tables"), 
                   tags$li("Interact with data visualizations"), 
                   tags$li("Make predictions for future metrics")
                 ),
                 br(),
                 tags$i(style = "font-family: 'Source Sans Pro'; font-size: 16px",
                        "App created by Riley Waters for COSC406 Environmentrics.",
                        br(),
                        "Data cleaned and compiled by Riley Waters. Last updated on November 23, 2017."
                 )
                )
              ),
              br(),br(),
              div(img(src = "skyline.jpg"), style="text-align: center;")
      ),
      
      # Data Explorer tab content
      tabItem(tabName = "vancouverTable",
              fluidRow(
                column(4, selectInput("yearV","Year:", c("All",unique(as.character(vancouverAll$Year))))
                ),
                column(4,selectInput("monthV","Month:", c("All",unique(as.character(vancouverAll$Month))))
                ),
                column(4,selectInput("dayV","Day:", c("All",unique(as.character(vancouverAll$Day))))
                )
              ),
              fluidRow(
                DT::dataTableOutput("vancouverOut")
              ),
              br(),br(),
              fluidRow(
                tags$i(style = "font-family: 'Source Sans Pro'; font-size: 16px",
                       "Max.Temp, Min.Temp, Mean.Temp, Heat.Deg.Days, Cool.Deg.Days:", tags$strong("Celcius"),
                       br(),
                       "Total.Rain, Total.Precip:", tags$strong("mm"),
                       br(),
                       "Snow.on.Grnd, Total.Snow:", tags$strong("cm"),
                       br(),
                       "Dir.of.MaxGust:", tags$strong("Tens of degrees"),
                       br(),
                       "Spd.of.MaxGust:", tags$strong("km/h")
                )
              )
      ),
      tabItem(tabName = "kelownaTable",
              fluidRow(
                column(4, selectInput("yearK","Year:", c("All",unique(as.character(kelownaAll$Year))))
                ),
                column(4,selectInput("monthK","Month:", c("All",unique(as.character(kelownaAll$Month))))
                ),
                column(4,selectInput("dayK","Day:", c("All",unique(as.character(kelownaAll$Day))))
                )
              ),
              fluidRow(
                DT::dataTableOutput("kelownaOut")
              ),
              br(),br(),
              fluidRow(
                tags$i(style = "font-family: 'Source Sans Pro'; font-size: 16px",
                       "Max.Temp, Min.Temp, Mean.Temp, Heat.Deg.Days, Cool.Deg.Days:", tags$strong("Celcius"),
                       br(),
                       "Total.Rain, Total.Precip:", tags$strong("mm"),
                       br(),
                       "Snow.on.Grnd, Total.Snow:", tags$strong("cm"),
                       br(),
                       "Dir.of.MaxGust:", tags$strong("Tens of degrees"),
                       br(),
                       "Spd.of.MaxGust:", tags$strong("km/h")
                )
              )
      ),
      tabItem(tabName = "calgaryTable",
              fluidRow(
                column(4, selectInput("yearC","Year:", c("All",unique(as.character(calgaryAll$Year))))
                ),
                column(4,selectInput("monthC","Month:", c("All",unique(as.character(calgaryAll$Month))))
                ),
                column(4,selectInput("dayC","Day:", c("All",unique(as.character(calgaryAll$Day))))
                )
              ),
              fluidRow(
                DT::dataTableOutput("calgaryOut")
              ),
              br(),br(),
              fluidRow(
                tags$i(style = "font-family: 'Source Sans Pro'; font-size: 16px",
                       "Max.Temp, Min.Temp, Mean.Temp, Heat.Deg.Days, Cool.Deg.Days:", tags$strong("Celcius"),
                       br(),
                       "Total.Rain, Total.Precip:", tags$strong("mm"),
                       br(),
                       "Snow.on.Grnd, Total.Snow:", tags$strong("cm"),
                       br(),
                       "Dir.of.MaxGust:", tags$strong("Tens of degrees"),
                       br(),
                       "Spd.of.MaxGust:", tags$strong("km/h")
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
  
  
  
  # Data Explorer components
  output$calgaryOut <- DT::renderDataTable(DT::datatable(
    options = list(pageLength = 12),
    {tableData <- calgaryAll
    if (input$yearC != "All") {
      tableData <- tableData[tableData$Year == input$yearC,]
    }
    if (input$monthC != "All") {
      tableData <- tableData[tableData$Month == input$monthC,]
    }
    if (input$dayC != "All") {
      tableData <- tableData[tableData$Day == input$dayC,]
    }
    tableData
    }
  ))
  output$kelownaOut <- DT::renderDataTable(DT::datatable(
    options = list(pageLength = 12),
    {tableData <- kelownaAll
    if (input$yearK != "All") {
      tableData <- tableData[tableData$Year == input$yearK,]
    }
    if (input$monthK != "All") {
      tableData <- tableData[tableData$Month == input$monthK,]
    }
    if (input$dayK != "All") {
      tableData <- tableData[tableData$Day == input$dayK,]
    }
    tableData
    }
  ))
  output$vancouverOut <- DT::renderDataTable(DT::datatable(
    options = list(pageLength = 12),
    {tableData <- vancouverAll
    if (input$yearV != "All") {
      tableData <- tableData[tableData$Year == input$yearV,]
    }
    if (input$monthV != "All") {
      tableData <- tableData[tableData$Month == input$monthV,]
    }
    if (input$dayV != "All") {
      tableData <- tableData[tableData$Day == input$dayV,]
    }
    tableData
    }
  ))
  
  
}

shinyApp(ui, server)