# Made by Riley Waters, Nov 2017 for STAT406 Environmentrics
# Rocky Cities Environmetrics App


# Automatically makes sure all required packages are installed and attached
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, DT, ggplot2)

# Read in the cleaned csv
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
monthList <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
locList <- c("VAN", "KEL", "CAL")
DataAll <- read.csv("all_CLEANED.csv", header = TRUE)
DataAll$Date.Time <- as.Date(DataAll$Date.Time)
DataAll$Month <- factor(DataAll$Month, levels = c(1,2,3,4,5,6,7,8,9,10,11,12), labels = monthList);
DataAll$City <- factor(DataAll$City, levels = c(1,2,3), labels = locList);

#=====================================================================UI=====================================================================
ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Rocky Cities App"),
    dashboardSidebar(
      sidebarMenu(
        
        # Tab names and icons
        menuItem("About", tabName = "about", icon = icon("question-circle-o")),
        menuItem("Data Explorer", tabName = "dataTable", icon = icon("table")),
        menuItem("Temperature", tabName = "temperature", icon = icon("thermometer-three-quarters"),
                 menuSubItem("Monthly", tabName = "t1"),
                 menuSubItem("Graphical Comparison", tabName = "t2"),
                 menuSubItem("Density Plot", tabName = "t3")
                 ),
        menuItem("Prediction Models", tabName = "predictionModels", icon = icon("eye")),
        menuItem("Forecast", tabName = "forecast", icon = icon("line-chart")),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("desktop"))
    )
  ),
  dashboardBody(
    tags$head(
      # Font of the top-right header
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
                        "App created by Riley Waters for STAT406 Environmentrics.",
                        br(),
                        "Data cleaned and compiled by Riley Waters. Last updated on November 23, 2017."
                 )
                )
              ),
              br(),br(),
              div(img(src = "skyline.jpg"), style="text-align: center;")
      ),
      
      # Data Explorer tab content
      tabItem(tabName = "dataTable",
              fluidRow(
                column(3, selectInput("tableCity","City:", c("All",unique(as.character(DataAll$City))))
                ),
                column(3, selectInput("tableYear","Year:", c("All",unique(as.character(DataAll$Year))))
                ),
                column(3,selectInput("tableMonth","Month:", c("All",unique(as.character(DataAll$Month))))
                ),
                column(3,selectInput("tableDay","Day:", c("All",unique(as.character(DataAll$Day))))
                )
              ),
              fluidRow(
                DT::dataTableOutput("tableOut")
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
                       "Spd.of.MaxGust:", tags$strong("km/h")
                )
              )
      ),
      
      
      
      # Temperatures tab content
      tabItem(tabName = "t1",
              fluidRow(
                box(title = "Mean Temperatures by Month", status = "primary", solidHeader = TRUE,
                    plotOutput("t1.1.Out")
                )
              )
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
  
  
  # returns a summarized dataframe with only City, Month, and Temperatures
  # MeanDMax and MeanDMin are the averages of the maximum and minimum temperatures
  summarized.df <- reactive({
    df <- DataAll
    monthly.df <- ddply(df,.(City, Month), summarize,    
                        meanT= round(mean(Mean.Temp, na.rm = TRUE),1) ,
                        maxT = round(max(Max.Temp, na.rm = TRUE),0) ,    
                        minT = round(min(Min.Temp, na.rm = TRUE),0),
                        MeanDMax = round(mean(Max.Temp, na.rm = TRUE),1),
                        MeanDMin = round(mean(Min.Temp, na.rm = TRUE),1))
    return(monthly.df) 
  })
  
  # Data table content
  output$tableOut <- DT::renderDataTable(DT::datatable(
    options = list(pageLength = 12),
    {
      if(input$tableCity == "VAN"){
        tableData <- subset(DataAll, DataAll$City == "VAN")}
      else if(input$tableCity == "KEL"){
        tableData <- subset(DataAll, DataAll$City == "KEL")}
      else if(input$tableCity == "CAL"){
        tableData <- subset(DataAll, DataAll$City == "CAL")}
      else{
        tableData <- DataAll
      }
      if (input$tableYear != "All") {
        tableData <- tableData[tableData$Year == input$tableYear,]
      }
      if (input$tableMonth != "All") {
        tableData <- tableData[tableData$Month == input$tableMonth,]
      }
      if (input$tableDay != "All") {
        tableData <- tableData[tableData$Day == input$tableDay,]
      }
      tableData
    }
  ))
  
  # T1 Content
  output$t1.1.Out <- renderPlot({
    smalldf <- summarized.df()
    p<- ggplot(data=smalldf, aes(factor(Month, levels=monthList), City, color=meanT) ) 
    p<-p + geom_text(size=10, label=as.character(round(smalldf$meanT, 0)))
    p<- p + scale_color_gradient(low="blue", high="orange")
    p <- p + theme(panel.background = element_rect(fill= "transparent"))
    p<- p+xlab("Month")
    print(p)
  })
  
}

shinyApp(ui, server)