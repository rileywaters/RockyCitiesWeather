# Made by Riley Waters, Nov 2017 for STAT406 Environmentrics
# Rocky Cities Environmetrics App

# Yes, The whole thing is written in this one file.
#Hit Run App in the upper right of this screen to run it.
#You might need to open in browser and zoom out a bit to get the best experience


# Automatically makes sure all required packages are installed and attached
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, DT, ggplot2, plyr, forecast)

# Read in the cleaned csv
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
monthList <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
locList <- c("Vancouver", "Kelowna", "Calgary")
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
        menuItem("Visualizations", tabName = "vis", icon = icon("line-chart"),
                 menuSubItem("Monthly", tabName = "t1"),
                 menuSubItem("Monthly Advanced", tabName = "t2"),
                 menuSubItem("Density", tabName = "t3"),
                 menuSubItem("Histogram", tabName = "t4")
                 ),
        menuItem("Testing", tabName = "test", icon = icon("eye"),
                 menuSubItem("Simple T-Tests", tabName = "m1"),
                 menuSubItem("Model Fitting", tabName = "m2"),
                 menuSubItem("Simulation Tests", tabName = "m3"),
                 menuSubItem("Modelling", tabName = "m4")
        )
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
                   "This is an application for interacting with historical climate data from Vancouver BC, Kelowna BC, and Calgary AB. 
                   Despite being close in location, these cities experience unique climates due their positions relative to the Pacific Ocean, Okanagan Valley, Rocky Mountains, and Canadian Praries.
                   Data was collected by",
                   a("Climate Canada", href = "http://climate.weather.gc.ca/"),
                   "at VANCOUVER INTL, KELOWNA, and CALGARY INTL weather stations. For detailed information about each variable, please visit the",
                   a("glossary", href = "http://climate.weather.gc.ca/glossary_e.html"), ".",
                   br(),br(),
                   "By clicking the tabs on the left, you can:"
                 ),
                 tags$ul(style = "font-family: 'Source Sans Pro'; font-size: 16px",
                   tags$li("Sort and filter the data table"), 
                   tags$li("Compare temperature and precipitation visualizations"), 
                   tags$li("Fit models on the data and run randomization tests")
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
              fluidPage(
                column(width = 6,
                  box(title = "Data Sample Selector", status = "warning", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                      sliderInput(
                        "range", "Years of data to sample from:", min = 1900, 
                          max = 2017, value = c(1900,2017), sep=""
                      ),
                      selectInput("opt.mmm", "Select which temperature set to visualize:",
                                  list("Mean Temperature" = "meanT",
                                       "Max Temperature" = "maxT", 
                                       "Min Temperature" = "minT"),                 
                                  selected="meanT")
                      
                  ),
                  box(title = "Temperatures Chart by Month", status = "primary", solidHeader = TRUE, collapsible = FALSE, collapsed = FALSE,width = 12,
                      plotOutput("t1.1.Out")
                  )
                  
                ),
                
                column(width = 6,
                  box(title = "Temperature Graph by Month", status = "primary", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                      plotOutput("t1.2.Out", height=500)
                  ),
                  box(title = "Absolute Max and Min", status = "primary", solidHeader = TRUE,width = 12,
                      h4(style = "font-familt:Source Sans Pro", textOutput("t1.21.Out")),
                      h4(style = "font-familt:Source Sans Pro", textOutput("t1.22.Out"))
                  )
                )
              )
      ),
      
      # Next Tab
      tabItem(tabName = "t2",
              fluidRow(
                column(width = 6,
                  box(title = "Data Sample Selector", status = "warning", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                      sliderInput(
                        "range2", "Years of data to sample from:", min = 1900, 
                        max = 2017, value = c(1900,2017), sep=""
                      )
                  ),
                  box(title = "Highs and Lows", status = "primary", solidHeader = TRUE,collapsible = FALSE, width = 12,
                      plotOutput("t1.4.Out", height=600)
                  )
                ),
                column(width = 6,
                  box(title = "Average of Highs and Lows", status = "primary", solidHeader = TRUE,collapsible = FALSE, width = 12,
                      plotOutput("t1.3.Out", height = 700)
                  )
                )
              )
      ),
      
      # Next Tab
      tabItem(tabName = "t3",
              fluidRow(
                column(width = 6,
                       box(title = "Data Sample Selector", status = "warning", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                           sliderInput(
                             "range3", "Years of data to sample from:", min = 1900, 
                             max = 2017, value = c(1900,2017), sep=""
                           ),
                           selectInput("opt.mmm2", "Select which temperature set to visualize:",
                                       list("Mean Temperature" = "meanT",
                                            "Max Temperature" = "maxT", 
                                            "Min Temperature" = "minT"),                 
                                       selected="meanT")
                       ),
                       box(title = "Temperature Density of Days", status = "primary", solidHeader = TRUE,collapsible = FALSE, width = 12,
                           plotOutput("t1.5.Out", height=500)
                       )
                ),
                column(width = 6,
                       box(title = "Temperature Buckets of Days", status = "primary", solidHeader = TRUE,collapsible = FALSE, width = 12,
                           plotOutput("t1.6.Out", height = 600)
                       )
                       
                )
              )
      ),
      
      # T4 tab content
      tabItem(tabName = "t4",
              column(width = 6,
                box(title = "Data Sample Selector", status = "warning", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                    sliderInput(
                      "range4", "Years of data to sample from:", min = 1900, 
                      max = 2017, value = c(1900,2017), sep=""
                    )
                )
              ),
              column(width = 6,
                box(title = "Histogram of Temperature", status = "primary", solidHeader = TRUE,collapsible = FALSE, width = 12,
                    sliderInput(inputId = "binwidth",
                                label = "Choose Temperature Band (degrees C)",
                                min = 1, max = 10, step = 1, value = 2),
                    plotOutput("t1.7.Out", height = 600)
                )
              )
      ),
      tabItem(tabName = "m1",
              fluidRow(
                column(width = 6,
                  box(title = "One Sample Test Selector", status = "warning", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                      selectInput("m1opt", "Select City:",
                                  list("Vancouver",
                                       "Kelowna", 
                                       "Calgary"),                 
                                  selected="Vancouver"),
                      selectInput("m1opt2", "Select Response:",
                                  list("Mean.Temp",
                                       "Max.Temp", 
                                       "Min.Temp",
                                       "Heat.Deg.Days",
                                       "Cool.Deg.Days",
                                       "Total.Rain",
                                       "Total.Snow",
                                       "Total.Precip",
                                       "Snow.on.Grnd",
                                       "Spd.of.Max.Gust"),                 
                                  selected="Mean.Temp"),
                      numericInput("m1opt3", "Mean to test against:", value = 10.1),
                      sliderInput(
                        "m1opt4", "Years of data to sample from:", min = 1900, 
                        max = 2017, value = c(1900,2017), sep=""
                      )
                  )
                ),
                column(width = 6,
                  box(title = "One Sample Results", status = "primary", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                      verbatimTextOutput("m1.Out")
                      )
                )
              ),
              fluidRow(
                column(width = 6,
                  box(title = "Two Sample Test Selector", status = "warning", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                  checkboxGroupInput("m2opt", "Select Two Cities:", choices = c("Vancouver", "Kelowna", "Calgary"), selected = c("Vancouver", "Kelowna"),
                                     inline =TRUE),
                  selectInput("m2opt3", "Select Response:",
                              list("Mean.Temp",
                                   "Max.Temp", 
                                   "Min.Temp",
                                   "Heat.Deg.Days",
                                   "Cool.Deg.Days",
                                   "Total.Rain",
                                   "Total.Snow",
                                   "Total.Precip",
                                   "Snow.on.Grnd",
                                   "Spd.of.Max.Gust"),                 
                              selected="Mean.Temp"),
                  sliderInput(
                    "m2opt2", "Years of data to sample from:", min = 1900, 
                    max = 2017, value = c(1900,2017), sep="")
                  )
                ),
                column(width = 6,
                       box(title = "Two Sample Results", status = "primary", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                           verbatimTextOutput("m2.Out")
                       )
                )
              )
              
      ),
      # m2 tab content
      tabItem(tabName = "m2",
              column(width = 6,
                     box(title = "Model Description", status = "warning", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                         p(style = "font-family: 'Source Sans Pro'; font-size: 14px",
                           "A model was fit on each city's data for the purposes of simulation, prediction, and advanced tests. 
                           The models used are seasonal time-series models on the Mean Temperature of each month from 2000 to 2017.
                           It is possible to fit a model on the daily Mean Temperatures (or other responses) since 1900, 
                            but the execution time to fit these models proved to be too long for the purposes of this app."),
                         selectInput("m3opt", "Select City:",
                                     list("Vancouver",
                                          "Kelowna", 
                                          "Calgary"),                 
                                     selected="Vancouver"),
                         sliderInput("m3opt2", "Number of months to predict after Dec 2017:", min = 1, max = 36, value = 12)
                     ),
                     box(title = "Prediction Results", status = "primary", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                         p("Each output in the list is the predicted mean temperature of the next month. The first result is for Jan 2018, and so on.."),
                         verbatimTextOutput("m3.2.Out"))
              ),
              column(width = 6,
                     box(title = "Arima Summary", status = "primary", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                         verbatimTextOutput("m3.Out")
                     ),
                     box(title = "Arima Plot", status = "primary", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                         plotOutput("m3.1.Out")
                     )
              )
      ),
      
      # m3 tab content
      tabItem(tabName = "m3",
              column(width = 6,
                     box(title = "Model Description", status = "warning", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                         selectInput("m4opt", "Select City:",
                                     list("Vancouver",
                                          "Kelowna", 
                                          "Calgary"),                 
                                     selected="Vancouver")
                         ),
                     box(title = "Simulated Data Plot", status = "primary", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12,
                        plotOutput("m4.Out")
                     )
              ),
              column(width = 6,
                     box(title = "Something", status = "primary", solidHeader = TRUE,collapsible = FALSE, collapsed = FALSE,width = 12
                         
                     )
                     
              )
    )
      
      
      
      
    )
  )
)


#=====================================================================SERVER=====================================================================
server <- function(input, output, session) {
  
  # returns a summarized dataframe with only City, Month, and Temperatures
  # MeanDMax and MeanDMin are the averages of the maximum and minimum temperatures
  summarized.df <- reactive({
    df <- subset(DataAll, Year >= input$range[1] & Year <= input$range[2])
    monthly.df <- ddply(df,.(City, Month), summarize,    
                        meanT= round(mean(Mean.Temp, na.rm = TRUE),1) ,
                        maxT = round(max(Max.Temp, na.rm = TRUE),0) ,    
                        minT = round(min(Min.Temp, na.rm = TRUE),0),
                        MeanDmax = round(mean(Max.Temp, na.rm = TRUE),1),
                        MeanDmin = round(mean(Min.Temp, na.rm = TRUE),1))
    return(monthly.df) 
  })
  summarized2.df <- reactive({
    df <- subset(DataAll, Year >= input$range2[1] & Year <= input$range2[2])
    monthly.df <- ddply(df,.(City, Month), summarize,    
                        meanT= round(mean(Mean.Temp, na.rm = TRUE),1) ,
                        maxT = round(max(Max.Temp, na.rm = TRUE),0) ,    
                        minT = round(min(Min.Temp, na.rm = TRUE),0),
                        MeanDmax = round(mean(Max.Temp, na.rm = TRUE),1),
                        MeanDmin = round(mean(Min.Temp, na.rm = TRUE),1))
    return(monthly.df) 
  })
  summarized3.df <- reactive({
    df <- subset(DataAll, Year >= input$range3[1] & Year <= input$range3[2])
    if(input$opt.mmm2 == "meanT"){
      df <- df[!is.na(df$Mean.Temp),]
      df$Temperature <- df$Mean.Temp
    }
    else if(input$opt.mmm2 == "maxT"){
      df <- df[!is.na(df$Max.Temp),]
      df$Temperature <- df$Max.Temp
    }
    else if(input$opt.mmm2 == "minT"){
      df <- df[!is.na(df$Min.Temp),]
      df$Temperature <- df$Min.Temp
    }
    return(df) 
  })
  summarized4.df <- reactive({
    df <- subset(DataAll, Year >= input$range4[1] & Year <= input$range4[2])
    if(input$opt.mmm2 == "meanT"){
      df <- df[!is.na(df$Mean.Temp),]
      df$Temperature <- df$Mean.Temp
    }
    else if(input$opt.mmm2 == "maxT"){
      df <- df[!is.na(df$Max.Temp),]
      df$Temperature <- df$Max.Temp
    }
    else if(input$opt.mmm2 == "minT"){
      df <- df[!is.na(df$Min.Temp),]
      df$Temperature <- df$Min.Temp
    }
    return(df) 
  })
  
  brk = c(seq(-30,30,10),50)
  label10s = c(as.character(seq(-30,30,10)))
  wx_range<-colorRampPalette(c(rgb(0,0.5,1), rgb(1,0.35,0) ))
  
  # Data table content
  output$tableOut <- DT::renderDataTable(DT::datatable(
    options = list(pageLength = 12),
    {
      if(input$tableCity == "Vancouver"){
        tableData <- subset(DataAll, DataAll$City == "Vancouver")}
      else if(input$tableCity == "Kelowna"){
        tableData <- subset(DataAll, DataAll$City == "Kelowna")}
      else if(input$tableCity == "Calgary"){
        tableData <- subset(DataAll, DataAll$City == "Calgary")}
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
  
  # T1.1 Content
  fun1.1 <- function(df, str.column.to.plot) {
    smalldf<-df
    if(str.column.to.plot == "meanT"){
      colPlot<-smalldf$meanT
      str<-"Mean Temperature(C)"}
    if(str.column.to.plot == "minT"){
      colPlot<-smalldf$minT
      str<-"Min Temperature(C)"
    }
    if(str.column.to.plot == "maxT"){
      colPlot<-smalldf$maxT
      str<-"Max Temperature(C)"
    }
    p<- ggplot(data=smalldf, aes(factor(Month, levels=monthList), City, color=meanT))
    p<-p + geom_text(size=10, label=as.character(round(colPlot, 0)))
    p<- p + scale_color_gradient(low="blue", high="orange")
    p <- p + theme(panel.background = element_rect(fill= "transparent"))
    p<- p+xlab("Month")
    p<- p+labs(title=paste(str, "by Month"))
    return(p)
  }
  output$t1.1.Out <- renderPlot({
    smalldf <- summarized.df()
    mPlot <- fun1.1(smalldf,input$opt.mmm)
    print(mPlot)
  })
  
  #T1.2 Content
  fun1.2 <- function(df, str.column.to.plot) {
    if(str.column.to.plot == "meanT")
      str<-"Mean Temperature(C)"
    if(str.column.to.plot == "maxT")
      str<-"Max Temperature(C)"
    if(str.column.to.plot == "minT")
      str<-"Min Temperature(C)"
    p<- ggplot(data=df, aes_string(x="Month", y=str.column.to.plot, group="City", color="City"))  
    p<- p+geom_point(size=5)
    p<- p+geom_line(size=1, alpha=0.9)
    p<- p + geom_hline(yintercept=c(-40,-30,-20,-10,0,10,20,30,40))
    p<- p + theme(panel.background = element_rect(fill= "transparent"))
    p<- p+ylab(paste(str))
    p<- p+xlab("Month")
    p<- p+labs(title=paste(str, "by Month"))
    return(p)
  }
  output$t1.2.Out <- renderPlot({    
    smalldf <- summarized.df()
    mPlot <- fun1.2(smalldf, input$opt.mmm) #meanT = 3, maxT=4, minT=5
    print(mPlot)    
  })
  
  # T1.21 Content
  output$t1.21.Out <- renderPrint({
    smalldf <- summarized.df()
    maxMax <- max(smalldf$maxT)
    rowMax <- which(grepl(maxMax, DataAll$Max.Temp))
    maxDate <- DataAll[rowMax[1],2]
    maxName <- DataAll[rowMax[1],1]
    cat("The highest high between", input$range[1], "and", input$range[2],"is", maxMax, " Celsius, occuring on",paste(maxDate), "in", paste(maxName))
  })
  output$t1.22.Out <- renderPrint({
    smalldf <- summarized.df()
    minMin <- min(smalldf$minT)
    rowMin <- which(grepl(minMin, DataAll$Min.Temp))
    minDate <- DataAll[rowMin[1],2]
    minName <- DataAll[rowMin[1],1]
    cat("The lowest low between", input$range[1], "and", input$range[2],"is", minMin, " Celsius, occuring on",paste(minDate),"in", paste(minName))
  })
  
  # T1.3 Content
  fun1.3 <- function(df) {
    p <- ggplot(df) 
    p <- p + geom_linerange(aes(x=City, 
                                ymin=MeanDmin, ymax=MeanDmax, 
                                color=City, size=3)) + coord_flip()
    p <- p + facet_grid(Month ~ .)
    p <- p + theme( #eliminate background, gridlines, and chart border
      plot.background = element_blank()
      ,panel.background = element_blank()
      ,panel.grid.major = element_line(colour="blue", size=0.5)
      ,panel.grid.minor = element_line(colour="black", size=0.3)
      ,axis.ticks=element_blank()
      ,axis.title.y=element_blank()
      ,axis.text.x =element_text(colour="grey20",angle=0,hjust=.5,vjust=.5,face="plain")
      ,legend.position="none"
    )
    p<- p+labs(title=paste("Average of Daily Highs and Lows by Month"))
    p <- p + xlab("City") + ylab("Temperature (C)")
    return(p)
  }
  output$t1.3.Out <- renderPlot({
    monthly.df <- summarized2.df()    
    MMbar <- fun1.3(monthly.df)
    print(MMbar)
  }, height=700)
  
  # T1.4 Content
  output$t1.4.Out <- renderPlot({    
    smalldf <- summarized2.df()
    p <- ggplot(data=smalldf, aes(x = factor(Month, levels=monthList),
                                  y=meanT,
                                  ymin=minT, ymax=maxT))
    p <- p + geom_crossbar(width=0.2, fill="red")
    p <- p + geom_text(data=smalldf, aes(y=maxT+5, label=maxT), color="red")
    p <- p + geom_text(data=smalldf, aes(y=minT-5, label=minT), color="blue")
    p <- p + facet_grid(City ~ .)
    p <- p + xlab("Month") + ylab("Temperature (C)")
    p <- p + labs(title = "Temperature Highs and Lows by Month")
    print(p)
  }) 
  
  # t1.5 Content
  output$t1.5.Out <- renderPlot({
    df <- summarized3.df()
    if(input$opt.mmm2 == "meanT")
      str<-"Mean Temperature(C)"
    if(input$opt.mmm2 == "maxT")
      str<-"Max Temperature(C)"
    if(input$opt.mmm2 == "minT")
      str<-"Min Temperature(C)"
    colorRange<-colorRampPalette(c(rgb(0,0,1), rgb(1,0.7,0) ))
    p<- ggplot(df, aes(Temperature, color=City)) 
    p<- p + stat_density(position="identity",geom="line", size=2)
    p <- p + geom_vline(xintercept=c(10,20, 30),
                        colour="#990000", linetype="dashed")
    p <- p + geom_vline(xintercept=0,
                        colour="#000000", linetype="dashed")
    p <- p + geom_vline(xintercept=c(-10,-20,-30),
                        colour="blue", linetype="dashed")
    p <- p + ylab("Density of Days")
    p<- p+labs(title=paste(str, "Densities"))
    print(p)    
  })
  
  #t1.6 content
  output$t1.6.Out <- renderPlot({
    df <- summarized3.df()
    if(input$opt.mmm2 == "meanT")
      str<-"Mean Temperature(C)"
    if(input$opt.mmm2 == "maxT")
      str<-"Max Temperature(C)"
    if(input$opt.mmm2 == "minT")
      str<-"Min Temperature(C)"
    #Bin the temperatures into 10 degree buckets, using the "cut" funtion
    df$TempBucket <- cut(df$Temperature, breaks=brk, labels=label10s)
    p <- ggplot(data=df, aes(City, fill=TempBucket )) + geom_bar()  
    p <- p + scale_fill_manual(values=wx_range(11))
    p <- p + ylab("Number of Days in Bucket")
    p<- p+labs(title=paste(str, "Buckets"))
    p <- p + theme( #eliminate background, gridlines, and chart border
      plot.background = element_blank()
      ,panel.border = element_blank()
      ,panel.background = element_rect("black")
      #,axis.text.y=element_blank()
      ,axis.ticks=element_blank()
      ,axis.title.x=element_blank()
      ,axis.text.x = element_text(colour="grey20",angle=0,hjust=.5,vjust=.5,face="plain")
    )  
    print(p)  
  })
  
  # t1.7 content
  output$t1.7.Out <- renderPlot({
    df <- summarized4.df()
    if(input$opt.mmm2 == "meanT")
      str<-"Mean Temperature(C)"
    if(input$opt.mmm2 == "maxT")
      str<-"Max Temperature(C)"
    if(input$opt.mmm2 == "minT")
      str<-"Min Temperature(C)"
    numcolors <- length(unique(df$Temperature)) #how many colors do we need?
    
    m <- ggplot(df, aes(x=Temperature, fill=factor(Temperature)))
    m <- m + geom_histogram(binwidth=input$binwidth)
    m <- m + scale_fill_manual(values=wx_range(numcolors))
    m <- m + facet_grid(City ~ .)
    m <- m + ylab("Count of Days")
    m<- m+labs(title=paste(str, "Histogram"))
    m <- m + geom_vline(xintercept=c(10,20, 30),
                        colour="#990000", linetype="dashed")
    m <- m + geom_vline(xintercept=0,
                        colour="#000000", linetype="dashed")
    m <- m + geom_vline(xintercept=c(-10,-20,-30),
                        colour="blue", linetype="dashed")
    m <- m + theme(legend.position="none")
    print(m)
  })
  
  # m1 content
  output$m1.Out <- renderPrint({
    if(input$m1opt == "Vancouver")
      df <- subset(DataAll, City == "Vancouver"& Year >=input$m1opt4[1] & Year <= input$m1opt4[2])
    if(input$m1opt == "Kelowna")
      df <- subset(DataAll, City == "Kelowna"& Year >=input$m1opt4[1] & Year <= input$m1opt4[2])
    if(input$m1opt == "Calgary")
      df <- subset(DataAll, City == "Calgary"& Year >=input$m1opt4[1] & Year <= input$m1opt4[2])

    y <- df[,input$m1opt2]
    if(is.na(input$m1opt3))
     print("No mean entered")
    else{
      m <- input$m1opt3
      t.test(y,mu=m)
    }
  })
  
  # m2 Content
  output$m2.Out <- renderPrint({
    if(length(input$m2opt) == 2){
      if(input$m2opt[1] == "Vancouver" & input$m2opt[2] =="Kelowna"){
        df <- subset(DataAll, City == "Vancouver"& Year >=input$m2opt2[1] & Year <= input$m2opt2[2])
        df2 <- subset(DataAll, City == "Kelowna"& Year >=input$m2opt2[1] & Year <= input$m2opt2[2])
      }
      else if(input$m2opt[1] =="Vancouver" & input$m2opt[2] == "Calgary"){
        df <- subset(DataAll, City == "Vancouver"& Year >=input$m2opt2[1] & Year <= input$m2opt2[2])
        df2 <- subset(DataAll, City == "Calgary"& Year >=input$m2opt2[1] & Year <= input$m2opt2[2])
      }
      else if(input$m2opt[1] =="Kelowna" & input$m2opt[2] == "Calgary"){
        df <- subset(DataAll, City == "Kelowna"& Year >=input$m2opt2[1] & Year <= input$m2opt2[2])
        df2 <- subset(DataAll, City == "Calgary"& Year >=input$m2opt2[1] & Year <= input$m2opt2[2])
      }
      df$y <- df[,input$m2opt3]
      df2$y <- df2[,input$m2opt3]
      t.test(df$y,df2$y)
    }
    else
      print("Please select exactly two cities.")
  })
  
  #m3 content
  fitted <- reactive({
    df <- subset(DataAll, City == input$m3opt & Year >= 2000 & Year <= 2017)
    monthly.df <- ddply(df,.(Year, Month), summarize, meanT= mean(Mean.Temp, na.rm = TRUE))
    monthlyFit <- ts(monthly.df$meanT)
    fit <- auto.arima(monthlyFit)
  })
  output$m3.Out <- renderPrint({
    fit <- fitted()
    fit
  })
  output$m3.1.Out <- renderPlot({
    fit <- fitted()
    fcast <- forecast(fit, h=18)
    plot(fcast, xlab = "YearMonth, starting with 0 = Jan 2000", ylab = "Temperature")
  })
  output$m3.2.Out <- renderPrint({
    fit <- fitted()
    p <- predict(fit, n.ahead = input$m3opt2)
    print(p$pred)
    
  })
  
  #m4 content
  fitted2 <- reactive({
    df <- subset(DataAll, City == input$m4opt & Year >= 2000 & Year <= 2017)
    monthly.df <- ddply(df,.(Year, Month), summarize, meanT= mean(Mean.Temp, na.rm = TRUE))
    monthlyFit <- ts(monthly.df$meanT)
    fit <- auto.arima(monthlyFit)
  })
  output$m4.Out <- renderPlot({
    fit <- fitted()
    plot(simulate(fit,future=FALSE),col='red', xlab = "YearMonth (Starting 0 = Jan 2000)")
  })
  
  
}

shinyApp(ui, server)