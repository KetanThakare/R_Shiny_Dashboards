library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(lubridate)
library(scales)
library(gridExtra)

pickedup_combine <- read.csv("Data/pickup_combined.csv")
pubydate <- read.csv("Data/TotalPUbyDate.csv")
pubyday<- read.csv("Data/TotalPUbyDay.csv")
pubymonth <- read.csv("Data/TotalPUbyMonth.csv")
pubymonth_fc <- read.csv("Data/TotalPUbyMonth_fc.csv")
pubyweek <- read.csv("Data/TotalPUbyWeek.csv")

ui <- dashboardPage(skin = "blue",
  dashboardHeader(title = "Uber vs Lyft"),
  
  dashboardSidebar(
    
    sidebarMenu(id="tabs",
                sidebarMenuOutput("menu"))),
  dashboardBody(
    tags$head(tags$style(HTML('.info-box {min-height: 60px;} .info-box-icon {height: 60px; line-height: 60px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
    #some stats
    #Average pickup per day in that month info box
    #Average pickup per week 
    tabItems(tabItem("dashboard",
    fluidRow(column(8, offset = 2, box(width = 8, htmlOutput("heading")))),
    fluidRow(box(width = 4, htmlOutput("average_dailybymonth")), column(3),
             box(width = 4, selectInput("enter_month", "Select Month:",
                                                  choices = c("June '16",
                                                              "July '16",
                                                              "August '16",
                                                              "September '16",
                                                              "October '16",
                                                              "November '16",
                                                              "December '16")))),
    
    fluidRow(infoBoxOutput("Lyft"),
                   column(3),infoBoxOutput("Uber")),
    fluidRow(column(12, div(style = "height:10px; padding: 0px0px", ""))),
    fluidRow(box(width = 7, htmlOutput(("average_Pickup_monthly")))),
             fluidRow(infoBoxOutput("Lyft1"),
                      column(3),infoBoxOutput("Uber1"))),

    tabItem("bc",box(width = 3, selectInput("intervaltype","Interval Type:", 
                                          choices = c("Monthly", 
                                                      "Weekly",
                                                      "Daily"))), box(width = 9, 
                               solidHeader = TRUE, 
                               plotlyOutput("barplot", height ="400px"))), 
    tabItem("pc",box(width = 3, selectInput("sp_month","Specify Month:", 
                                            choices = c("June '16",
                                                        "July '16",
                                                        "August '16",
                                                        "September '16",
                                                        "October '16",
                                                        "November '16",
                                                        "December '16"), selected = "July '16")), box(width = 9, solidHeader = TRUE, plotlyOutput("piechart", height ="400px"))),
    tabItem("lc", box(width = 3, dateRangeInput("date_s", "Date Range:", start = "2016-06-05", end = "2016-12-31", min = "2016-06-05",
                                 max = "2016-12-31", format = "yyyy-mm-dd", startview = "2016-06-05")), box(width = 9, solidHeader = TRUE, plotlyOutput("linechart", height ="400px")))
        )
    )
  )


server <- function(input, output, session) {
  output$heading <- renderUI({
    HTML("<h5><b>The objective is to do a comparative anaalysis of Uber and Lyft pickups at Phoenix Airport. We start with some mean stats to gain a basic idea.</h3>")
  })
  #rendering default tab
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Charts",icon = icon("bar-chart-o"),
               menuSubItem("Bar Chart viz", tabName = "bc"),
               menuSubItem("Pie Chart viz", tabName = "pc"),
               menuSubItem("Line Chart viz", tabName = "lc")
      )
    )
  })
  isolate({updateTabItems(session, "tabs", "dashboard")})
  
  
  
    output$average_dailybymonth <- renderUI({
    HTML(paste("<h4 style = align:center>Average pickup per day for ","<b>",input$enter_month,"</b>"," is:" ))
  })
    
    output$average_Pickup_monthly <- renderUI({
      HTML(paste("<h5>Average pickup for period of June '16 to December '16 was:"))
    })
  
  #infobox-Uber
  output$Uber <- renderInfoBox({
    if (input$enter_month == "July '16" | input$enter_month == "August '16" | input$enter_month == "October '16" | input$enter_month == "December '16")
    {
      pubymonth_fc_temp1_Uber = pubymonth_fc[pubymonth$Month == input$enter_month,1]
      monthly_total = pubymonth_fc[pubymonth_fc$Month == "August '16" & pubymonth_fc$Service_Provider == "Uber",4]
      average1 = as.integer(monthly_total/31)
    }
    else
    {
      pubymonth_fc_temp1_Uber = pubymonth_fc[pubymonth$Month == input$enter_month,1]
      monthly_total = pubymonth_fc[pubymonth_fc$Month == "August '16" & pubymonth_fc$Service_Provider == "Uber",4]
      average1 = as.integer(monthly_total/30)
    }
    infoBox(
      "Uber Pickups:", paste0(average1), icon = icon("taxi"), color = "green"
    )
  })
  
  #infobox-Lyft
  output$Lyft <- renderInfoBox({
    if (input$enter_month == "July '16" | input$enter_month == "August '16" | input$enter_month == "October '16" | input$enter_month == "December '16")
    {
      pubymonth_fc_temp1_Uber = pubymonth_fc[pubymonth$Month == input$enter_month,1]
      monthly_total = pubymonth_fc[pubymonth_fc$Month == "August '16" & pubymonth_fc$Service_Provider == "Lyft",4]
      average1 = as.integer(monthly_total/31)
    }
    else
    {
      pubymonth_fc_temp1_Uber = pubymonth_fc[pubymonth$Month == input$enter_month,1]
      monthly_total = pubymonth_fc[pubymonth_fc$Month == "August '16" & pubymonth_fc$Service_Provider == "Lyft",4]
      average1 = as.integer(monthly_total/30)
    }
    infoBox(
      "Lyft Pickups:", paste0(average1), icon = icon("car"), color = "red"
    )
  })
  
  
  output$Uber1 <- renderInfoBox({
  infoBox(
    "Average Uber Pickups:",as.integer(mean(pubymonth$Uber)), icon = icon("taxi"), color = "green"
  )
})
  
  output$Lyft1 <- renderInfoBox({
    infoBox(
      "Average Lyft Pickups:",as.integer(mean(pubymonth$Lyft)), icon = icon("car"), color = "red"
    )
  })
  
  
  
  #CHARTS
  barplottest <- reactive({
    q <- plot_ly(pubyday, x = ~t_day, y = ~Lyft, type = 'bar', name = "Lyft", marker = list(color = 'rgb(255,69,0)')) %>% add_trace(y = ~Uber, name = "Uber", marker = list(color = 'rgb(50,205,50)'))  %>% layout(xaxis = list(title = '', tickangle = -45), yaxis = list(title = 'Count'), barmode = 'group', rangeslider = list (type = "date"))
    p <- plot_ly(pubymonth, x = ~Month, y = ~Lyft, type = 'bar', name = "Lyft",marker = list(color = 'rgb(255,69,0)')) %>% add_trace(y = ~Uber, name = "Uber", marker = list(color = 'rgb(50,205,50)'))  %>% layout(xaxis = list(title = '', tickangle = -45), yaxis = list(title = 'Count'), barmode = 'group', rangeslider = list (type = "date"))
    r <- plot_ly(pubyweek, x = ~week, y = ~Lyft, type = 'bar', name = "Lyft",marker = list(color = 'rgb(255,69,0)')) %>% add_trace(y = ~Uber, name = "Uber", marker = list(color = 'rgb(50,205,50)'))  %>% layout(xaxis = list(title = '', tickangle = -90), yaxis = list(title = 'Count'), barmode = 'group', rangeslider = list (type = "date"))
    if ( "Monthly" %in% input$intervaltype) return(p)
    if ( "Daily" %in% input$intervaltype) return(q)
    if ("Weekly" %in% input$intervaltype) return(r)
  })
  output$barplot <- renderPlotly({   
    dataplots = barplottest()
    print(dataplots)
  }) 
  
  piepcharttest <- reactive({
    colors <- c('rgb(255,69,0)', 'rgb(50,205,50)')
    pubymonth_fc_temp <- pubymonth_fc[pubymonth_fc$Month == input$sp_month,]
    p_r <- plot_ly(pubymonth_fc_temp, labels = ~Service_Provider, values = ~n, type = 'pie', marker = list(colors = colors)) %>%
      layout(title = 'Market Share of Uber and Lyft',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
  
  output$piechart <- renderPlotly({
  dataplots_1 = piepcharttest()
  print(dataplots_1)  
  })
  
  linecharttest <- reactive({
    
      mindate <- as.Date(input$date_s[1]) 
      maxdate <- as.Date(input$date_s[2])
      
      temp_data = subset(pubyday, as.Date(t_day) > mindate & as.Date(t_day) < maxdate)
      Day = as.Date(temp_data$t_day)
      plot_s <- ggplot(temp_data, aes(x = Day)) + geom_line(aes(y = Uber, colour = "Uber")) + geom_line(aes(y = Lyft, colour = "Lyft")) + scale_colour_manual(name=" ", values=c(Uber="green", Lyft="red"))+ scale_x_date(limits = as.Date(c(mindate, maxdate)), date_breaks = "10 days", date_minor_breaks = "1 day", labels=date_format("%d-%m-%Y")) + theme_minimal() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Day") + ylab("No. of Customers") + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
      plot_ <-ggplotly(plot_s)
      plot_
      })
  
   output$linechart <- renderPlotly({
     dataplots_2 = linecharttest()
     print(dataplots_2)
        })
  
  
}

shinyApp(ui, server)

# pubymonth_fc_temp <- pubymonth_fc[pubymonth_fc$Month == "August '16",]
# pubymonth_fc_temp
# View(pubymonth_fc_temp)
