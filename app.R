library(shiny)
library(shinythemes)
library(shinyWidgets)
fifa_wc <- read.csv('WorldCups.csv')


ui <- fluidPage(theme = shinytheme("readable"),
  setBackgroundImage(src = "fifa1.jpg"),
  titlePanel("FIFA viz"),
  sidebarLayout(
    sidebarPanel( width = 3,
      tags$style(".well {background-color: #bfe6e6;}"),
      helpText(h5("This application gives info and stats about FIFA worldcups")),
      selectInput("year", label = "Choose a FIFA worldcup year", 
                  choices = list("1930",
                                 "1934",
                                 "1938",
                                 "1950",
                                 "1954",
                                 "1958",
                                 "1962",
                                 "1966",
                                 "1970",
                                 "1974",
                                 "1978",
                                 "1982",
                                 "1986",
                                 "1990",
                                 "1994",
                                 "1998",
                                 "2002",
                                 "2006",
                                 "2010",
                                 "2014",
                                 "2018"), selected = "2010")
    ),
    mainPanel(
      fluidRow(
      tabsetPanel( type = "tabs",
                   tabPanel(h5("Information"),
                            fluidRow(style="height:10px;", column(12, " ")),
      fluidRow(#style="height:10px;",
        column(2,div(style = "height: 170px; padding: 0px 0px",imageOutput("preImage"))),
        column(8,div(style = "height: 170px; padding: 0px 0px",htmlOutput("text"))),
        column(2,div(style = "height: 170px; padding: 0px 0px",imageOutput("mascotImage")))
        ),
      fluidRow(column(12,div(style = "height: 1px; padding: 0px 0px"," "))),
      fluidRow(
        column(3,div(style = "height: 150px; padding: 0px 0px",htmlOutput("win"),imageOutput("imagewinner"))),
        column(3,div(style = "height: 150px; padding: 0px 0px",htmlOutput("rp"),imageOutput("imagerp"))),
        column(3,div(style = "height: 150px; padding: 0px 0px",htmlOutput("th"),htmlOutput("text1"), align = "center")),
        column(3,div(style = "height: 150px; padding: 0px 0px",htmlOutput("fo"),htmlOutput("text2"), align = "center"))
      ),
      fluidRow(
        column(12,div(style = "height: 10px; padding: 0px 0px"," "))),
      fluidRow(
        column(3,div(style = "height: 45px; padding: 0px 0px",htmlOutput("win1"), align = "center")),
        column(3,div(style = "height: 45px; padding: 0px 0px",htmlOutput("rp1"), align = "center")),
        column(3,div(style = "height: 45px; padding: 0px 0px",textOutput("th1"))),
        column(3,div(style = "height: 45px; padding: 0px 0px",textOutput("fo1")))
      )
      
      ),
      
     tabPanel(h5("Some Stats"),
              fluidRow(column(12,div(style = "height: 01px; padding: 0px 0px"))),
      
      fluidRow(column(12,
                      fluidRow( 
                               column(12,div(style = "height: 05px; padding: 0px 0px"),fluidRow(column(3,offset = 1, htmlOutput("teamspl")), 
                                                  column(3,offset =3 ,htmlOutput("matpl"))))))),
      fluidRow(column(4,div(style = "height: 05px; padding: 0px 0px"),offset = 4,htmlOutput("glsc"))),
      
      fluidRow(column(12,div(style = "height: 05px; padding: 0px 0px"))),
      
      fluidRow(column(3,htmlOutput("attend"), offset = 4))
      )
      )
     ))))
      
  

    
  
  
  




server <- function(input, output, session) {
  
  #flag-header-one
  output$preImage <- renderImage({
    filename <- normalizePath(file.path('www',
                                        paste('image', input$year, '.jpg', sep='')))
    
    list(src = filename,
         width = 120,
         height = 140,
         alt = paste("Image number", input$year))
   
    
  }, deleteFile = FALSE)
  
  #mascot
  output$mascotImage <- renderImage({
    filename <- normalizePath(file.path('www',
                                        paste('mascot',input$year, '.png', sep='')))
    
    list(src = filename,
         width = 120,
         height = 140,
         alt = paste("Official Mascot","(Note: Mascots were used in FIFA worldcups post 1966)"))
    
    
  }, deleteFile = FALSE)
  
  #text-header-one
  output$text <- renderText({
    infotext <- fifa_wc[fifa_wc$Year == input$year, 11]
    infolink <- fifa_wc[fifa_wc$Year == input$year, 12]
    paste("<p><h5 style = color:#21618C>",infotext,"</h5></p>","<a href =",infolink,">","Read more...","</a>")})
  #wintrpext
  output$win <- renderText({ 
  paste("<h3 style= color:#154360>","Winner","</h3>")})
  
  output$rp <- renderText({ 
  paste("<h3 style= color:#154360>","Runner up","</h3>")})
  
  output$th <- renderText({ 
    paste("<h2 style= color:#154360>"," ","</h2>")})
  
  output$fo <- renderText({ 
    paste("<h2 style= color:#154360>"," ","</h2>")})
  
  
  #image-winner
  output$imagewinner <- renderImage({
    filename <- normalizePath(file.path('www',
                                        paste('imagew', input$year, '.png', sep='')))
    
    list(src = filename,
         width = 175,
         height = 110,
         alt = paste("Image number", input$year))
    
  }, deleteFile = FALSE)
  #image-runnerup
  output$imagerp <- renderImage({
    filename <- normalizePath(file.path('www',
                                        paste('imager', input$year, '.png', sep='')))
    
    list(src = filename,
         width = 175,
         height = 110,
         alt = paste("Image number", input$year))
    
  }, deleteFile = FALSE)
  #image-text-thirs-fourth
  output$text1 <- renderText({ 
    n <-input$year
    third = fifa_wc[fifa_wc$Year == n, 5]
    paste("<h4 style= color:#154360>","The team placed third was","<h3 style = color:#145A32>",third,"</h3>","</h4>")}) 

  output$text2 <- renderText({ 
    n <-input$year
    fourth = fifa_wc[fifa_wc$Year == n, 6]
    paste("<h4 style= color:#154360>","The team placed fourth was","<h3 style = color:#145A32>",fourth,"</h3>","</h4>")})
  
#text-below
  #wintrpext
  output$win1 <- renderText({ 
    wn = fifa_wc[fifa_wc$Year == input$year,3]
    paste("<h4 style= color:#145A32 align = center>",wn,"</h4>")})
  
  output$rp1 <- renderText({ 
    rp = fifa_wc[fifa_wc$Year == input$year,4]
    paste("<h4 style= color:#145A32 align = center>",rp,"</h4>")})
    
  output$th1 <- reactive({
    paste("")
  })
  output$fo1 <- reactive({
    paste("")
  })
  
  output$teamspl <- renderText({ QT = fifa_wc[fifa_wc$Year == input$year, 8]
    paste("<h4 style= color:#145A32 align = 'center'><img src= teamsqicon.png height = 50 width = 50 align = middle><br><b>Teams Qualified:</b>","<br>","<h2 style=color:#17A589 align = center>",QT,"</h3>","</img>","</h4>")})
  
  output$matpl <- renderText({ MP = fifa_wc[fifa_wc$Year == input$year, 9]
    paste("<h4 style= color:#145A32 align = 'center'><img src= matchpicon.png height = 50 width = 50 align = middle><br><b>Total Matches Played:</b>","<br>","<h2 style=color:#17A589 align = center>",MP,"</h2>","</img>","</h4>")})
   
  output$glsc <- renderText({ GL = fifa_wc[fifa_wc$Year == input$year, 7]
    paste("<h4 style= color:#145A32><img src= goalsicon.png height = 50 width = 50><b>Total Goals Scored:</b>","<br>","<h1 style=color:#17A589 align = center>",GL,"</h1>","</h4>")})
  
  output$attend <- renderText({ AD = fifa_wc[fifa_wc$Year == input$year, 10]
    paste("<h4 style= color:#145A32><img src= crowdicon.png height = 50 width = 70><b>Attendance:</b>","<br>","<h1 style=color:#17A589 align = center>",AD,"</h1>","</h4>")})
  
  
}

shinyApp(ui = ui, server = server)
