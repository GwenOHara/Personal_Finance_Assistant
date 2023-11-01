#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(openxlsx)
library(data.table)
library(DT)
library(shinycssloaders)


source("./Functions.R")

rv <- reactiveValues()

####App####



# Define UI for application 
ui <- fluidPage(
  title = 'Premium Bond High Value Prize Checker',
  
  #Use a gradient in background
  #setBackgroundColor("Teal"),
  
  setBackgroundColor(
    color = c("#7B68EE", "#20B2AA"),
    gradient = "radial"
  ),
  
  # Application title
  titlePanel(htmlOutput('title')),
  mainPanel(  br(),
              br(),
              uiOutput('bond.check.button'),
              br(),
              br(),
              span(textOutput('successful.message'), style = "color:yellow; font-size:20px"), 
              span(textOutput('unsuccessful.message'), style = "color:orange; font-size:20px"), 
              DTOutput("prize.matches"),
              br(),
              br(),
              withSpinner(DTOutput("pbtable"))
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Determine the current month 
  date <- tolower(format(Sys.Date(), "%B-%Y"))
  output$title <- renderText(paste("Premium Bonds High Value Prize Checker", format(Sys.Date(), "%B %Y")))
  
  # Determine the url based on this month
  newFile <- paste0("https://www.nsandi.com/files/asset/xlsx/prize-", date, ".xlsx")
  # Create filename to save the data to system folder
  file_name <- paste0("prize-", date, ".csv")
  file_path <- paste0(getwd(),"/data/")
  file <- paste0(file_path, file_name)
  
  options(scipen = 999)
  prem.bond.prize.data <- data.table(read.xlsx(newFile, startRow = 3))
  
  setnames(prem.bond.prize.data, 
           c("Prize Value", "Bond Number", "Total Holding", "Area", "Bond value", "Date of Purchase"))
  
  prem.bond.prize.data[, `Date of Purchase` := format(as.POSIXct(as.Date(`Date of Purchase`, 
                                                                         origin = "1900-01-01")), '%b-%y')]
  prem.bond.prize.data <- as.data.frame(prem.bond.prize.data)
  
  prem.bond.prize.data$Area <- as.factor(prem.bond.prize.data$Area)
  rv$prem.bonds.prize.data <- prem.bond.prize.data
  
  output$pbtable <- renderDT(prem.bond.prize.data, 
                             filter = "top",
                             options = list(
                               pageLength = 1000,
                               pagination = TRUE
                             ))
  
  output$bond.check.button <- renderUI(
    actionButton('bondchecker', 'Check My Bond Numbers', icon = icon("gears"),
                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  
  
  observeEvent(input$bondchecker,{
    bonds <- read.csv(paste0(file_path, "bonds.csv"))
    if(file.exists(paste0(file_path, 'last_time_bonds.csv'))){
      last.time.bonds <- read.csv(paste0(file_path, "last_time_bonds.csv"))
      last.time.bonds <- subset(last.time.bonds, select = -c(X))
      if(any(bonds != last.time.bonds)){
        all.bonds <- MakeBondNumbers(bonds = bonds, file_path = file_path)
        
      } else {
        all.bonds <- read.csv(paste0(file_path, "all_bonds.csv"))
        all.bonds <- subset(all.bonds, select = -c(X))
      }
      
      
    } else {
      all.bonds <- MakeBondNumbers(bonds = bonds, file_path = file_path)
      
    }
    
    
    #test.data2 <- data.table(bonds = rv$prem.bonds.prize.data$`Bond Number`[1:3],
    #                        owner = "Graham")
    
    #if(any(rv$prem.bonds.prize.data$`Bond Number` %in% test.data2$bonds)){
    
    
    if(any(rv$prem.bonds.prize.data$`Bond Number` %in% all.bonds$bonds)){
      output$successful.message <- renderText(("There is a match in the high value winners this month!"))
      
      
      table <- data.table(rv$prem.bonds.prize.data)
      match.table <- table[, match := lapply(`Bond Number`, function(x) x %in% all.bonds$bonds)]
      filter.match.table <-  match.table[`match` == "TRUE"][, `match` := NULL]
      
      
      setkey(filter.match.table, `Bond Number`)
      setkey(all.bonds, `bonds`)
      filter.match.table[all.bonds]
      
      output$prize.matches <- renderDT(filter.match.table, 
                                       filter = "top",
                                       options = list(
                                         pageLength = 1000,
                                         pagination = TRUE
                                       ))
      
    } else {
      output$unsuccessful.message <- renderText(("Sorry no Bonds match the high value winners this month"))
      
    }
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
