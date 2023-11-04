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
library(shinythemes)
library(shinyalert)
library(shinyBS)



source("./Functions.R")

rv <- reactiveValues()

####App####

# Define UI for application 
ui <- fluidPage(
  title = 'Premium Bond High Value Prize Checker',
  setBackgroundColor(
    color = c("#7B68EE", "#20B2AA"),
    gradient = "radial"
  ),
  tags$head(
    tags$style(
      HTML(".shiny-notification {
      position:fixed;
      top:calc(15%);
      left:calc(40%)
      }")
    )
  ),

  # Application title
  titlePanel(htmlOutput('title')),
  mainPanel(  br(),
              br(),
              fluidRow(
                column(6, 
              uiOutput('file.input')),
              column(4,
              uiOutput('file.input.example'))),

              uiOutput('bond.check.button'),
              bsModal("modalExample", "Bond data file example", "data.example", size = "large",
                      DTOutput("csv.data")),
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
server <- function(input, output, session = getDefaultReactiveDomain()) {
  # Determine the current month 
  date <- tolower(format(Sys.Date(), "%B-%Y"))

  output$title <- renderText(paste("NS&I Premium Bonds High Value Prize Checker", format(Sys.Date(), "%B %Y")))
  
  # Determine the url based on this month
  newFile <- paste0("https://www.nsandi.com/files/asset/xlsx/prize-", date, ".xlsx")
  # Create filename to save the data to system folder
  file_name <- paste0("prize-", date, ".csv")
  file_path <- paste0(getwd(),"/data/")
  file <- paste0(file_path, file_name)
  
  #Check if the file existis on NS&I website 
  check.data <- CheckPrizeFileExists(newFile = newFile)
  if(is.null(check.data)){
    shinyalert("File not found", "Current months file is not available on NS&I website", type = "error")
  } else {
  
    
  #Getting prize data  
  options(scipen = 999)
    
  prem.bond.prize.data <- data.table(read.xlsx(newFile, startRow = 3))
  setnames(prem.bond.prize.data, 
           c("Prize Value", "Bond Number", "Total Holding", "Area", "Bond value", "Date of Purchase"))
  prem.bond.prize.data[, `Date of Purchase` := format(as.POSIXct(as.Date(`Date of Purchase`, 
                                                                         origin = "1900-01-01")), '%b-%y')]
  prem.bond.prize.data$Area <- as.factor(prem.bond.prize.data$Area)
  rv$prem.bonds.prize.data <- prem.bond.prize.data

 output$pbtable <- renderDT(prem.bond.prize.data, rownames =FALSE, 
                             filter = "top",
                             options = list(
                               pageLength = 1000,
                               pagination = TRUE
                             ) 
                             )
  
  output$bond.check.button <- renderUI(
    actionButton('bondchecker', 'Check My Bond Numbers', icon = icon("gears"),
                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  
  output$file.input.example <- renderUI(
    actionButton('data.example', 'See example data format for csv file', icon = icon("table"),
                 style = "color: #fff; background-color: #52606D; border-color: #3e4c59"))

   
  output$file.input <- renderUI(
     fileInput("premium.bonds", "Upload CSV file of your Premium Bonds",
                      accept = c(
                        ".csv"))) 
 
   observeEvent(input$data.example, {
     toggleModal(session, 'modalExample', toggle = "open")
   })
   
 
    output$csv.data <- DT::renderDT(
            datatable(data.table(from = c('bond number 1','bond number 3'),
                 to = c('bond number 2','bond number 4'),
                 owner = c('name i.e. pingu', 'name i.e. gromit')), rownames = FALSE),
            
  
    )

  
  observeEvent(input$bondchecker,{
    if(is.null(input$premium.bonds)) 
    {shinyalert("No Premium Bonds loaded to check", "Please upload your csv file containing your bond numbers", type = "warning")}
    
    req(input$premium.bonds)
    showNotification("Checking bond numbers", duration = 2,  type = "warning")
    
    bonds <- read.csv(input$premium.bonds$datapath)
    all.bonds <- MakeBondNumbers(bonds = bonds)
   
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
      
      output$prize.matches <- renderDT(datatable(filter.match.table, rownames =FALSE), 
                                       filter = "top",
                                       options = list(
                                         pageLength = 1000,
                                         pagination = TRUE
                                       ))
      
    } else {
      output$unsuccessful.message <- renderText(("Sorry no Bonds match the high value winners this month"))
      
    }
  })
  
  } # end of else after if check.data = NULL
  
}

# Run the application 
shinyApp(ui = ui, server = server)
