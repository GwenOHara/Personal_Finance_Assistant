#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(openxlsx)
library(data.table)
library(DT)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(htmlOutput('title')),

    # Sidebar with a slider input for number of bins 
    # sidebarLayout(
    #     sidebarPanel(
    #         # sliderInput("bins",
    #         #             "Number of bins:",
    #         #             min = 1,
    #         #             max = 50,
    #         #             value = 30)
    #     ),

        # Show a plot of the generated distribution
        mainPanel(  br(),
                    br(),
          actionButton('bondchecker', 'Check My Bond Numbers', icon = icon("gears"),
                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          br(),
          br(),
          br(),
          br(),
          DTOutput("pbtable")
        )
    #)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

# Determine the current month and the url based on this month
  date <- tolower(format(Sys.Date(), "%B-%Y"))
  newFile <- paste0("https://www.nsandi.com/files/asset/xlsx/prize-", date, ".xlsx")
  # Create filename to save the data to system folder
  file_name <- paste0("prize-", date, ".csv")
  file_path <- paste0(getwd(),"/data/")
  file <- paste0(file_path, file_name)
  
  output$title <- renderText(paste("Premium Bonds High Value Prize Checker", format(Sys.Date(), "%B %Y")))
  
  
  options(scipen = 999)
  if(!file.exists(file)){
    #Add in a shiny warning if the newFile doesn't exist and stop
  prem.bond.prize.data <- data.table(read.xlsx(newFile, startRow = 3))
  
  setnames(prem.bond.prize.data, 
           c("Prize Value", "Bond Number", "Total Holding", "Area", "Bond value", "Date of Purchase"))
  
  prem.bond.prize.data[, `Date of Purchase` := format(as.POSIXct(as.Date(`Date of Purchase`, 
                                                                         origin = "1900-01-01")), '%b-%y')]
  
  write.csv(prem.bond.prize.data, paste0(file_path, file_name))
  
  prem.bond.prize.data <- as.data.frame(prem.bond.prize.data)
  
  } else {
    prem.bond.prize.data <- read.csv(file)
    prem.bond.prize.data <- subset(prem.bond.prize.data, select = -c(X))
  }
  
  prem.bond.prize.data$Area <- as.factor(prem.bond.prize.data$Area)
  
  
 output$pbtable <- renderDT(prem.bond.prize.data, 
                            filter = "top",
                            options = list(
                            pageLength = 1000,
                            pagination = TRUE
                            ))

}

# Run the application 
shinyApp(ui = ui, server = server)
