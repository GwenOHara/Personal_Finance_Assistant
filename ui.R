
ui <- dashboardPage(
  
  title = "Persoanl Finance Assistant",
  skin = "blue",
  
  header = dashboardHeader(
    title = tagList(
      
      # The bit before the comma here is shown by default in the top-left. The bit after the comma is shown when the sidebar is collapsed.
      # Replace favicon.svg with your own image
      span(class = "logo-lg", "Finances"), img(src = 'favicon.png', width = 30)),
    
    # Adds a visible tag if the app is not in live mode (e.g. "Test")
    leftUi = tagList(
      uiOutput("EnvirUI")
    )#,
    
    # Creates the documentation menu in the top-right. To fill with links, edit kLinksDoc at the top of Interface.r
    # dropdownMenu(
    #   type = "messages",
    #   headerText = "",
    #   badgeStatus = NULL,
    #   icon = list("Documentation", icon("caret-down"))#,
    #   # .list = Map(GenerateDocLinkTag, label = names(kLinksDoc), 
    #   #             link = kLinksDoc)
    # )
    
  ),
  
  # Sidebar ===============================================================
  
  sidebar = dashboardSidebar(
    id = 'SidebarExpanded',  # ID so that observers (e.g. for the TM1 buttons) can find out if the sidebar has been collapsed or not
    
    # Reference the .css file, where styling can be added
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "myapp.css")),
    
    # Make the app's icon appear in the browser tab. The icon can be in svg, png or ico format, but should be called favicon.*
    tags$head(tags$link(rel = "icon", href = "favicon.png")),
    
    sidebarMenu(
      
      id = "Tabs",  # Setting id makes input$Tabs give the tabName of currently-selected tab
      
      
      menuItem("High Value Prizes", tabName = 'Tab1', icon = icon("sack-dollar")),
      uiOutput('style_tag'),
      # Check https://fontawesome.com/v5.3.1/icons?d=gallery&m=free for more valid icon names
      
      menuItem("Stocks & Shares", icon = icon("calendar-alt"), startExpanded = TRUE,
               menuSubItem("Share Prices", tabName = "Tab2", icon = icon("folder-open"))#,
               #menuSubItem("Tab 3", tabName = "Tab3", icon = icon("chart-line"))
      )
      
    )
    
  ),
  
  
  
  
  body = dashboardBody(
   
    tags$head(
      tags$style(
        HTML(".shiny-notification {
      position:fixed;
      top:calc(15%);
      left:calc(40%)
      }")
      )
    ),
    
    tabItems(
      
      # The first tab ===============================================================
      tabItem("Tab1",
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
      ),
      
      # The second tab ===============================================================
      tabItem("Tab2",
              #This tab is adapted from the basic code from:
              # A shiny app for monitoring a stock portfolio and comparing stock performance
              # January 2021
              # Peer Christensen
              # hr.pchristensen@gmail.com
              fluidPage(theme = shinytheme("cyborg"),
                
                # Title
                titlePanel("Shares"),
                
                # Sidebar 
                sidebarLayout(
                  sidebarPanel(width = 3,
                               
                               # Let user pick stocks
                               pickerInput(
                                 inputId = "stocks",
                                 label = h4("Shares"),
                                 choices = NULL,
                                 options = list(`actions-box` = TRUE, 
                                                liveSearch = TRUE), 
                                 multiple = T
                               ),
                               
                               # Pick time period
                               radioButtons("period", label = h4("Period"),
                                            choices = list("1 month" = 1, "3 months" = 2, "6 months" = 3, 
                                                           "12 months" = 4, "5 years" = 5, "YTD" = 6), 
                                            selected = 4
                               ),
                               
                               # Pick benchmark
                               radioButtons("benchmark", label = h4("Benchmark"),
                                            choices = list("SP500" = 1, "FTSE100" = 2,"None" = 3),
                                            selected = 3)
                              
                               
                  ),
                  
                  # Plot results
                  mainPanel(
                    h4(tags$b('Last Closing Share Price')),
                    DTOutput('share.price.table'),
                    h4(tags$b('Share Price')),
                    plotlyOutput("share.price.plot",height=800),
                    br(),
                    h4(tags$b('Baselined to 100')),
                    plotlyOutput("relative.share.price.plot",height=800)
                  )
                )
              )
              
              
              
              
              # "Here's a browser button. Use it when developing to pause the app and run ad-hoc code:",
              # actionButton("browser", "browser()")
              
      )
    )
  )
  )
    