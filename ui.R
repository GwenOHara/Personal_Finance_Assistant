
ui <- dashboardPage(
  
  title = "Personal Finance Assistant",
  skin = "blue",
  
  header = dashboardHeader(
    title = tagList(
      
      # The bit before the comma here is shown by default in the top-left. The bit after the comma is shown when the sidebar is collapsed.
      # Replace favicon.svg with your own image
      span(class = "logo-lg", " Personal Finances"), img(src = 'favicon.png', width = 30)),
    
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
    
    #import all the fontawesomefonts 
    tags$style('@import url(https://use.fontawesome.com/release/v6.1.0.css/all.css);'),
    
    sidebarMenu(
      
      id = "Tabs",  # Setting id makes input$Tabs give the tabName of currently-selected tab
      
      
      menuItem("Premium Bond Prizes", tabName = 'Tab1', icon = icon("sack-dollar")),
      uiOutput('style_tag'),
      # Check https://fontawesome.com/v5.3.1/icons?d=gallery&m=free for more valid icon names
      
      menuItem("Stocks & Shares", icon = icon("money-check-dollar"), startExpanded = TRUE,
               menuSubItem("Share Prices", tabName = "Tab2", icon = icon("chart-line"))#,
               #menuSubItem("Tab 3", tabName = "Tab3", icon = icon("chart-line"))
      ),
      
      menuItem("Retirement Plan", tabName = 'Tab3', icon = icon("coins"))
      
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
                    withSpinner(plotlyOutput("share.price.plot",height=800)),
                    br(),
                    h4(tags$b('Baselined to 100')),
                    withSpinner(plotlyOutput("relative.share.price.plot",height=800))
                  )
                )
              )
              
              # "Here's a browser button. Use it when developing to pause the app and run ad-hoc code:",
              # actionButton("browser", "browser()")
              
      ),
      
      # The third tab ===============================================================
      
      
      tabItem("Tab3",
              # titlePanel(htmlOutput('title')),
              box(width = 12, height = "auto",
                  column(width = 3, numericInput("new1", "new1", value = NULL) %>% 
                            PopifyDelayed("title1", "message")),
                  column(width = 3, numericInput("new2", "new2", value = NULL) %>% 
                           PopifyDelayed("title2", "message")),
                  column(width = 3, numericInput("new3", "new3", value = NULL) %>% 
                           PopifyDelayed("title3", "message")),
                  column(width = 2, numericInput("new4", "new4", value = NULL) %>% 
                           PopifyDelayed("title4", "message"))
              ),
              box(width = 12, height = "auto",
                  fluidRow(
                  column(width = 3, numericInput("new5", "new1", value = NULL) %>% 
                           PopifyDelayed("title5", "message")),
                  column(width = 3, numericInput("new6", "new2", value = NULL) %>% 
                           PopifyDelayed("title6", "message")),
                  column(width = 3, numericInput("new7", "new3", value = NULL) %>% 
                           PopifyDelayed("title7", "message")),
                  column(width = 2, numericInput("new8", "new4", value = NULL) %>% 
                           PopifyDelayed("title8", "message"))
                  ),
                  fluidRow(
                    column(width = 3, numericInput("new9", "new1", value = NULL) %>% 
                             PopifyDelayed("title9", "message")),
                    column(width = 3, numericInput("new10", "new2", value = NULL) %>% 
                             PopifyDelayed("title10", "message")),
                    column(width = 3, numericInput("new11", "new3", value = NULL) %>% 
                             PopifyDelayed("title11", "message")),
                    column(width = 2, numericInput("new12", "new4", value = NULL) %>% 
                             PopifyDelayed("title12", "message"))
                  )
              )
                  
                  # numericInput("new2", "new1", value = NULL) %>% 
                  #   PopifyDelayed("title2", "message"),
                  # 
                  # div(class = 'dividerl'),
                  # div(class = 'dividerr'),
                  # 
                  # numericInput("new4", "new1", value = NULL) %>% 
                  #   PopifyDelayed("title3", "message"),
                  # numericInput("new3", "new1", value = NULL) %>% 
                  #   PopifyDelayed("title4", "message")
                  # )
              # mainPanel(  br(),
              #             br(),
              #             fluidRow(
              #               column(6,
              #                      uiOutput('file.input')),
              #               column(4,
              #                      uiOutput('file.input.example'))),
              # 
              #             uiOutput('bond.check.button'),
              #             bsModal("modalExample", "Bond data file example", "data.example", size = "large",
              #                     DTOutput("csv.data")),
              #             br(),
              #             br(),
              #             span(textOutput('successful.message'), style = "color:yellow; font-size:20px"),
              #             span(textOutput('unsuccessful.message'), style = "color:orange; font-size:20px"),
              #             DTOutput("prize.matches"),
              #             br(),
              #             br(),
              #             withSpinner(DTOutput("pbtable"))
              # )
      )
      
    )
  )
  )
    