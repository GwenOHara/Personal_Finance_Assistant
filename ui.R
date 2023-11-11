
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
    
    useShinyjs(),
    
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
                          span(textOutput('successful.message'), style = "color:blue; font-size:20px"),
                          span(textOutput('unsuccessful.message'), style = "color:black; font-size:20px"),
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
              box(title = 'About You',
                  width = 12, height = "auto", background = 'light-blue',
                  collapsible = TRUE,
                  column(width = 3, numericInput("age", "Your Age", value = 40) %>% 
                            PopifyDelayed("Age", "Enter your current age in years")),
                  column(width = 2, 
                         radioGroupButtons(
                           inputId = "gender",
                           label = "Gender",
                           choices = c("Female", "Male"))),
                  column(width = 3, numericInput("retirement.age", "Retirement Age", value = 55) %>% 
                           PopifyDelayed("Retirement Age", "Enter the age you plan to retire")),
                  column(width = 3, numericInputIcon("current.income", "What is your current annual income (before tax)", value = NULL,
                                                 icon = icon("sterling-sign")) %>% 
                           PopifyDelayed("title3", "Enter your cuurrent income so that target income can be calculated"))#,
              ),
              
              box(title = 'Your Current Financial Position',
                  width = 12, height = "auto", background = 'light-blue',
                  collapsible = TRUE,
                  fluidRow(
                    column(width = 11,
                           checkboxInput('making.contributions', label = "Are you still making pension contributions?"))),
                  fluidRow(
                  column(width = 3, numericInputIcon("pension.pot", "Pensions", value = NULL,
                                                     icon = icon("sterling-sign")) %>% 
                           PopifyDelayed("Pensions Pots", "How much have you got saved up in your pension pots so far?")),
                 
                column(width = 3, 
                         shinyjs::hidden(numericInputIcon(
                    inputId = "contribution",
                    label = "Your contribution",
                    value = 10,
                    icon = icon("percent")
                  ) %>% 
                           PopifyDelayed("Employee contribution", "Enter the % you are contributing"))),
                  
                  column(width = 2, 
                         shinyjs::hidden(numericInputIcon(
                           inputId = "empl.contribution",
                           label = "Employer contribution",
                           value = 8,
                           icon = icon("percent")
                         ) %>% 
                           PopifyDelayed("Employee contribution", "Enter the % your employer contributes")))
                  ),
                  fluidRow(
                    column(width = 3, numericInputIcon("cash.not.isa", "Value of Cash Savings", value = NULL,
                                                       icon = icon("sterling-sign")) %>% 
                             PopifyDelayed("c", "Enter your current income so that target income can be calculated"))
                  ),
                  fluidRow(
                    column(width = 3, numericInputIcon("cash", "Value of Cash ISAs", value = NULL,
                                                       icon = icon("sterling-sign")) %>% 
                             PopifyDelayed("c", "Enter your cuurrent income so that target income can be calculated")),
                    column(width = 3, numericInputIcon("lisa", "Value of Lifetime ISAs", value = NULL,
                                                       icon = icon("sterling-sign")) %>% 
                             PopifyDelayed("c", "Enter your cuurrent income so that target income can be calculated")),
                    column(width = 3, numericInputIcon("s_and_s", "Value of Stocks & Shares ISAs", value = NULL,
                                                       icon = icon("sterling-sign")) %>% 
                             PopifyDelayed("c", "Enter your cuurrent income so that target income can be calculated")),
                    column(width = 2, numericInputIcon("other", "Value of other investments", value = NULL,
                                                       icon = icon("sterling-sign")) %>% 
                             PopifyDelayed("c", "Enter your cuurrent income so that target income can be calculated")),
                  )
              ),
              box(title = 'Your Assumptions',
                  width = 12, height = "auto", background = 'light-blue',
                  collapsible = TRUE,
                  fluidRow(
                    column(width = 11,
                           checkboxInput('lump.sum.1', label = "Want cash Lump Sum at retirement?"))),
                  column(width = 3, numericInput("state.pens", "State pesnions", value = 10600) %>% 
                           PopifyDelayed("b", "Enter the age you plan to retire")),
                  column(width = 3, numericInputIcon("inflation", "inflation Rate", value = NULL,
                                                            icon = icon("percent")) %>% 
                                  PopifyDelayed("c", "Enter the expected long-term inlfation rate")),
                   column(width = 2, 
                          radioGroupButtons(
                  inputId = "growth",
                  label = "Growth Rate",
                  choices = c("low", "low-medium", 
                              "medium", "medium-high", "high"))
              )# %>% 
                  #          PopifyDelayed("title4", "message"))
              ,
              column(width = 3, sliderInput("lump.sum", "Select % of pension to take", 
                                            min = 0, max = 25, value = 25) %>% 
                       PopifyDelayed("a", "Enter your current age in years"))
              )
              
                  
             
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
    