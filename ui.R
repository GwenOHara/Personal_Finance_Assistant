
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
      
      # menuItem("Stocks & Shares", icon = icon("money-check-dollar"), startExpanded = TRUE,
      #          menuSubItem("Share Prices", tabName = "Tab2", icon = icon("chart-line"))#,
      #          #menuSubItem("Tab 3", tabName = "Tab3", icon = icon("chart-line"))
      # ),
      
      menuItem("Retirement Plan", tabName = 'Tab2', icon = icon("coins"))
      
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
      
      # Premium Bonds ===============================================================
      tabItem("Tab1",
              fluidPage(theme = shinytheme("cyborg"),
              titlePanel(htmlOutput('title')),
              mainPanel(  br(),
                          br(),
                          fluidRow(
                            column(6, 
                                   uiOutput('file.input')),
                            div(style = "margin-top:25px;",
                            uiOutput('bond.download.button'))
                            ),
                           uiOutput('bond.check.button'),
                          br(),
                          span(textOutput('successful.message'), style = "color:blue; font-size:20px"),
                          span(textOutput('unsuccessful.message'), style = "color:black; font-size:20px"),
                          DTOutput("prize.matches"),
                          br(),
                          withSpinner(DTOutput("pbtable"))
              ))
      ),
      
      # Share Prices ===============================================================
      # tabItem("Tab3",
      #         #This tab is adapted from the basic code from:
      #         # A shiny app for monitoring a stock portfolio and comparing stock performance
      #         # January 2021
      #         # Peer Christensen
      #         # hr.pchristensen@gmail.com
      #         fluidPage(theme = shinytheme("cyborg"),
      #           
      #           # Title
      #           titlePanel("Shares"),
      #           
      #           # Sidebar 
      #           sidebarLayout(
      #             sidebarPanel(width = 3,
      #                          
      #                          # Let user pick stocks
      #                          pickerInput(
      #                            inputId = "stocks",
      #                            label = h4("Shares"),
      #                            choices = NULL,
      #                            options = list(`actions-box` = TRUE, 
      #                                           liveSearch = TRUE), 
      #                            multiple = T
      #                          ),
      #                          
      #                          # Pick time period
      #                          radioButtons("period", label = h4("Period"),
      #                                       choices = list("1 month" = 1, "3 months" = 2, "6 months" = 3, 
      #                                                      "12 months" = 4, "5 years" = 5, "YTD" = 6), 
      #                                       selected = 4
      #                          ),
      #                          
      #                          # Pick benchmark
      #                          radioButtons("benchmark", label = h4("Benchmark"),
      #                                       choices = list("SP500" = 1, "FTSE100" = 2,"None" = 3),
      #                                       selected = 3)
      #                         
      #                          
      #             ),
      #             
      #             # Plot results
      #             mainPanel(
      #               h4(tags$b('Last Closing Share Price')),
      #               DTOutput('share.price.table'),
      #               h4(tags$b('Share Price')),
      #               withSpinner(plotlyOutput("share.price.plot",height=800)),
      #               br(),
      #               h4(tags$b('Baselined to 100')),
      #               withSpinner(plotlyOutput("relative.share.price.plot",height=800))
      #             )
      #           )
      #         )
      #         
      #         # "Here's a browser button. Use it when developing to pause the app and run ad-hoc code:",
      #         # actionButton("browser", "browser()")
      #         
      # ),
      # 
      # Retirement Planning ===============================================================
      
      
      tabItem("Tab2",
              fluidRow(
                column(6, 
                       uiOutput('file.input.ret')),
                div(style = "margin-top:25px;",
                    uiOutput('download.button.ret'))
              ),
              # titlePanel(htmlOutput('title')),
              box(title = 'About You',
                  width = 12, height = "auto", background = 'light-blue',
                  collapsible = TRUE,
                  column(width = 1, numericInput("age", "Your Age", value = 40) %>% 
                            PopifyDelayed("Age", "Enter your current age in years")),
                  column(width = 2, 
                         radioGroupButtons(
                           inputId = "gender",
                           label = "Gender",
                           choices = c("Female", "Male"))),
                  column(width = 1, numericInput("retirement.age", "Retirement Age", value = 55) %>% 
                           PopifyDelayed("Retirement Age", "Enter the age you plan to retire")),
                  column(width = 2, numericInputIcon("current.income", "What is your current annual income (before tax)", value = NULL,
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
                 
                column(width = 2, 
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
                           PopifyDelayed("Employee contribution", "Enter the % your employer contributes"))),
                column(width = 2, 
                       shinyjs::hidden(numericInputIcon(
                         inputId = "pens.contr.inc",
                         label = "Annual Change In Contribution",
                         value = 1,
                         icon = icon("percent")
                       ) %>% 
                         PopifyDelayed("Annual increase", "Assumed increase in contributions each year due to pay rise etc.")))
                  ),
                  fluidRow(
                    column(width = 3, numericInputIcon("cash.not.isa", "Value of Cash Savings", value = NULL,
                                                       icon = icon("sterling-sign")) %>% 
                             PopifyDelayed("c", "Enter your current income so that target income can be calculated"))
                  ),
                  fluidRow(
                    column(width = 3, numericInputIcon("cash", "Value of Cash ISAs", value = NULL,
                                                       icon = icon("sterling-sign")) %>% 
                             PopifyDelayed("c", "Enter your current income so that target income can be calculated")),
                    column(width = 3, numericInputIcon("lisa", "Value of Lifetime ISAs", value = NULL,
                                                       icon = icon("sterling-sign")) %>% 
                             PopifyDelayed("c", "Enter your current income so that target income can be calculated")),
                    column(width = 3, numericInputIcon("s_and_s", "Value of Stocks & Shares ISAs", value = NULL,
                                                       icon = icon("sterling-sign")) %>% 
                             PopifyDelayed("c", "Enter your current income so that target income can be calculated")),
                    column(width = 2, numericInputIcon("other", "Value of other investments", value = NULL,
                                                       icon = icon("sterling-sign")) %>% 
                             PopifyDelayed("c", "Enter your current income so that target income can be calculated")),
                  ),
                fluidRow(
                  column(width = 3, numericInputIcon("con.cash", "Annual savings Cash ISAs", value = NULL,
                                                     icon = icon("sterling-sign")) %>% 
                           PopifyDelayed("c", "Enter your expected annual savings")),
                  column(width = 3, numericInputIcon("con.lisa", "Annual savings Lifetime ISAs", value = NULL,
                                                     icon = icon("sterling-sign")) %>% 
                           PopifyDelayed("c", "Enter your expected annual savings")),
                  column(width = 3, numericInputIcon("con.s_and_s", "Annual savings Stocks & Shares ISAs", value = NULL,
                                                     icon = icon("sterling-sign")) %>% 
                           PopifyDelayed("c", "Enter your expected annual savings")),
                  column(width = 3, numericInputIcon("con.other", "Annual savings Other", value = NULL,
                                                     icon = icon("sterling-sign")) %>% 
                           PopifyDelayed("c", "Enter your expected annual savings"))
                )
              ),
              box(title = 'Your Assumptions',
                  width = 12, height = "auto", background = 'light-blue',
                  collapsible = TRUE,
                  fluidRow(
                    column(width = 11,
                           checkboxInput('lump.sum.1', label = "Want cash Lump Sum at retirement?"))),
                  fluidRow(
                    column(width = 3, numericInput("state.pens", "State pensions", value = 10600) %>% 
                           PopifyDelayed("b", "Enter the age you plan to retire")),
                  column(width = 3, numericInputIcon("inflation", "Inflation Rate", value = NULL,
                                                     step = 0.01,
                                                     icon = icon("percent")) %>% 
                                  PopifyDelayed("c", "Enter the expected long-term inflation rate")),
                   column(width = 2, 
                          radioGroupButtons(
                  inputId = "pen.growth",
                  label = "Pension Growth Rate",
                  choices = c("low", "low-medium", 
                              "medium", "medium-high", "high"),
                  selected = "medium-high")
                  ),
                  column(width = 2, 
                         radioGroupButtons(
                           inputId = "sav.growth",
                           label = "Savings Growth Rate",
                           choices = c("low", "low-medium", 
                                       "medium", "medium-high", "high"))
                  
              )
              )# %>% 
                  #          PopifyDelayed("title4", "message"))
              ,
              fluidRow(
                column(width = 2, numericInput("take.pens.age", "Age Start Taking Pension", value = 55) %>% 
                         PopifyDelayed("Start Pension Age", "Enter the age you plan to start taking your pwersonal pension")),
                column(width = 2, numericInput("inv.change", "Years before retirement to move investments to lower risk", value = 5) %>% 
                         PopifyDelayed("Investment Change", "Enter how many years before taking pension you want to move your investments to low risk investments")),
                column(width = 2, switchInput('pension.option', label = 'Pension Type', onLabel = 'Drawdown' , offLabel = 'Annuity')),
                column(width = 2, shinyjs::hidden(numericInput("drawdown.amount", "Amount to draw from pension per year", value = NULL)) %>% 
                         PopifyDelayed("Pension Drawdown Amount", "Enter the amont you think you will need to draw from your pension each year to cover expenses.  This has been populated based on your current income using Department of Work and Pensions guidelines")),
                
              ),
              
              column(width = 3, 
                     sliderTextInput(
                       inputId = "lump.sum",
                       label = "Select tax free % of pension to take:", 
                       choices = seq(from = 25,
                                     to = 0,
                                     by = -1),
                       grid = TRUE
                     )%>% 
                       PopifyDelayed("a", "Enter your current age in years"))),
              
              fluidRow(
                box(width = 12,
                    div (style = 'overflow-y:hidden; height:calc(100vh-300px_; width:100%',
                         plotlyOutput('pension.graph')))
              ),
              
              
              fluidRow(
                box(width = 12,
                    div (style = 'overflow-y:hidden; height:calc(100vh-300px_; width:100%',
                         htmlOutput('annuity.table.title'),
                         br(),
                         DTOutput("annuity.table.inf"),
                        plotlyOutput('annuity.pie.graph')))
              )
              
                  
      )
      
    )
  )
  )
    