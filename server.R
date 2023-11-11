# A shiny app for:
#Checking high value premium bond winners
#monitoring a stock portfolio and comparing stock performance
# Novermber 2023
# Gwen O'Hara


# Note what environment we're in (only used for a label by default; can be used to prevent overwriting live data from a test app)
envir <- Sys.getenv('ENVIR')
if(envir=='')  envir <- 'dev'


# Define server logic required to draw a histogram
shinyServer(function(input, output, session = getDefaultReactiveDomain()) {
  
  # Init ===============================================================
  
  rv <- reactiveValues()
  output$style_tag <- renderUI({
    if(input$Tabs=='Tab1')
      return(tags$head(tags$style(HTML('.content-wrapper {background-color:#a7d6d4;}'))))

    if(input$Tabs=='Tab2')
      return(tags$head(tags$style(HTML('.content-wrapper {background-color:a7d6d4;}'))))
  })
  
  
  # Display the environment (e.g. live/test/dev) in the title bar
  # output$EnvirUI <- renderUI({
  #   dashboardLabel(toupper(envir), status='info')
  # })
  
  # High Value Premium Bond Prize Checker #### ===============================================================
  
  output$title <- renderText(paste("NS&I Premium Bonds High Value Prize Checker", format(Sys.Date(), "%B %Y")))
  
 
  #Check if the file exists on NS&I website 
  check.data <- CheckPrizeFileExists(newFile = knewFile)
  
  #Output the prize data
  if(is.null(check.data)){
    shinyalert("File not found", "Current months file is not available on NS&I website", type = "error")
  } else {
    #Getting prize data 
    rv$prem.bonds.prize.data <- prem.bond.prize.data <- GetPrizeData(newFile = knewFile)
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
    
    output$csv.data <- DT::renderDT(
      datatable(data.table(from = c('bond number 1','bond number 3'),
                           to = c('bond number 2','bond number 4'),
                           owner = c('name i.e. pingu', 'name i.e. gromit')), rownames = FALSE))
    
  } # end of else after if check.data = NULL
  
  
    observeEvent(input$data.example, {
      toggleModal(session, 'modalExample', toggle = "open")
    })
    
    observeEvent(input$bondchecker,{
      if(is.null(input$premium.bonds)) 
      {shinyalert("No Premium Bonds loaded to check", "Please upload your csv file containing your bond numbers", type = "warning")}
      
      req(input$premium.bonds)
      showNotification("Checking bond numbers", duration = 2,  type = "warning")
      
      bonds <- read.csv(input$premium.bonds$datapath)
      all.bonds <- MakeBondNumbers(bonds = bonds)
      
      if(any(rv$prem.bonds.prize.data$`Bond Number` %in% all.bonds$bonds)){
        filter.match.table <- MakeMatchTable(output = output, 
                                             prem.bonds.prize.data = rv$prem.bonds.prize.data, 
                                             all.bonds = all.bonds)
        
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
    
  #} 
  
  # Share Price Tracker #### ===============================================================
  
  rv$ftse100 <- kftse100
  rv$sp500 <- ksp500 
  rv$stocks <- NULL
 
   observe({
    req(kftse100tickers, ksp500tickers.data)
    rv$alltickers <- GetAllTickers(kftse100tickers, ksp500tickers.data)
  })
  
  observeEvent(input$Tabs,{
    if(input$Tabs == "Tab2")LoadStockData(rv = rv, session = session)
  })
  
  observeEvent(c(input$period,input$stocks,input$benchmark), {
   req(!is.null(rv$stocks))
    prices <- Shares_Calculate_Data_For_Graphs(rv, input)

  # Create plot
    output$share.price.plot <- renderPlotly({
      SharePriceGraph1(prices)
    })
    
    output$relative.share.price.plot <- renderPlotly({
      SharePriceGraph2(prices)
      })
    
    output$share.price.table <- renderDT(datatable(SharePriceTable(prices), 
                                                   rownames =FALSE), 
                                         )
  })

  # Retirement Forecaster ===============================================================
  #Source of assumptions
  #https://advisors.vanguard.com/insights/article/series/market-perspectives#projected-returns
  
  #https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/expectationoflifeprincipalprojectionunitedkingdom
  
  
  # When calculating  what income is needed in retirement use Department of Work and Pensions guidelines of:
  # Up to £14,300 -> 80% of current income
  # £14,301 - £26,300 -> 70% of current income
  # £26,301 - £37600 -> 70% of current income
  # £37601 - £60000 -> 60% of current income
  # Over £60000 -> 50% of current income
  
  observe(ShowHideElement('salary', input$making.contributions!=TRUE))
  observe(ShowHideElement('contribution', input$making.contributions!=TRUE))
  observe(ShowHideElement('empl.contribution', input$making.contributions!=TRUE))
  observe(ShowHideElement('lump.sum', input$lump.sum.1!=TRUE))
  
  
  observeEvent({input$gender; input$age}, {
    req(klife_exp, klife_exp2)
    rv$life.exp.gend <- Life_Expectancy(age = input$age, 
                                      gender = input$gender, 
                                      life_exp = klife_exp,
                                      life_exp2 = klife_exp2)
  })
  

  observeEvent({input$current.income; input$pension.pot; input$contribution; 
    input$empl.contribution; input$cash.not.isa; input$cash; input$lisa; input$s_and_s;
    input$other},{
      req(rv$life.exp.gend)
    })
  
  #rv$retirement.forecast <- data.table( years = c(1:round(rv$life.exp$year.left,0)))  
  
  
  
  # Admin ===============================================================
  
  #observeEvent({input$browser}, browser() )
  
})
