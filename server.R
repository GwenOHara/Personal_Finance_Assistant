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
    prem.bond.prize.data <- ColNumThousandFormat(prem.bond.prize.data, 'Val.of.Bond')
    prem.bond.prize.data <- ColNumThousandFormat(prem.bond.prize.data, 'Prize.Value')
    prem.bond.prize.data <- ColNumThousandFormat(prem.bond.prize.data, 'Total.V.of.Holding')
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
  
  # Share Price Tracker #### ===============================================================
  rv$ftse100 <- ftse100 <- GetFTSE100Stocks(
         do.cache = TRUE,
         cache.folder = file.path(tempdir(), "BGS_Cache"))
  
  rv$sp500 <- sp500 <- GetSP500Stocks(
    do.cache = TRUE,
    cache.folder = file.path(tempdir(), "BGS_Cache"))
  
  benchmarks <- c("^FTSE","^GSPC")

  ftse100tickers <- paste0(ftse100$tickers, ".L")
  sp500tickers.data <- data.table(sp500)[Tickers %in% c("AMZN", "AAPL", "MSFT", 
                                                        "TSLA", "GOOGL", "GOOG", "NVDA", 
                                                        "META", "BRK.B", "PFE", "KO" ,
                                                        "UNH")]
  sp500tickers <- sp500tickers.data$Tickers
  sp500tickers <- gsub("[.]B",'-B',sp500tickers)
  alltickers <- c(ftse100tickers, sp500tickers)
  rv$stocks <- NULL
  
  observeEvent(input$Tabs,{
    if(input$Tabs == "Tab2"){
      
      time.this({
        rv$prices <- tq_get(alltickers, 
                         get  = "stock.prices",
                         from = today()-months(60),
                         to   = today(),
                         complete_cases = F) %>%
          select(symbol,date,close)
        
        rv$bench <- tq_get(benchmarks,
                        get  = "stock.prices",
                        from = today()-months(60),
                        to   = today()) %>%
          select(symbol,date,close)
     }, "Updating share price information...", progress.bar = TRUE)
      
      join.ftse.data <- data.table(company = rv$ftse100$company,
                                    tickers = rv$ftse100$tickers,
                                    sector = rv$ftse100$ICB.sector,
                                   symbol = paste0(rv$ftse100$tickers, ".L"))
      join.sp500.data <- data.table(company = rv$sp500$Company,
                                   tickers = rv$sp500$Tickers,
                                   sector = rv$sp500$GICS.Sector,
                                   symbol = rv$sp500$Tickers)
      join.data <- rbind(join.ftse.data, join.sp500.data)
      
      rv$graph.data <- join.data[data.table(rv$prices), on = .(symbol)]
            
      updatePickerInput(session, 'stocks', 
                        choices = sort(unique(rv$graph.data$company)), 
                        selected = "Aviva")
      rv$stocks <- TRUE
    }
  })
  
  
  # server logic based on user input
  observeEvent(c(input$period,input$stocks,input$benchmark), {
   req(!is.null(rv$stocks))
   
    prices.2 <- data.table(symbol = rv$graph.data$company,
                           date = rv$graph.data$date ,
                           close = rv$graph.data$close)

    prices <- prices.2[symbol %in% if(is.null(input$stocks))"AVvia" else input$stocks]

    if (input$period == 1) {
      prices <- prices[date  >= today()-months(1)]
    }
    
    if (input$period == 2) {
      prices <- prices[date  >= today()-months(3)]
     }
    
    if (input$period == 3) {
      prices <- prices[date  >= today()-months(6)]}
    
    if (input$period == 4) {
      prices <- prices[date  >= today()-months(12)]}
    
    if (input$period == 5) {
      prices <- prices[date  >= today()-months(60)]}
    
    if (input$period == 6) {
      prices <- prices[year(date)  == year(today())]}
   
    bench2 <- data.table(rv$bench)
    
    if (input$benchmark == 1) {
      bench <- bench2[symbol=="^GSPC" & date > min(prices$date)]
     
      prices <- rbind(prices,bench) }
    
    if (input$benchmark == 2) {
      bench <- bench2[symbol=="^FTSE" & date > min(prices$date)]
      prices <- rbind(prices,bench) }
    
    # Create plot
    output$share.price.plot <- renderPlotly({
        ggplotly(prices %>%
                   ggplot(aes(date, close,colour = symbol)) +
                   geom_line(size = 1, alpha = .9) +
                   # uncomment the line below to show area under curves
                   #geom_area(aes(fill=symbol),position="identity",alpha=.2) +
                   theme_minimal(base_size=16) +
                   theme(axis.title=element_blank(),
                         plot.background = element_rect(fill = "black"),
                         panel.background = element_rect(fill="black"),
                         panel.grid = element_blank(),
                         legend.text = element_text(colour="white"))
        )
    })
    




    output$relative.share.price.plot <- renderPlotly({
        ggplotly(prices %>%
                   group_by(symbol) %>%
                   mutate(init_close = if_else(date == min(date),close,NA_real_)) %>%
                   mutate(value = round(100 * close / sum(init_close,na.rm=T),1)) %>%
                   ungroup() %>%
                   ggplot(aes(date, value,colour = symbol)) +
                   geom_line(size = 1, alpha = .9) +
                   # uncomment the line below to show area under curves
                   #geom_area(aes(fill=symbol),position="identity",alpha=.2) +
                   theme_minimal(base_size=16) +
                   theme(axis.title=element_blank(),
                         plot.background = element_rect(fill = "black"),
                         panel.background = element_rect(fill="black"),
                         panel.grid = element_blank(),
                         legend.text = element_text(colour="white"))
      )
    })
    
    current.share.price <-
      if( nrow(prices[date == today()]) == 0){
        prices[date == today()-1]
      } else{
      prices[date == today()]}

    current.share.price[ , close := format(round(current.share.price$close, 2), big.mark = ",")]
    setnames(current.share.price, "symbol", "company")
    
    output$share.price.table <- renderDT(datatable(current.share.price, rownames =FALSE), 
                                         )
  })

  # Retirement Forecaster ===============================================================
  #Source of assumptions
  #https://advisors.vanguard.com/insights/article/series/market-perspectives#projected-returns
  assumptions <- read.csv("https://raw.githubusercontent.com/GwenOHara/Personal_Finance_Assistant/retirement_planner/data/retirement_forecast_assumptions.csv")
  r_stk <- assumptions$global_stocks
  r_bond <- assumptions$global_bonds
  r_infl <- assumptions$inflation
  expected.returns  <- data.table(low = (90*r_stk+10*r_bond)/100, #0%-20% equities
                                              low_med = (70*r_stk+30*r_bond)/100, #20%-40% equities 
                                              med = (50*r_stk+50*r_bond)/100, #40%-60% equities
                                              med_high = (30*r_stk+70*r_bond)/100, #60%-80% equities,
                                              high = (10*r_stk+90*r_bond)/100) #80%-100% equities 
  real.expected.returns <- expected.returns[, lapply(.SD, function(col) col - r_infl), 
                                       .SDcols = colnames(expected.returns)]
  
  
  # Admin ===============================================================
  
  #observeEvent({input$browser}, browser() )
  
})
