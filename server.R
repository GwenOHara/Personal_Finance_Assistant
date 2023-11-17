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
  
  
  output$premium_bond_download <- downloadHandler(filename = "premium.bonds.csv",
                                                  content = function(file){
                                                    write.csv(data.table(from = c('bond number 1','bond number 3'),
                                                                         to = c('bond number 2','bond number 4'),
                                                                         owner = c('name i.e. pingu', 'name i.e. gromit')), file, row.names = FALSE)})
  
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
    
    output$bond.download.button <- renderUI(
      downloadButton(outputId = 'premium_bond_download', 
                     label = 'Download example file'))
    
    
    # 
    # output$file.input.example <- renderUI(
    #   actionButton('data.example', 'See example data format for csv file', icon = icon("table"),
    #                style = "color: #fff; background-color: #52606D; border-color: #3e4c59"))
    
    output$file.input <- renderUI(
      fileInput("premium.bonds", "Upload CSV file of your Premium Bonds",
                accept = c(
                  ".csv"))) 
    
    output$csv.data <- DT::renderDT(
      datatable(data.table(from = c('bond number 1','bond number 3'),
                           to = c('bond number 2','bond number 4'),
                           owner = c('name i.e. pingu', 'name i.e. gromit')), rownames = FALSE))
    
  } # end of else after if check.data = NULL
  
  
    # observeEvent(input$data.example, {
    #   toggleModal(session, 'modalExample', toggle = "open")
    # })
    
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
  observe(ShowHideElement('pens.contr.inc', input$making.contributions!=TRUE))
  observe(ShowHideElement('lump.sum', input$lump.sum.1!=TRUE))
  
  observe({
  req(kr_infl)
  updateNumericInputIcon(session, 'inflation', "Inflation Rate", value = kr_infl*100,
                         step = 0.01,
                         icon = icon("percent"))
  })
  
  
  observeEvent({input$gender; input$age}, {
    req(klife_exp, klife_exp2)
    rv$life.exp.gend <- Life_Expectancy(age = input$age, 
                                      gender = input$gender, 
                                      life_exp = klife_exp,
                                      life_exp2 = klife_exp2)
  })
  

  observeEvent({input$current.income; input$pension.pot; input$contribution; 
    input$empl.contribution; input$cash.not.isa; input$cash; input$lisa; input$s_and_s;
    input$other; input$inflation; input$retirement.age; input$sav.growth; input$pen.growth;
    input$making.contributions},{
      req(rv$life.exp.gend,input$age>1)
      

      
      #Make the data based on inputs
      rv$retirement.data <- data.table(age = c(input$age:110))
      list.comp.infl <- CompoundRateListInfl(list.values = if(is.na(input$inflation))kr_infl else input$inflation/100, 
                                         rate = if(is.na(input$inflation))kr_infl else input$inflation/100, 
                                         rows = nrow(rv$retirement.data))-1
      
      rv$retirement.data[ , compound.inflation := list.comp.infl[2:length(list.comp.infl)]]
      rv$retirement.data[, pension.pot := if(is.na(input$pension.pot))0 else input$pension.pot]
      rv$retirement.data[, other.cash := if(is.na(input$cash.not.isa))0 else input$cash.not.isa]
      rv$retirement.data[, cash.isa := if(is.na(input$cash))0 else input$cash]
      rv$retirement.data[, lisa := if(is.na(input$lisa))0 else input$lisa]
      rv$retirement.data[, ss.isa := if(is.na(input$s_and_s))0 else input$s_and_s]
      rv$retirement.data[, other := if(is.na(input$other))0 else input$other]

      rv$retirement.data[, pens.contribution := if(input$making.contributions == TRUE){
         if(is.na(input$current.income))0 else  {
        if(is.na(input$contribution))0 else input$contribution/100*input$current.income}
        }else{0}]
      rv$retirement.data[, employer.contribution := if(input$making.contributions == TRUE){
                           if(is.na(input$current.income))0 else  {
        if(is.na(input$empl.contribution))0 else input$empl.contribution/100*input$current.income}
      }else{0}]
      
      rv$retirement.data[, life.expectancy := rv$life.exp.gend$value]
      
      rv$retirement.data[, total.pens.con := pens.contribution + employer.contribution]
   
      list.pen.c.r <- CompoundRateList(list.values = if(is.na(input$pens.contr.inc))kr_incr else input$pens.contr.inc/100, 
                       rate = if(is.na(input$pens.contr.inc))kr_incr else input$pens.contr.inc/100, 
                       rows = nrow(rv$retirement.data)-1)
      
      rv$retirement.data[, pen.con.rate := c(if(is.na(input$pens.contr.inc))kr_incr else input$pens.contr.inc/100, 
                                             list.pen.c.r[2:length(list.pen.c.r)] )]
      rv$retirement.data[, new.pen.rate := pen.con.rate*100]
      # rv$retirement.data[,  := if(age <= input$retirement.age)
      # {rv$retirement.data$total.pens.con * rv$retirement.data$new.pen.rate
      #   } else {
      #     0}
      #     ]
      
      
      rv$retirement.data[, tot.pens.con.infl := Map(PCont, 
                                      age = rv$retirement.data$age, 
                                      total.pens.con = rv$retirement.data$total.pens.con,
                                      new.pen.rate = rv$retirement.data$new.pen.rate, 
                                      MoreArgs = list(retirement.age = input$retirement.age))]
      
      
      # maximum contribution per year is £40k
      rv$retirement.data[, tot.pens.con.infl := lapply(tot.pens.con.infl,  function(x){min(40000, x)})]
      
      
      pgr <- CompoundRateListInfl(list.values = if(is.null(input$pen.growth))kexpected.returns$med else kexpected.returns[[input$pen.growth]], 
                                  rate = if(is.null(input$pen.growth))kexpected.returns$med else kexpected.returns[[input$pen.growth]], 
                                  rows = nrow(rv$retirement.data))
      
      sgr <- CompoundRateListInfl(list.values = if(is.null(input$sav.growth))kexpected.returns$low else kexpected.returns[[input$sav.growth]], 
                                  rate = if(is.null(input$sav.growth))kexpected.returns$low_med else kexpected.returns[[input$sav.growth]], 
                                  rows = nrow(rv$retirement.data))
      
      rv$retirement.data[, pens.growth.rate := pgr[2:length(pgr)]]
      rv$retirement.data[, sav.growth.rate := sgr[2:length(sgr)]]

      #Adding the annual contributions to the pension pot
      rv$retirement.data[, pens.cont.fin := cumsum(rv$retirement.data$tot.pens.con.infl)]
      rv$retirement.data[, closing.pen.pot := (pension.pot+pens.cont.fin)*(1+pens.growth.rate)]
      
      pension.graph <- rv$retirement.data[, .(age, pension.pot, pens.growth.rate, life.expectancy)]
      
      pension.graph[, new.cont := as.numeric(rv$retirement.data$tot.pens.con.infl)]
      
      pension.graph[1, closing.pen := new.cont+as.numeric(pension.pot)*pens.growth.rate]
      
      closing.pen.list <- PensionTotalList(list.values =  pension.graph$closing.pen[1], 
                               new.contr =  pension.graph$new.cont, 
                               rate =  pension.graph$pens.growth.rate[1],
                               rows = nrow(pension.graph)-1)
      pension.graph[, closing.pen := if(sum(closing.pen.list) ==0)0 else closing.pen.list]
     
      rv$pension.graph <- pension.graph
      rv$graphout <- 1
      
      rv$plotPension <- runif(1)
      
      # rv$retirement.data[ , p.con.inf := rv$retirement.data$pens.contribution * 
      #                      (1- rv$retirement.data$compound.inflation)]
      # rv$retirement.data[ , e.con.inf := rv$retirement.data$employer.contribution * 
      #                      (1- rv$retirement.data$compound.inflation)]
      
      
      #new.table[, new.contr := cumsum(rv$retirement.data$tot.pens.con.infl)]
      #new.table[, pens.tot := pension.pot*pens.growth.rate+new.contr]
    })
  
  observe({
    rv$plotPension
    req(!is.null(rv$graphout),  sum(rv$pension.graph$closing.pen)!=0)
    fig <- plot_ly(data = rv$pension.graph,
                   x = ~closing.pen,
                   y = ~age,
                   type = 'scatter',
                   mode = 'lines') %>%
      layout(xaxis = list(title = "Pension Pot"),
             yaxis = list(title = "Age" )) %>%
      add_trace(y = ~life.expectancy) %>%
      add_trace(y = input$retirement.age)
    
  output$pension.graph <- renderPlotly(fig)
 
  })
  
  # Admin ===============================================================
  
  #observeEvent({input$browser}, browser() )
  
})
