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
  
  # rv$ftse100 <- kftse100
  # rv$sp500 <- ksp500 
  # rv$stocks <- NULL
  # 
  # observe({
  #   req(kftse100tickers, ksp500tickers.data)
  #   rv$alltickers <- GetAllTickers(kftse100tickers, ksp500tickers.data)
  # })
  # 
  # observeEvent(input$Tabs,{
  #   if(input$Tabs == "Tab2")LoadStockData(rv = rv, session = session)
  # })
  # 
  # observeEvent(c(input$period,input$stocks,input$benchmark), {
  #   req(!is.null(rv$stocks))
  #   prices <- Shares_Calculate_Data_For_Graphs(rv, input)
  #   
  #   # Create plot
  #   output$share.price.plot <- renderPlotly({
  #     SharePriceGraph1(prices)
  #   })
  #   
  #   output$relative.share.price.plot <- renderPlotly({
  #     SharePriceGraph2(prices)
  #   })
  #   
  #   output$share.price.table <- renderDT(datatable(SharePriceTable(prices), 
  #                                                  rownames =FALSE), 
  #   )
  # })
  
  # Retirement Forecaster ===============================================================
  #Source of assumptions
  #Projected return growth for investments
  #https://advisors.vanguard.com/insights/article/series/market-perspectives#projected-returns
  
  #Life expectancy
  #https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/expectationoflifeprincipalprojectionunitedkingdom
  
  #Current annuity rates
  #https://www.hl.co.uk/retirement/annuities/best-buy-rates
  #or
  #https://www.sharingpensions.co.uk/annuity_rates.htm
  #or
  #https://www.retirementline.co.uk/annuities/annuity-rates/
  
  
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
  observe(ShowHideElement('annuity.pie.graph', input$lump.sum.1!=TRUE))
  observe(ShowHideElement('drawdown.amount', input$pension.option!=TRUE))
  observe(ShowHideElement('annuity.table.title', input$pension.option==TRUE))
  observe(ShowHideElement('annuity.table.inf', input$pension.option==TRUE))

  observeEvent({input$current.income ;input$pension.option}, {
    req(input$pension.option==TRUE, input$current.income > 0)
    if(input$current.income <= 14300) income.level <- 0.8 * input$current.income
    if(input$current.income > 14301 && input$current.income <= 37600) income.level <- 0.7 * input$current.income
    if(input$current.income > 37601 && input$current.income <= 60000) income.level <- 0.6 * input$current.income
    if(input$current.income >= 60001) income.level <- 0.5 * input$current.income
    
    updateNumericInput(session, 'drawdown.amount', "Amount to draw from pension per year", value = income.level)
    })
  
  observe({
    req(kr_infl)
    updateNumericInputIcon(session, 'inflation', "Inflation Rate", value = kr_infl*100,
                           step = 0.01,
                           icon = icon("percent"))
  })
  
  output$file.input.ret <- renderUI(
    fileInput("ret.file.preferences", "Upload CSV file with your setting preferences",
              accept = c(
                ".csv"))) 
  
  observeEvent(input$ret.file.preferences, {
    
    settings <- read.csv(input$ret.file.preferences$datapath, header = TRUE, stringsAsFactors = FALSE)
    
    
    Map(RetirementSettingUI,
        id = settings$app.id,
        type = settings$input.type,
        value = settings$setting,
        MoreArgs = list(session = session))
    
    
  })
  
  output$download.button.ret <- renderUI(
    downloadButton(outputId = 'retirement_download', 
                   label = 'Download example file'))
  
  #Example retirement setting csv file
  output$retirement_download <- downloadHandler(
    filename = "retirement.forecast.csv",
    content = function(file){
      write.csv(data.table(app.id = c('age', 'gender', 'retirement.age',
                                      'current.income', 'pension.pot',
                                      'making.contributions', 'contribution',
                                      'empl.contribution', 'pen.growth', 'take.pens.age',
                                      'inv.change'),
                           input.type = c('numericInput',
                                          'radioGroupButtons',
                                          'numericInput',
                                          'numericInputIcon',
                                          'numericInputIcon',
                                          'checkboxInput',
                                          'numericInputIcon',
                                          'numericInputIcon',
                                          'radioGroupButtons',
                                          'numericInputIcon',
                                          'numericInputIcon'),
                           info = c('Age',
                                    'Gender',
                                    'Retirement Age',
                                    'Annual Income',
                                    'Pension',
                                    'Pension Contributions being made?',
                                    'Your Contribution',
                                    'Employers Contribution',
                                    'Pension Growth Rate',
                                    'Age start personal pension',
                                    'Years before retirement to move to low risk invesments'),
                           setting = c('35',
                                       'Female',
                                       '68',
                                       '30000',
                                       '100000',
                                       'TRUE',
                                       '8',
                                       '4',
                                       'medium',
                                       '60',
                                       '5')), file, row.names = FALSE)})
  
  
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
    input$making.contributions; input$take.pens.age; input$inv.change; input$pension.option;
    input$lump.sum; input$lump.sum.1; input$age},{
      req(rv$life.exp.gend,input$age>1)
     
      if(!is.na(input$inflation)){
        #Adjust expected returns for inflation and platform fees(0.5%)
        kexpected.returns <- kexpected.returns - input$inflation/100 - 0.005
      }      
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
   
      if(input$making.contributions == TRUE){
      rv$retirement.data[, pens.contribution := if(is.na(input$current.income))0 else  {
          if(is.na(input$contribution))0 else input$contribution/100*input$current.income}]
        
        rv$retirement.data[, employer.contribution := if(is.na(input$current.income))0 else  {
          if(is.na(input$empl.contribution))0 else input$empl.contribution/100*input$current.income}]
        
        }
      
      if(input$making.contributions == FALSE){
        rv$retirement.data[, pens.contribution := 0]
        rv$retirement.data[, employer.contribution := 0]
        }
      
      
      rv$retirement.data[, life.expectancy := rv$life.exp.gend$value]
      
      rv$retirement.data[, total.pens.con := pens.contribution + employer.contribution]
      
      list.pen.c.r <- CompoundRateList(list.values = if(is.na(input$pens.contr.inc))kr_incr else input$pens.contr.inc/100, 
                                       rate = if(is.na(input$pens.contr.inc))kr_incr else input$pens.contr.inc/100, 
                                       rows = nrow(rv$retirement.data)-1)
      
      rv$retirement.data[, pen.con.rate := c(if(is.na(input$pens.contr.inc))kr_incr else input$pens.contr.inc/100, 
                                             list.pen.c.r[2:length(list.pen.c.r)] )]
      rv$retirement.data[, new.pen.rate := pen.con.rate*100]
      
      rv$retirement.data[, tot.pens.con.infl := Map(PCont, 
                                                    age = rv$retirement.data$age, 
                                                    total.pens.con = rv$retirement.data$total.pens.con,
                                                    new.pen.rate = rv$retirement.data$new.pen.rate, 
                                                    MoreArgs = list(retirement.age = input$retirement.age))]
      
      
      # maximum contribution per year is £40k
      rv$retirement.data[, tot.pens.con.infl := lapply(tot.pens.con.infl,  function(x){min(40000, x)})]
      
      pgr <- CompoundRateListInfl(list.values = if(is.null(input$pen.growth))kexpected.returns$medium else kexpected.returns[[input$pen.growth]], 
                                  rate = if(is.null(input$pen.growth))kexpected.returns$medium else kexpected.returns[[input$pen.growth]], 
                                  rows = nrow(rv$retirement.data))
      
      
      sgr <- CompoundRateListInfl(list.values = if(is.null(input$sav.growth))kexpected.returns$low else kexpected.returns[[input$sav.growth]], 
                                  rate = if(is.null(input$sav.growth))kexpected.returns$low_medium else kexpected.returns[[input$sav.growth]], 
                                  rows = nrow(rv$retirement.data))
      
      rv$retirement.data[, pens.growth.rate := pgr[2:length(pgr)]]
      rv$retirement.data[, sav.growth.rate := sgr[2:length(sgr)]]
      
      #Make the lowest growth rate based on the age that the user wants to move pension to lower risk investments
      if(!is.na(input$inv.change)){
        req(input$inv.change > 0)
        change.age <- input$take.pens.age - input$inv.change
        if(change.age >0 ){
          pension.growth.rate.at.age <- rv$retirement.data[age == change.age, c(pens.growth.rate)]
          low.pgr <- CompoundRateListInfl(list.values = kexpected.returns$low, 
                                          rate = kexpected.returns$low, 
                                          rows = nrow(rv$retirement.data))
          low.pgr.new <- low.pgr[2:length(low.pgr)]*pension.growth.rate.at.age
          row.num.change <- nrow(rv$retirement.data) - which(rv$retirement.data$age == change.age)+1
          rv$retirement.data[age >= change.age , pens.growth.rate := low.pgr.new[1:row.num.change]]
        } else {
          shinyalert("Years to move Investments to Low Risk", "The number of years enterred are too many to calculate, please adjust", type = "error")
          req(FALSE)
        }
        
      }
      
      
      #Adding the annual contributions to the pension pot
      pension.graph <- rv$retirement.data[, .(age, pension.pot, pens.growth.rate, life.expectancy)]
      
      pension.graph[, new.cont := as.numeric(rv$retirement.data$tot.pens.con.infl)]
      
      pension.graph[1, closing.pen := new.cont+as.numeric(pension.pot)*pens.growth.rate]
      
      closing.pen.list <- PensionTotalList(list.values =  pension.graph$closing.pen[1], 
                                           new.contr =  pension.graph$new.cont, 
                                           rate =  pension.graph$pens.growth.rate[1],
                                           rows = nrow(pension.graph)-1)
      
      
      pension.graph[, closing.pen := if(sum(closing.pen.list) ==0)0 else closing.pen.list]
      
      
      
      if(change.age >0 & !is.null(change.age)){
        
        growth.change <- pension.graph[age == change.age, c(closing.pen)]  
        closing.pen.list.lower <- PensionTotalList(list.values =  growth.change, 
                                                   new.contr = rep(0,nrow(pension.graph)), 
                                                   rate =  1+kexpected.returns$low,
                                                   rows = nrow(pension.graph)-1)
        if(sum(closing.pen.list > 0)){
          new.closing.pen.list.a <- c(closing.pen.list[1:which(closing.pen.list == closing.pen.list.lower[1])])
          rows.pg <- nrow(pension.graph) - length(new.closing.pen.list.a)+1
          new.closing.pen.list.b <- c(new.closing.pen.list.a,
                                      closing.pen.list.lower[2:rows.pg])
          
          pension.graph[, closing.pen := if(sum(new.closing.pen.list.b) ==0)0 else new.closing.pen.list.b]
          
          rv$lump.sum.base <- pension.graph[age == input$take.pens.age, closing.pen]
          
          if(input$lump.sum.1){
            #Adjust Pension amount for taking tax free amount at start of taking pension
            lump.sum.amount <- rv$lump.sum.base * input$lump.sum/100
            lump.sum.growth.rate <- pension.graph[age >= input$take.pens.age, pens.growth.rate]/ pension.graph[age >= input$take.pens.age, pens.growth.rate][1]
            lump.sum.list <- lump.sum.amount *lump.sum.growth.rate
            
            pension.graph[age >= input$take.pens.age, lump.sum.adj := lump.sum.list]
            pension.graph[is.na(pension.graph)] <-0
            pension.graph[, closing.pen := closing.pen - lump.sum.adj][, lump.sum.adj := NULL]
          }
          
        }
        
      }
      
      if(!(input$pension.option)){
        pension.graph <- pension.graph[1:which(age == input$take.pens.age)]
        annuity <- pension.graph[.N, closing.pen]
        annuity.rate.table <- round((kAnnuity.Rates[, c('55','60', '65', '70', '75')]/100000 * annuity), 0)
        
        
        #Work out inflation rate to adjust back to todays money
        today.inflation.back.adjustment.rate <- CompoundRateListInfl(list.values = rv$retirement.data[1, compound.inflation], 
                                                                     rate = rv$retirement.data[1, compound.inflation], 
                                                                     rows = nrow(pension.graph)+1)
        annuity.rate.tabl.infl.adj <- round((annuity.rate.table/tail(today.inflation.back.adjustment.rate,1)),0)
      
        annuity.rate.tabl.infl.adj <- ColNumThousandFormat(annuity.rate.tabl.infl.adj, '55')
        annuity.rate.tabl.infl.adj <- ColNumThousandFormat(annuity.rate.tabl.infl.adj, '60')
        annuity.rate.tabl.infl.adj <- ColNumThousandFormat(annuity.rate.tabl.infl.adj, '65')
        annuity.rate.tabl.infl.adj <- ColNumThousandFormat(annuity.rate.tabl.infl.adj, '70')
        annuity.rate.tabl.infl.adj <- ColNumThousandFormat(annuity.rate.tabl.infl.adj, '75')
        
        rv$annuity.rate.table.adj <- cbind(Type = kAnnuity.Rates[,1], annuity.rate.tabl.infl.adj)
      } else {
        
      }
      
      rv$pension.graph <- pension.graph
      rv$graphout <- 1
      
      rv$plotPension <- runif(1)
      
    })
  
  observe({
    rv$plotPension
    output$pension.graph <- NULL
    req(!is.null(rv$graphout),  sum(rv$pension.graph$closing.pen)!=0)
    fig <- plot_ly(data = rv$pension.graph,
                   y = ~closing.pen,
                   x = ~age,
                   name = 'Forecast Pension Value',
                   type = 'scatter',
                   mode = 'lines',
                   line = list(width = 5),
                   fill = 'tonexty') %>%
      layout(title = 'Pension Pot Forecast',
             yaxis = list(title = "Pension Pot"),
             xaxis = list(title = "Age" )) %>%
      add_trace(x = ~life.expectancy, name = 'Average Life Expectancy', fill = NA,
                line = list(dash = 'dot')) %>% 
      add_trace(x = input$retirement.age, name = 'Retirement Age', fill = NA,
                line = list(dash = 'dash')) %>%
      add_trace(x = input$take.pens.age, name = 'Pension Start', fill = NA,
                line = list(dash = 'dash'))
    
    # Lump Sum Graph 
    value.annuity <- rv$lump.sum.base
    pie.data <-  data.table(class = c('lump sum', 'annuity'),
                            amount = c(
                              if(input$lump.sum.1){
                                input$lump.sum/100 *  value.annuity} else {0},
                              if(input$lump.sum.1){
                                (1 - (input$lump.sum/100)) * value.annuity}
                              else {value.annuity}))
    
    pie <- plot_ly(data = pie.data,
                   values = ~amount,
                   labels = ~factor(class),
                   marker = list(colors = c("green", "purple")),
                   type = "pie",
                   textinfo = 'text + values',
                   text = paste(round(pie.data$amount,0))) %>%
      layout(title = if(!(input$lump.sum.1)) "Remaining Pension Pot" else 
        "Lump Sum and Remaining Pension Pot")
    
    output$annuity.pie.graph <- renderPlotly(pie)
    
    output$pension.graph <- renderPlotly(fig)
    
    output$annuity.table.title <- renderText(paste("<b>",'Potential Annuity Amounts in todays value', "<b>"))
    output$annuity.table.inf <- renderDT(rv$annuity.rate.table.adj, rownames =FALSE, 
                                     options = list(
                                       pageLength = 1000,
                                       pagination = FALSE
                                     )) 
    
    
  })
  
  # Admin ===============================================================
  
  #observeEvent({input$browser}, browser() )
  
})
