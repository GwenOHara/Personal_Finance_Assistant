####Fixed Variables Premium Bonds ####
# Determine the current month 
kdate <- tolower(format(Sys.Date(), "%B-%Y"))
# Determine the url based on this month
knewFile <- paste0("https://www.nsandi.com/files/asset/xlsx/prize-", kdate, ".xlsx")

#### Fixed Variables Share Price ####

kbenchmarks <- c("^FTSE","^GSPC")

kftse100 <- GetFTSE100Stocks(
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"))

ksp500 <- GetSP500Stocks(
  do.cache = TRUE,
  cache.folder = file.path(tempdir(), "BGS_Cache"))

kftse100tickers <- paste0(kftse100$tickers, ".L")
ksp500tickers.data <- data.table(ksp500)[Tickers %in% c("AMZN", "AAPL", "MSFT", 
                                                       "TSLA", "GOOGL", "GOOG", "NVDA", 
                                                       "META", "BRK.B", "PFE", "KO" ,
                                                       "UNH")]

####Fixed Variables Retirement Planner ####

klife_exp <- read.csv("https://raw.githubusercontent.com/GwenOHara/Personal_Finance_Assistant/retirement_planner/data/life_expectancy.csv",
                     check.names = FALSE)
klife_exp2 <- melt(data.table(klife_exp), id.vars = c('sex'))
kassumptions <- read.csv("https://raw.githubusercontent.com/GwenOHara/Personal_Finance_Assistant/retirement_planner/data/retirement_forecast_assumptions.csv")
kAnnuity.Rates <- read.csv("https://raw.githubusercontent.com/GwenOHara/Personal_Finance_Assistant/retirement_planner/data/annuity.rates.csv",
                           check.names = FALSE)
kr_stk <- kassumptions$global_stocks
kr_bond <- kassumptions$global_bonds
kr_infl <- kassumptions$inflation
kr_incr <- 1
kr_cash <- kassumptions$cash
kexpected.returns  <- data.table(high = (90*kr_stk+10*kr_bond)/100, #0%-20% equities
                                 'medium-high' = (70*kr_stk+30*kr_bond)/100, #20%-40% equities 
                                medium = (50*kr_stk+50*kr_bond)/100, #40%-60% equities
                                'low-medium' = (30*kr_stk+70*kr_bond)/100, #60%-80% equities,
                                low = (10*kr_stk+90*kr_bond)/100) #80%-100% equities 
kreal.expected.returns <- kexpected.returns[, lapply(.SD, function(col) col - kr_infl), 
                                          .SDcols = colnames(kexpected.returns)]

#### General Functions #####

#Change a specific data table column of numbers to be formatted with a thousand separator
ColNumThousandFormat <- function(dt, col){
  dt[[col]] <- format(dt[[col]], big.mark = ",")
  return(dt)
}


# Run some code and time it; print a message along with the time
time.this <- function(code, message=NULL, progress.bar=FALSE, disable=NULL, notification.msg=NULL){
  require(crayon)
  if(!is.null(disable)) shiny.disable_all(disable)
  cat(message) # Write the label to the R console

  tm <- system.time({  # Show a progress bar showing the task has started
    if(progress.bar)  withProgress(try({ val <- eval.parent(code, n=2) }), message=message)
    else  try({ val <- eval.parent(code, n=2) })
  }, gcFirst=FALSE)

  tms <- tm['elapsed'][[1]]
  tm_string <- format( tms, digits=3 )

  if(!is.null(message)){  # Write the time taken to the R console
    if(tms<0.1){ fmt <- crayon::silver
    } else if(tms>0.5){ fmt <- crayon::bold
    } else { fmt <- crayon::reset }

        cat("", fmt(glue("({tm_string}s)\n\n")))
        }

  if(!is.null(notification.msg)){  # Show a shiny notification when finished
    showNotification(paste0(notification.msg, " in ", tm_string, " seconds."),
                     closeButton=FALSE, type='message')
  }

  if(!is.null(disable)) shiny.enable_all(disable)
  if(exists("val")){ # Return the output if it exists
    return(val)
  }
}

# Popify an element with a delay (in ms)

# Workaround from: https://stackoverflow.com/questions/47477237/delaying-and-expiring-a-shinybsbstooltip

PopifyDelayed <- function(..., options=NULL, delay=1000){
  delayopt <- list('xx')
  names(delayopt) <- glue("delay': {'show':<delay>, 'hide':0}, 'xx", .open='<', .close='>')
  popify(..., options = c(options, delayopt))
}

ShowHideElement <- function(inputID, hide.flag, anim = TRUE){
  fn <- if(hide.flag) hideElement else showElement
  do.call(fn, list(inputID, anim=anim))
}

ShowHideElements <- function(inputIDs, hide.flag, anim = T){
  lapply(inputIDs, ShowHideElement, hide.flag, anim)
}

ShowHideTabs <- function(inputID, tabs.to.hide, hide.flag){
  req(length(hide.flag)>0)
  fn <- if(hide.flag) hideTab else showTab
  mapply(fn,target = tabs.to.hide, MoreArgs = list(inputID=inputID))
}


#### Premium Bond Functions ####


CreateAllBonds <- function(first.bond, second.bond, owner){
  #Get only the numbers
  end.first <- substr(first.bond, 6, 11)
  end.second <- substr(second.bond, 6, 11)
  start.first <- substr(first.bond, 1, 5)
  interpolate.bonds <- seq.int(from = end.first, to = end.second)
  full.bond.names <- data.table(bond = paste0(start.first, interpolate.bonds),
                                owner = owner)
  
}


MakeBondNumbers <- function(bonds){
  all.bonds <- rbindlist(Map(CreateAllBonds,
                             first.bond = bonds$from, 
                             second.bond = bonds$to, 
                             owner = bonds$owner))
}

CheckPrizeFileExists <- function(newFile) {
  out <- tryCatch(read.xlsx(newFile, startRow = 3), 
                  error = function(e)
                    NULL)
  return(out)
}

GetPrizeData <- function (newFile){
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
  
  return(prem.bond.prize.data)
  
}


MakeMatchTable <- function(output, prem.bonds.prize.data, all.bonds){
  output$successful.message <- renderText(("There is a match in the high value winners this month!"))
  
  table <- data.table(prem.bonds.prize.data)
  match.table <- table[, match := lapply(`Bond Number`, function(x) x %in% all.bonds$bonds)]
  filter.match.table <-  match.table[`match` == "TRUE"][, `match` := NULL]
  
  setkey(filter.match.table, `Bond Number`)
  setkey(all.bonds, `bonds`)
  out <- filter.match.table[all.bonds]
}

#### Share Price Tracker ####


GetAllTickers <- function(ftse100tickers, sp500tickers){
  sp500tickers <- sp500tickers$Tickers
  sp500tickers <- gsub("[.]B",'-B',sp500tickers)
  alltickers <- c(ftse100tickers, sp500tickers)
  
}


LoadStockData <- function(rv, session){
  time.this({
    rv$prices <- tq_get(rv$alltickers, 
                        get  = "stock.prices",
                        from = today()-months(60),
                        to   = today(),
                        complete_cases = F) %>%
      select(symbol,date, close)
    
    rv$bench <- tq_get(kbenchmarks,
                       get  = "stock.prices",
                       from = today()-months(60),
                       to   = today()) %>%
      select(symbol, date, close)
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

Shares_Calculate_Data_For_Graphs <- function(rv, input){
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
  
  #For adding benchmark to data
  bench2 <- data.table(rv$bench)
  
  if (input$benchmark == 1) {
    bench <- bench2[symbol=="^GSPC" & date > min(prices$date)]
    prices <- rbind(prices,bench) }
  
  if (input$benchmark == 2) {
    bench <- bench2[symbol=="^FTSE" & date > min(prices$date)]
    prices <- rbind(prices,bench) }
  
  return(prices)
}


SharePriceGraph1 <- function(prices){
  ggplotly(prices %>%
             ggplot(aes(date, close,colour = symbol)) +
             geom_line(linewidth = 1, alpha = .9) +
             # uncomment the line below to show area under curves
             #geom_area(aes(fill=symbol),position="identity",alpha=.2) +
             theme_minimal(base_size=16) +
             theme(axis.title=element_blank(),
                   plot.background = element_rect(fill = "black"),
                   panel.background = element_rect(fill="black"),
                   panel.grid = element_blank(),
                   legend.text = element_text(colour="white"))
  )
}

SharePriceGraph2 <-function(prices){
  ggplotly(prices %>%
             group_by(symbol) %>%
             mutate(init_close = if_else(date == min(date),close,NA_real_)) %>%
             mutate(value = round(100 * close / sum(init_close,na.rm=T),1)) %>%
             ungroup() %>%
             ggplot(aes(date, value,colour = symbol)) +
             geom_line(linewidth = 1, alpha = .9) +
             # uncomment the line below to show area under curves
             #geom_area(aes(fill=symbol),position="identity",alpha=.2) +
             theme_minimal(base_size=16) +
             theme(axis.title=element_blank(),
                   plot.background = element_rect(fill = "black"),
                   panel.background = element_rect(fill="black"),
                   panel.grid = element_blank(),
                   legend.text = element_text(colour="white"))
  )
}

SharePriceTable <- function(prices){
  current.share.price <-
    if( nrow(prices[date == today()]) == 0){
      prices[date == today()-1]
    } else{
      prices[date == today()]}
  
  current.share.price[ , close := format(round(current.share.price$close, 2), big.mark = ",")]
  setnames(current.share.price, "symbol", "company")
  
  return(current.share.price)
  
}

#### Retirement Forecast ####

#Updating the UI based on input config
RetirementSettingUI <- function(id, type, session, value){
  if(type == 'numericInput')
  {updateNumericInput(session = session, inputId = id, value = value)}
  if(type == 'numericInputIcon')
  {updateNumericInputIcon(session = session, inputId = id, value = value)}  
  if(type == 'checkboxInput')
  {updateCheckboxInput(session = session, inputId = id, value = value)}
  if(type == 'radioGroupButtons')
  {updateRadioGroupButtons(session = session, inputId = id, selected = value)}
}


Life_Expectancy <- function(age, gender, life_exp, life_exp2){
current.year <- as.numeric(format(Sys.Date(), "%Y"))
birth.year <- current.year - age
birth.year.check <- if(birth.year < min(as.numeric(colnames(life_exp[2:ncol(life_exp)])))){
  min(as.numeric(colnames(life_exp[2:ncol(life_exp)])))
} else {
  birth.year
}

life.exp <- life_exp2[variable == birth.year.check, ][, years.to.exp := round({value - age},0)]
life.exp[, years.to.max := 120 - age][, value := round(value,0)]
out <- life.exp[ sex == gender]

}


CompoundRateList <- function(list.values, rate, rows){
  for(i in 1:rows){  
    #list.values <- c(list.values, tail(list.values, 1)* cumprod(1+rate))
    #list.values <- c(list.values, tail(list.values, 1)* (1+rate)^i)
    list.values <- c(list.values, tail(list.values, 1)*(1+rate))
  }
  list.values
}

CompoundRateListInfl <- function(list.values, rate, rows){
  for(i in 1:rows){  
    list.values <- c(list.values, (1+rate)^i)
  }
  list.values
}

PCont <- function(age, retirement.age, total.pens.con, new.pen.rate){
  if(age <= retirement.age){
    total.pens.con * new.pen.rate
  } else {
    0
  }
}

PensionTotalList <- function(list.values, rate, new.contr, rows){
  for(i in 1:rows){  
   list.values <- c(list.values, tail(list.values, 1)*rate + new.contr[i+1] )
  }
  list.values
}


