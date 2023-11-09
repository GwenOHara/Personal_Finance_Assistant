#### Functions #####

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

#Change a specific data table column of numbers to be formatted with a thousand separator
ColNumThousandFormat <- function(dt, col){
  dt[[col]] <- format(dt[[col]], big.mark = ",")
  return(dt)
}