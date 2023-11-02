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


MakeBondNumbers <- function(bonds, file_path){
  all.bonds <- rbindlist(Map(CreateAllBonds,
                             first.bond = bonds$from, 
                             second.bond = bonds$to, 
                             owner = bonds$owner))
  
  write.csv(all.bonds, paste0(file_path, 'all_bonds.csv'))
  write.csv(bonds, paste0(file_path, 'last_time_bonds.csv'))
  
  return(all.bonds)
}

CheckPrizeFileExists <- function(newFile) {
  out <- tryCatch(read.xlsx(newFile, startRow = 3), 
                  error = function(e)
                    NULL)
  return(out)
}