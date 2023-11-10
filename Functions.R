
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