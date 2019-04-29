#' Day of hydrologic year
#'
#' This function reads a date or vector of dates and returns the day of the
#' hydrologic year (begining Oct 01) 
#' @param hDates date or date vector
#' day_of_hydro_year()

day_of_hydro_year <- function(hDates = NULL) {
  
  # Check to see if dates vector is a date, don't coerce, but raise exception
  if (!lubridate::is.Date(hDates) & !lubridate::is.POSIXct(hDates)) {
    
    stop('Please convert the dates using as.Date()')
    
  }
  
  pYear <- ifelse(lubridate::month(hDates) >= 10, lubridate::year(hDates) + 1,
                  lubridate::year(hDates)) - 1
  
  # Check data type -- IF is.DATE
  if (lubridate::is.Date(hDates)) {
    
    fdohy <- as.Date(paste0(pYear, '-10-01'), '%Y-%m-%d')
    
    dohy <- as.numeric(hDates) - as.numeric(fdohy) + 1
    
  }

  # Check data type -- IF is.POSIXct
  if (lubridate::is.POSIXct(hDates)) {
    
    fdohy <- as.POSIXct(paste0(pYear, '-10-01'), '%Y-%m-%d',
                        tz = 'America/Los_Angeles')

    dohy <- (as.numeric(hDates) - as.numeric(fdohy)) / 86400
  
  }

  return(dohy)
  
}
