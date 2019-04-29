#' Day of hydrologic year
#'
#' This function reads a date or vector of dates and returns the hydrologic year
#' (begining Oct 01) 
#' @param hDates date or date vector

hydro_year <- function(hDates = NULL) {

  # library('')

  # Check to see if dates vector is a date, don't coerce, but raise exception
  if (!lubridate::is.Date(hDates) & !lubridate::is.POSIXct(hDates)) {
    
    stop('Please convert the dates using as.Date() or as.POSIXct()')
    
  }

  hYear <- ifelse(lubridate::month(hDates) >= 10, lubridate::year(hDates) + 1,
                  lubridate::year(hDates))

  return(hYear)
  
}

