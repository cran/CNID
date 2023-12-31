#' @title Calculate the number of days in a specified year
#'
#' @description
#' Calculate the number of days in a specified year.
#'
#' @param year A given year, the default is current year.
#'
#' @return Days in a specified year.
#'
#' @examples
#' ydays(2022)
#'
#' @export

#------------------------------------------------------------------------------#

ydays = function(
    year = as.integer(format(Sys.Date(), "%Y"))
) {
  
  if (is.na(as.numeric(year))) {
    stop("year must be numeric value")
  }

  year = as.integer(year)
  days_in_year = 365 + (
    (year %% 4 == 0 && year %% 100 != 0) || 
    (year %% 400 == 0)
  )
  
  return(days_in_year)
  
}

#------------------------------------------------------------------------------#

