#' @title Get zodiac information from ID number
#'
#' @description
#' Get zodiac information from ID number.
#'
#' @param id A vector of ID numbers.
#'
#' @return Zodiac vector obtained from ID numbers.
#'
#' @examples
#' id = c(
#' "652801197305161555",
#' "130206202202291545", 
#' "110101841125178"
#' )
#' zodiac(id)
#'
#' @export

#------------------------------------------------------------------------------#

zodiac = function(id) {
  cnid_info(id)$zodiac
}

#------------------------------------------------------------------------------#