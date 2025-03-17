#' @title Convert the 15 Digits ID Number to 18 Digits
#'
#' @description
#' Convert the 15 Digits ID Number to 18 Digits.
#'
#' @param id A vector of ID numbers.
#'
#' @return The corresponding 18 digits ID number.
#'
#' @examples
#' id = c(
#'   "653127198503161793",
#'   "652801197305161555",
#'   "130206202202291545", 
#'   "110101841125178",
#'   "12345678",
#'   "65312a198204181793"
#' )
#' convert18(id)
#'
#' @export

#------------------------------------------------------------------------------#

convert18 = function(id) {
  
  # Check code weight and corresponding table
  weight = c(7, 9, 10, 5, 8, 4, 2, 1, 6, 3, 7, 9, 10, 5, 8, 4, 2)
  check_code = c("1", "0", "X", "9", "8", "7", "6", "5", "4", "3", "2")
  
  process_id = function(x) {
  # Initializes the result list
    res = list(
      id = x,
      id_type = NA,
      check = "",
      note = "",
      valid = FALSE,
      id18 = NA
  )
  
    tryCatch({
      # ID number foundation form verification
      if (!nchar(x) %in% c(15, 18)) {
        res$note = paste0(
          res$note, "[", "\u4e0d\u6ee1\u8db315\u621618\u4f4d\u8981\u6c42", "]"
        )
        stop(paste0(
          "The ID number ", "'", x, "'", 
          " is invalid. It must contain either 15 or 18 digits."
        ))
      }
      
      if (!grepl("^\\d{15}(\\d{2}[0-9X])?$", x)) {
        res$note = paste0(
          res$note, "[", "\u5305\u542b\u4e0d\u5408\u89c4\u7684\u5b57\u7b26", "]"
        )        
        stop(paste0(
          "The ID number ", "'", x, "'", 
          " is invalid. It must be 15 pure digits, or 18 characters where",
          " the first 17 are digits and the last is either a digit or 'X'."
        ))
      }
      
      # Extract basic information
      if (nchar(x) == 15) {
        res$id_type = "15\u4f4d\u8eab\u4efd\u8bc1"
        region_code = substr(x,1,6)
        birth_date_code = paste0("19", substr(x,7,12))
        police_station_code = substr(x,13,14)
        gender_code = as.numeric(substr(x,15,15))
      } else {
        res$id_type = "18\u4f4d\u8eab\u4efd\u8bc1"
        region_code = substr(x,1,6)
        birth_date_code = substr(x,7,14)
        police_station_code = substr(x,15,16)
        gender_code = as.numeric(substr(x,17,17))
        
        # Check code verification
        chars = strsplit(x, "")[[1]]
        sum_val = sum(weight * as.numeric(chars[1:17]))
        calc_check = check_code[sum_val %% 11 + 1]
        
        res$check = ifelse(
          chars[18] == calc_check, 
          "\u901a\u8fc7", 
          "\u4e0d\u901a\u8fc7"
        )
        
        if (res$check == "\u4e0d\u901a\u8fc7") {
          res$note = paste0(
            res$note, "[", "\u6821\u9a8c\u7801\u6821\u9a8c\u5931\u8d25", "]"
          )
        }
      }
      
      # Date of birth processing
      birth_date = as.Date(birth_date_code, "%Y%m%d")
      if (is.na(birth_date)) {
        res$note = paste0(
          res$note, "[", "\u51fa\u751f\u65e5\u671f\u767b\u8bb0\u9519\u8bef", "]"
        )
      }
      
      # Validate ID
      if (nchar(x) == 15) {
        res$valid = ifelse(!is.na(birth_date), TRUE, FALSE)        
      } else {
        res$valid = ifelse(
          res$check == "\u901a\u8fc7" & !is.na(birth_date), TRUE, FALSE
        )
      }
      
      # Convert 15 digits to 18 digits
      if (nchar(x) == 18) res$id18 = x
      else {
        if (is.na(birth_date)) res$id18 = NA
        else {
          chars = strsplit(x, "")[[1]]
          sum_val = sum(weight * as.numeric(c(chars[1:6], 1, 9, chars[7:15])))
          res$id18 = paste0(
            region_code, 
            birth_date_code, 
            police_station_code,
            gender_code,
            check_code[sum_val %% 11 + 1]
          )
        }
      }
      
      # ID number Content verification
      if (is.na(birth_date) & res$check != "\u4e0d\u901a\u8fc7") {
        stop(paste0(
          "The ID number ", "'", x, "'", 
          " is invalid. Please check it's date of birth."
        ))
      }
      
      if (!is.na(birth_date) & res$check == "\u4e0d\u901a\u8fc7") {
        stop(paste0(
          "The ID number ", "'", x, "'", 
          " is invalid. Please check it's 18th digit check code."
        ))
      }
      
      if (is.na(birth_date) & res$check == "\u4e0d\u901a\u8fc7") {
        stop(paste0(
          "The ID number ", "'", x, "'", 
          " is invalid. Please check it's 18th digit check code and date of birth."
        ))
      }
      
    }, error = function(e) {
      message(paste0("WARNING:", e$message))
    })
    
    return(res)
  }
  
  # Process all ID
  result = lapply(id, process_id)
  
  # Convert to a data frame
  do.call(rbind, lapply(result, as.data.frame))

}
#------------------------------------------------------------------------------#