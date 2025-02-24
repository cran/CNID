#' @title Get full information from ID number
#'
#' @description
#' Get full information from ID number.
#'
#' @param id A vector of ID numbers.
#'
#' @return A list about date of birth, age, gender, etc.
#' obtained from ID number.
#'
#' @examples
#' id = c(
#' "652801197305161555",
#' "130206202202291545", 
#' "110101841125178"
#' )
#' cnid_info(id)
#'
#' @export

#------------------------------------------------------------------------------#

cnid_info = function(id) {
  
  # Administrative division code data
  region_data = data.frame(
    code = region_code_base$code,
    region = region_code_base$region
  )
  
  # Check code weight and corresponding table
  weight = c(7,9,10,5,8,4,2,1,6,3,7,9,10,5,8,4,2)
  check_code = c("1","0","X","9","8","7","6","5","4","3","2")
  
  # Chinese zodiac
  zodiacs = c(
    "\u9f20", "\u725b", "\u864e", "\u5154", "\u9f99", "\u86c7",
    "\u9a6c", "\u7f8a", "\u7334", "\u9e21", "\u72d7", "\u732a"
  )
  
  # constellation
  cstl_data = data.frame(
    month = 1:12,
    cut_day = c(20, 19, 21, 20, 21, 21, 23, 23, 23, 23, 22, 22),
    cstls = c(
      "\u6c34\u74f6\u5ea7", "\u53cc\u9c7c\u5ea7", 
      "\u767d\u7f8a\u5ea7", "\u91d1\u725b\u5ea7",
      "\u53cc\u5b50\u5ea7", "\u5de8\u87f9\u5ea7", 
      "\u72ee\u5b50\u5ea7", "\u5904\u5973\u5ea7",
      "\u5929\u79e4\u5ea7", "\u5929\u874e\u5ea7", 
      "\u5c04\u624b\u5ea7", "\u6469\u7faf\u5ea7"
    )
  )
  
  process_id = function(x) {
    # Initializes the result list
    res = list(
      id_type = NA,
      check = NA,
      warning = NA,
      valid = FALSE,
      region = NA,
      gender = NA,
      birth_date = NA,
      age = NA,
      age_by_year = NA,
      zodiac = NA,
      cstl = NA
    )
    
    tryCatch({
      # Foundation form verification
      if (!nchar(x) %in% c(15, 18)) {
        stop("Invalid ID length.")
      }
      
      if (!grepl("^\\d{15}(\\d{2}[0-9X])?$", x)) {
        stop("Invalid ID character.")
      }
      
      # Convert to uppercase letters
      x = toupper(x)
      
      # Extract basic information
      if (nchar(x) == 15) {
        res$id_type = "15\u4f4d\u8eab\u4efd\u8bc1"
        region_code = substr(x,1,6)
        res$birth_date = as.Date(paste0("19", substr(x,7,12)), "%Y%m%d")
        gender_code = as.numeric(substr(x,15,15))
      } else {
        res$id_type = "18\u4f4d\u8eab\u4efd\u8bc1"
        region_code = substr(x,1,6)        
        res$birth_date = as.Date(substr(x,7,14), "%Y%m%d")
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
          res$warning = "\u6821\u9a8c\u7801\u6821\u9a8c\u5931\u8d25"
        }
      }
      
      # Check date        
      if (is.na(res$birth_date)) {
        res$warning = "\u51fa\u751f\u65e5\u671f\u767b\u8bb0\u9519\u8bef"
      }
      
      # Region processing
      region_match = region_data[region_data$code == region_code, ]
      res$region = ifelse(
        nrow(region_match) > 0, 
        region_match$region[1], 
        "\u672a\u77e5\u5730\u533a"
      )
      
      # Gender processing
      res$gender = ifelse(gender_code %% 2 == 1, "\u7537", "\u5973")
      
      # Validate ID
      if (nchar(x) == 15) {
        res$valid = ifelse(!is.na(res$birth_date), TRUE, FALSE)        
      } else {
        res$valid = ifelse(
          res$check == "\u901a\u8fc7" & !is.na(res$birth_date), TRUE, FALSE
        )
      }
      
      # Date of birth related processing
      if (!is.na(res$birth_date)) {
        # Age processing
        today = Sys.Date()
        birth = res$birth_date
        res$age = as.integer(format(today, "%Y")) - as.integer(format(birth, "%Y"))
        if (format(today, "%m%d") < format(birth, "%m%d")) res$age = res$age - 1
  
        # Age by year processing
        res$age_by_year = as.integer(format(today, "%Y")) - 
          as.integer(format(birth, "%Y"))
        
        # Chinese zodiac processing
        res$zodiac = zodiacs[(as.integer(format(birth, "%Y")) - 1900) %% 12 + 1]
        
        # constellation
        m = as.integer(format(birth, "%m"))
        d = as.integer(format(birth, "%d"))
        
        if (d >= cstl_data$cut_day[m]) {
          res$cstl = cstl_data$cstls[m]
        } else {
          previous_month = ifelse(m == 1, 12, m - 1)
          res$cstl = cstl_data$cstls[previous_month]
        }
      }
      
    }, error = function(e) {
      message(paste("ERROR:", e$message))
    })
    
    return(res)
  }
  
  # Process all ID
  result = lapply(id, process_id)
  
  # Convert to a data frame
  data.frame(
    id = id,
    id_type = sapply(result, function(x) x$id_type),
    check = sapply(result, function(x) x$check),
    warning = sapply(result, function(x) x$warning),
    valid = sapply(result, function(x) x$valid),
    region = sapply(result, function(x) x$region),
    gender = sapply(result, function(x) x$gender),
    birth_date = as.Date(sapply(result, function(x) x$birth_date), origin = "1970-01-01"),
    age = sapply(result, function(x) x$age),
    age_by_year = sapply(result, function(x) x$age_by_year),
    zodiac = sapply(result, function(x) x$zodiac),
    cstl = sapply(result, function(x) x$cstl),    
    stringsAsFactors = FALSE
  )
}

#------------------------------------------------------------------------------#
