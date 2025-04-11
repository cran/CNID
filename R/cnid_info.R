#' @title Get full information from ID number
#'
#' @description
#' Get full information from ID number.
#'
#' @param id A vector of ID numbers.
#'
#' @return A data frame about date of birth, age, gender, etc.
#' obtained from ID number.
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
  weight = c(7, 9, 10, 5, 8, 4, 2, 1, 6, 3, 7, 9, 10, 5, 8, 4, 2)
  check_code = c("1", "0", "X", "9", "8", "7", "6", "5", "4", "3", "2")
  
  # Chinese zodiac
  zodiacs = c(
    "\u9f20", "\u725b", "\u864e", "\u5154", "\u9f99", "\u86c7",
    "\u9a6c", "\u7f8a", "\u7334", "\u9e21", "\u72d7", "\u732a"
  )
  
  # Lunar solar table
  calendar_data = data.frame(
    date = lunar_solar_table$solar,
    lunar_date = lunar_solar_table$lunar
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
      id = x,
      id_type = NA,
      check = "",
      note = "",
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
        region_code = substr(x, 1, 6)
        birth_date_code = paste0("19", substr(x, 7, 12))
        gender_code = as.numeric(substr(x, 15, 15))
      } else {
        res$id_type = "18\u4f4d\u8eab\u4efd\u8bc1"
        region_code = substr(x, 1, 6)
        birth_date_code = substr(x, 7, 14)
        gender_code = as.numeric(substr(x, 17, 17))
        
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
      
      # Region processing
      region_match = region_data[region_data$code == region_code, ]
      res$region = ifelse(
        nrow(region_match) > 0, 
        region_match$region[1], 
        "\u672a\u77e5\u5730\u533a"
      )
      
      # Date of birth related processing
      res$birth_date = as.Date(birth_date_code, "%Y%m%d")
      if (is.na(res$birth_date)) {
        res$note = paste0(
          res$note, "[", "\u51fa\u751f\u65e5\u671f\u767b\u8bb0\u9519\u8bef", "]"
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
        y = as.integer(format(birth, "%Y"))
        if (y >= 1800 & y <= 2200) {
          calendar_data$date = as.Date(gsub("-", "", calendar_data$date), "%Y%m%d")
          calendar_data$year = format(calendar_data$date, "%Y")
          sfdate = calendar_data$date[format(birth, "%Y") == calendar_data$year]
          index = (as.integer(format(birth, "%Y")) - 1900) %% 12 + 1 - 
            (format(birth, "%m%d") < format(sfdate, "%m%d"))
          res$zodiac = zodiacs[index]
        }

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
      
      # ID number Content verification
      if (is.na(res$birth_date) & res$check != "\u4e0d\u901a\u8fc7") {
        stop(paste0(
          "The ID number ", "'", x, "'", 
          " is invalid. Please check it's date of birth."
        ))
      }
      
      if (!is.na(res$birth_date) & res$check == "\u4e0d\u901a\u8fc7") {
        stop(paste0(
          "The ID number ", "'", x, "'", 
          " is invalid. Please check it's 18th digit check code."
        ))
      }
      
      if (is.na(res$birth_date) & res$check == "\u4e0d\u901a\u8fc7") {
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
