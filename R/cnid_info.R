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
  
  # Current date
  current_year = as.integer(format(Sys.Date(), "%Y"))
  current_month = as.integer(format(Sys.Date(), "%m"))
  current_day = as.integer(format(Sys.Date(), "%d"))
  
  # Extract the basic information of birth, gender and region
  birth_year = character(length(id))
  birth_month = character(length(id))
  birth_day = character(length(id))
  gender_code = character(length(id))
  region_code = character(length(id))
  
  for (i in 1:length(id)) {
    if (nchar(id[i]) == 15) {
      # 15 digit ID number
      birth_year[i] = paste0("19", substr(id[i], 7, 8))
      birth_month[i] = substr(id[i], 9, 10)
      birth_day[i] = substr(id[i], 11, 12)
      gender_code[i] = as.integer(substr(id[i], 15, 15))
      region_code[i] = as.integer(substr(id[i], 1, 6))
    } else if (
        nchar(id[i]) == 18 & 
        (substr(id[i], 18, 18) %in% as.character(c(0:9, "X")))
      ) {
      # 18 digit ID number
      birth_year[i] = substr(id[i], 7, 10)
      birth_month[i] = substr(id[i], 11, 12)
      birth_day[i] = substr(id[i], 13, 14)
      gender_code[i] = as.integer(substr(id[i], 17, 17))
      region_code[i] = as.integer(substr(id[i], 1, 6))
    } else {
      birth_year[i] = NA
      birth_month[i] = NA
      birth_day[i] = NA
      gender_code[i] = NA
      region_code[i] = NA
    }
  }
  
  for (i in 1:length(birth_year)) {
    y = birth_year[i]
    if (grepl("^\\d+$", y) == TRUE) {
      if (as.integer(y) > current_year) {
        birth_year[i] = NA
      } else {
        birth_year[i] = birth_year[i]
      }
    } else {
      birth_year[i] = NA
    }
  }
  birth_year = as.integer(birth_year)
  
  for (i in 1:length(birth_month)) {
    m = birth_month[i]
    if (grepl("^\\d+$", m) == TRUE) {
      if (as.integer(m) < 1 | as.integer(m) > 12) {
        birth_month[i] = NA
      } else {
        birth_month[i] = birth_month[i]
      }
    } else {
      birth_month[i] = NA
    }
  } 
  birth_month = as.integer(birth_month)
  
  for (i in 1:length(birth_day)) {
    d = birth_day[i]
    if (grepl("^\\d+$", d) == TRUE) {
      if (as.integer(d) < 1 | as.integer(d) > 31) {
        birth_day[i] = NA
      } else {
        birth_day[i] = birth_day[i]
      }
    } else {
      birth_day[i] = NA
    }
  }  
  birth_day = as.integer(birth_day)

  gender_code = as.integer(gender_code)
  region_code = as.integer(region_code)
  
  # Parse the birth date
  birth_date = seq(
    as.Date("1900/1/1"), 
    by = "month", 
    length.out = length(id)
  )
  for (i in 1:length(id)) {
    y = birth_year[i]
    m = birth_month[i]
    d = birth_day[i]
    if (is.na(y) | is.na(m) | is.na(d)) {
      birth_date[i] = NA
    } else {
      if (d <= mdays(m, y)) {
        birth_date[i] = as.Date(
          paste(y, m, d, sep = "-")
        )
      } else {
        birth_date[i] = NA
      }
    }
  }
  
  # Accurate age
  age = integer(length(id))
  for (i in 1:length(id)) {
    y = birth_year[i]
    m = birth_month[i]
    d = birth_day[i]
    bd = birth_date[i]
    if (is.na(bd)) {
      age[i] = NA
    } else {
      if (
        current_month < m |
        (current_month == m & current_day < d)
      ) {
        age[i] = current_year - y - 1
      } else {
        age[i] = current_year - y
      }
    }
  }
  
  # Age by year
  age_by_year = integer(length(id))
  for (i in 1:length(id)) {
    y = birth_year[i]
    if (is.na(y)) {
      age_by_year[i] = NA
    } else {
      age_by_year[i] = current_year - y
    }
  }
  
  # Parse gender
  gender = ifelse(
    gender_code %% 2 == 0, "\u5973", "\u7537"
  )
  
  # Parse region
  region = character(length = length(id))
  for (i in 1:length(id)) {
    rc = region_code[i]
    if (is.na(rc)) {
      region[i] = NA
    } else {
      if (rc %in% region_code_base$code) {
        region[i] = region_code_base$region[
          region_code_base$code == rc
        ]
      } else {
        region[i] = NA
      }
    }
  }
  
  # Parse the Chinese zodiac
  zodiacs <- c(
    "\u9f20", "\u725b", "\u864e", "\u5154", "\u9f99", "\u86c7",
    "\u9a6c", "\u7f8a", "\u7334", "\u9e21", "\u72d7", "\u732a"
  )
  zodiac = character(length = length(id))
  for (i in 1:length(id)) {
    y = birth_year[i]
    if (is.na(y)) {
      zodiac[i] = NA
    } else {
      zodiac[i] = zodiacs[(y - 1900) %% 12 + 1]
    }
  }
  
  # Parse constellation
  get_cstl = function(birth_month, birth_day) {
    cstls = c(
      "\u6c34\u74f6\u5ea7", "\u53cc\u9c7c\u5ea7", 
      "\u767d\u7f8a\u5ea7", "\u91d1\u725b\u5ea7",
      "\u53cc\u5b50\u5ea7", "\u5de8\u87f9\u5ea7", 
      "\u72ee\u5b50\u5ea7", "\u5904\u5973\u5ea7",
      "\u5929\u79e4\u5ea7", "\u5929\u874e\u5ea7", 
      "\u5c04\u624b\u5ea7", "\u6469\u7faf\u5ea7"
    )
    xinzuo = character(length = length(birth_month))
    for (i in 1:length(birth_month)) {
      y = birth_year[i]
      m = birth_month[i]
      d = birth_day[i]
      bd = birth_date[i]
      if (is.na(m) | is.na(d)) {
        xinzuo[i] = NA
      } else if (
        is.na(y) == FALSE & is.na(m) == FALSE & 
        is.na(d) == FALSE & is.na(bd) == TRUE
      ) {
        xinzuo[i] = NA
      } else if ((m == 1 & d >= 20) | (m == 2 & d <= 18)) {
        xinzuo[i] = cstls[1]  # shuiping(0120-0218)
      } else if ((m == 2 & d >= 19) | (m == 3 & d <= 20)) {
        xinzuo[i] = cstls[2]  # shuangyu(0219-0320)
      } else if ((m == 3 & d >= 21) | (m == 4 & d <= 19)) {
        xinzuo[i] = cstls[3]  # baiyang(0321-0419)
      } else if ((m == 4 & d >= 20) | (m == 5 & d <= 20)) {
        xinzuo[i] = cstls[4]  # jinniu(0420-0520)
      } else if ((m == 5 & d >= 21) | (m == 6 & d <= 20)) {
        xinzuo[i] = cstls[5]  # shuangzi(0521-0620)
      } else if ((m == 6 & d >= 21) | (m == 7 & d <= 22)) {
        xinzuo[i] = cstls[6]  # juxie(0621-0722)
      } else if ((m == 7 & d >= 23) | (m == 8 & d <= 22)) {
        xinzuo[i] = cstls[7]  # shizi(0723-0822)
      } else if ((m == 8 & d >= 23) | (m == 9 & d <= 22)) {
        xinzuo[i] = cstls[8]  # chunv(0823-0922)
      } else if ((m == 9 & d >= 23) | (m == 10 & d <= 22)) {
        xinzuo[i] = cstls[9]  # tiancheng(0923-1022)
      } else if ((m == 10 & d >= 23) | (m == 11 & d <= 21)) {
        xinzuo[i] = cstls[10]  # tianxie(1023-1121)
      } else if ((m == 11 & d >= 22) | (m == 12 & d <= 21)) {
        xinzuo[i] = cstls[11]  # sheshou(1122-1221)
      } else if ((m == 12 & d >= 22) | (m == 1 & d <= 19)) {
        xinzuo[i] = cstls[12]  # mojie(1222-0119)
      } else {
        xinzuo[i] = NA
      }
    }
    return(xinzuo)
  }
  cstl = get_cstl(birth_month, birth_day)
  
  # Check the ID number for logical problems
  key = data.frame(
    region, birth_year, birth_month,
    birth_day, birth_date, gender
  )
  check_id = logical(length = length(id))
  for (i in 1:length(id)) {
    if (any(is.na(key[i, ]))) {
      check_id[i] = FALSE
    } else {
      check_id[i] = TRUE
    }
  }
  
  # Generate result list
  result = list(
    check_id = check_id,
    birth_year = birth_year,
    birth_month = birth_month,
    birth_day = birth_day,
    birth_date = birth_date,
    age = age,
    age_by_year = age_by_year,
    gender = gender,
    region = region,
    zodiac = zodiac,
    cstl = cstl
  )
  
  # prompt
  if (all(nchar(id) %in% c(15, 18)) == FALSE) {
    warning("There are cases where the ID number is not 15 or 18 digits.")
  }
  
  if (any(check_id == FALSE)) {
    warning("There are cases where the ID number has a logical error.")
  }
  
  # Return final result
  return(result)
  
}

#------------------------------------------------------------------------------#
