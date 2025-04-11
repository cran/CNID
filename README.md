
# Introduction

The Chinese ID number contains a lot of information, this package helps you get the region, date of birth, age, age based on year, gender, zodiac, constellation information from the Chinese ID number.

## Installation

``` r
# Install development version via GitHub
remotes::install_github("chuxinyuan/cnid")
# Install from CRAN
install.packages("CNID")
```

## Usage

Get full information from ID number.

``` r
id = c(
  "653127198503161793",
  "652801197305161555", 
  "130206202202291545",
  "110101841125178",
  "12345678",
  "65312a198204181793"
)
cnid_info(id)
```

Check whether the ID number is valid.

``` r
check_id(id)
```

Get date of birth, age, gender, etc.

``` r
birth_date(id)
age(id)
age_by_year(id)
gender(id)
region(id)
zodiac(id)
cstl(id)
```

Convert the 15 Digits ID Number to 18 Digits.

``` r
convert18(id)
```

Calculate whether a given year is a leap year. Return TRUE if it is a leap year, 
FALSE otherwise.

``` r
leap_year(2024)
```

Calculate the number of days in a given year and month, such as february 2022.

``` r
mdays(2022, 2)
```

Calculate the number of days in a given year, such as 2022.

``` r
ydays(2022)
```

## License

CNID is free and open source software, licensed under MIT + file LICENSE.
