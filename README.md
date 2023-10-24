
# Introduction

The Chinese ID number contains a lot of information, this package helps you get the date of birth, age, age based on year, gender, region, zodiac, constellation information from the Chinese ID number.

## Installation

``` r
# Install development version from GitLab
remotes::install_gitlab("chuxinyuan/cnid")
# Install from CRAN
install.packages("CNID")
```

## Usage

Get full information from ID number.

``` r
id = c(
  "652801197305161555", 
  "130206202202291545",
  "110101841125178"
)
cnid_info(id)
```

Check the ID number for logical errors.

``` r
check_id(id)
```

Get date of birth, age, gender, etc.

``` r
birth_year(id)
birth_month(id)
birth_day(id)
birth_date(id)
age(id)
age_by_year(id)
gender(id)
region(id)
zodiac(id)
cstl(id)
```

Calculate the number of days in a specified year and month, the default is 
current year. such as february 2022.

``` r
mdays(2, 2022)
```

Calculate the number of days in a specified year, the default is current year. 
such as 2022.

``` r
ydays(2022)
```

## License

CNID is free and open source software, licensed under MIT + file LICENSE.
