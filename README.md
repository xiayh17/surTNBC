
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surTNBC

<!-- badges: start -->
<!-- badges: end -->

The goal of surTNBC is to do survival analysis from a deg database

## Installation

You can install the released version of surTNBC from
[github](https://github.com/xiayh17/surTNBC) or [gitee](https://gitee.com/xiayh17/surTNBC) with:

``` r
devtools::install_github("xiayh17/surTNBC")
devtools::install_git("https://gitee.com/xiayh17/surTNBC")
```

## download data

[click to
download](https://yonghexia-my.sharepoint.com/:t:/g/personal/xiayh_yonghexia_onmicrosoft_com/Ef6PWaUOVFRDrEJ04nsXmfkByKL0DY1h8jBnjd2UyOiuig?e=4Vsmkx)

## data prepare

``` r
library(DBI)
library(dplyr)
library(dbplyr)
iso_tpm2 <- data.table::fread("iso_tpm.txt")
iso_tpm2 <- iso_tpm2 %>% dplyr::rename(names = V1)
con <- DBI::dbConnect(RSQLite::SQLite(), "portal-database-output.sqlite")
dplyr::copy_to(con, iso_tpm2, "iso_tpm2",
               overwrite = TRUE,
               temporary = FALSE, 
               indexes = list(
                 c("names")
               )
)
# call dbDisconnect() when finished working with a connection 
DBI::dbDisconnect(con)
```
