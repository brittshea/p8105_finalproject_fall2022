Vaccine_data
================
2022-12-09

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.0      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(readxl)

knitr::opts_chunk$set(
  fig.width = 6, 
  fig.asp = 0.6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.color = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_color_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

``` r
#Loading vaccine data 
vaccine_data_tidy = read_csv(file = "./data/coverage-by-modzcta-adults_03.09.22.csv")%>%
  janitor::clean_names()%>%
  mutate(modzcta = as.character(modzcta),count_partially_prop = round(count_partially_cumulative/pop_denominator,2), count_Fully_prop = round(count_fully_cumulative/pop_denominator,2), count_1Plus_prop = round(count_1plus_cumulative/pop_denominator,2), count_Additional_prop = round(count_additional_cumulative/pop_denominator,2))
```

    ## Rows: 177 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (3): NEIGHBORHOOD_NAME, BOROUGH, Label
    ## dbl (6): MODZCTA, POP_DENOMINATOR, COUNT_PARTIALLY_CUMULATIVE, COUNT_FULLY_C...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
vaccine_data_tidy$modzcta <- gsub('\\s+', '', vaccine_data_tidy$modzcta)

#Loading zipcode data 
zipcode_data <- read.csv("data/Modified_Zip_Code_Tabulation_Areas__MODZCTA_.csv") %>% 
  janitor::clean_names() %>% 
  select(modzcta, label,zcta,pop_est,the_geom)
```

``` r
merged_vaccine_data = merge(vaccine_data_tidy,zipcode_data, by = "modzcta", all.x = TRUE)

write.csv(merged_vaccine_data, "./data/merged/merged_vaccine_data.csv", row.names=FALSE)
```
