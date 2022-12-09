Vaccine_data
================
2022-12-09

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
  janitor::clean_names()
```

``` r
merged_vaccine_data = merge(vaccine_data_tidy,zipcode_data, by = "modzcta", all.x = TRUE)
```
