population
================
Erfan Faridmoayer
2022-12-07

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

### Cleaning Population Data

``` r
pop_df = 
read.csv("./total pop.csv")

pop_tidy = 
  pop_df[-1, ] %>% 
  janitor::clean_names() %>% 
  rename(population = p001001) %>% 
  select(geo_id, population) %>% 
  mutate(zip_code = gsub(".*US", '', geo_id))
```
