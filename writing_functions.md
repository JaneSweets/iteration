Writing Functions
================
Shaolei Ma
2023-10-26

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
    ## 
    ## Attaching package: 'rvest'
    ## 
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

Set seed for reproducibility.

``` r
set.seed(12345)
```

## Z score function

Z scores substract the mean and divide by the sd.

``` r
x_vec = rnorm(20, mean = 5, sd = .3)
```

Compute Z scores for `x_vec`.

``` r
(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.6103734  0.7589907 -0.2228232 -0.6355576  0.6347861 -2.2717259
    ##  [7]  0.6638185 -0.4229355 -0.4324994 -1.1941438 -0.2311505  2.0874460
    ## [13]  0.3526784  0.5320552 -0.9917420  0.8878182 -1.1546150 -0.4893597
    ## [19]  1.2521303  0.2664557

Write a function to do this!

``` r
z_score = function(x){
  
  if(!is.numeric(x)) {
    stop("Argument should be numbers")
  } else if (length(x) < 2) {
    stop("You need at lease 2 numbers to get z scores")
  }
  
  z = (x - mean(x)) / sd(x)
  
  z
  
}
```

Check that this works.

``` r
z_score(x_vec)
```

    ##  [1]  0.6103734  0.7589907 -0.2228232 -0.6355576  0.6347861 -2.2717259
    ##  [7]  0.6638185 -0.4229355 -0.4324994 -1.1941438 -0.2311505  2.0874460
    ## [13]  0.3526784  0.5320552 -0.9917420  0.8878182 -1.1546150 -0.4893597
    ## [19]  1.2521303  0.2664557

``` r
z_score(x = rnorm(10, mean = 5))
```

    ##  [1]  0.5952213  1.1732833 -0.6221352 -1.3990896 -1.4371950  1.4719158
    ##  [7] -0.4830567  0.4590828  0.4520244 -0.2100512

Keep checking.

``` r
z_score(x = 3)
```

    ## Error in z_score(x = 3): You need at lease 2 numbers to get z scores

``` r
z_score(c("my", "name", "is", "jeff"))
```

    ## Error in z_score(c("my", "name", "is", "jeff")): Argument should be numbers

``` r
z_score(c(T, T, F, T))
```

    ## Error in z_score(c(T, T, F, T)): Argument should be numbers

``` r
z_score(iris)
```

    ## Error in z_score(iris): Argument should be numbers

## Multiple outputs.

Write a function that returns mean and standard deviation.

``` r
mean_and_sd = function(x) {
  
  if(!is.numeric(x)) {
    stop("Argument should be numbers")
  } else if (length(x) < 2) {
    stop("You need at lease 2 numbers to get means and sds")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
}
```

Double check I did this right ..

``` r
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.02 0.250

## Start getting means and sds

``` r
x_vec = rnorm(n = 30, mean = 5, sd = .5)

tibble(
  mean = mean(x_vec),
  sd = sd(x_vec)
)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.16 0.643

Let’s write a function that uses `n`, a true mean, and true SD as
inputs.

``` r
sim_mean_sd = function(n_obs, mu, sigma) {
  
  x_vec = rnorm(n = n_obs, mean = mu, sd = sigma)
  
  tibble(
    mean = mean(x_vec),
    sd = sd(x_vec)
  )
}

sim_mean_sd(n_obs = 30, mu = 5, sigma = .5)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.22 0.543

## LoTR data

``` r
import_LoTR = function(path = "data/LotR_Words.xlsx", movie_name, data_range) {
  
  movie_df =
    readxl::read_excel(path, range = data_range) |> 
    mutate(movie = movie_name) |> 
    janitor::clean_names() |> 
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words"
    ) |> 
    select(movie, everything())
  
  movie_df
  
}

lotr_tidy = import_LoTR(movie_name = "fellowship_ring", data_range = "B3:D6") |> 
  bind_rows(import_LoTR(movie_name = "two_towers", data_range = "F3:H6")) |> 
  bind_rows(import_LoTR(movie_name = "return_king", data_range = "J3:L6")) |> 
  mutate(race = str_to_lower(race))
```

## NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

nsduh_table <- function(html, table_num, table_name) {
  
  table = 
    html |> 
    html_table() |> 
    nth(table_num) |>
    slice(-1) |> 
    select(-contains("P Value")) |>
    pivot_longer(
      -State,
      names_to = "age_year", 
      values_to = "percent") |>
    separate(age_year, into = c("age", "year"), sep = "\\(") |>
    mutate(
      year = str_replace(year, "\\)", ""),
      percent = str_replace(percent, "[a-c]$", ""),
      percent = as.numeric(percent),
      name = table_name) |>
    filter(!(State %in% c("Total U.S.", "Northeast", "Midwest", "South", "West")))
}

nsduh_results = 
  bind_rows(
    nsduh_table(nsduh_html, 1, "marj_one_year"),
    nsduh_table(nsduh_html, 4, "cocaine_one_year"),
    nsduh_table(nsduh_html, 5, "heroin_one_year")
  )
```

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4
