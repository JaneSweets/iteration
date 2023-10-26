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
