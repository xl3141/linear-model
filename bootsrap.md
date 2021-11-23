bootstrapping
================
Xinyuan Liu
11/23/2021

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)

set.seed(1)
```

``` r
n_samp = 250

sim_df_const = 
  tibble(
    x = rnorm(n_samp, 1, 1),
    error = rnorm(n_samp, 0, 1),
    y = 2 + 3 * x + error
  )

sim_df_nonconst = sim_df_const %>% 
  mutate(
  error = error * .75 * x,
  y = 2 + 3 * x + error
)
```

``` r
sim_df_const %>% 
  lm(y ~ x, data = .) %>% 
  broom::tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.98    0.0981      20.2 3.65e- 54
    ## 2 x               3.04    0.0699      43.5 3.84e-118

## Let’s try to use the bootstapping for inference

``` r
bootstrap_sample = 
  sim_df_nonconst %>% 
    sample_frac(size = 1, replace = TRUE) %>% 
    arrange(x)

lm(y ~ x, data = bootstrap_sample)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x, data = bootstrap_sample)
    ## 
    ## Coefficients:
    ## (Intercept)            x  
    ##       1.897        3.196

Let’s write a function

``` r
boot_sample = function(df){
  
  sample_frac(df, size = 1, replace = TRUE)
  
}
```

NOw make a tibble to keep track of everything

``` r
boot_strap_df = 
  tibble(
    strap_number = 1:1000,
    strap_sample = rerun(1000, boot_sample(sim_df_nonconst))
  )
```

From here… things are kind the same as always

``` r
boot_strap_result=
  boot_strap_df %>% 
    mutate(
      models = map(.x = strap_sample, ~lm(y ~ x, data = .x)),
      results = map(models, broom::tidy)
    ) %>% 
    select(strap_number, results) %>% 
    unnest(results)

boot_strap_result %>% 
  ggplot(aes(x = estimate)) + 
  geom_histogram()+ 
  facet_grid(. ~ term, scales = "free")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](bootsrap_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
lm(y ~ x, data = sim_df_nonconst) %>% 
  broom::tidy()
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)     1.93    0.105       18.5 1.88e- 48
    ## 2 x               3.11    0.0747      41.7 5.76e-114

``` r
boot_strap_result %>% 
  group_by(term) %>% 
  summarize(
    se = sd(estimate)
  )
```

    ## # A tibble: 2 x 2
    ##   term            se
    ##   <chr>        <dbl>
    ## 1 (Intercept) 0.0747
    ## 2 x           0.101

## Use ‘modelr’

``` r
boot_straps = 
  sim_df_nonconst %>% 
  modelr::bootstrap(n = 1000, id = "strap_number")
```

## air\_bnb

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(stars = review_scores_location / 2) %>% 
  rename(
    boro = neighbourhood_group,
    neighborhood = neighbourhood) %>% 
  filter(boro != "Staten Island") %>% 
  select(price, stars, boro, neighborhood, room_type)

nyc_airbnb %>% 
  ggplot(aes(x = stars, y = price, color = room_type)) + 
  geom_point() 
```

    ## Warning: Removed 9962 rows containing missing values (geom_point).

![](bootsrap_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
nyc_airbnb %>% 
  filter(boro == "Manhattan") %>% 
  modelr::bootstrap(n = 1000, id = "strap_number") %>% 
  mutate(
    models = map(.x = strap, ~lm(price ~ stars, data = .x)),
    results = map(models, broom::tidy)
  ) %>% 
  select(strap_number, results) %>% 
  unnest(results)
```

    ## # A tibble: 2,000 x 6
    ##    strap_number term        estimate std.error statistic  p.value
    ##    <chr>        <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 0001         (Intercept)    -28.4     28.6     -0.994 3.20e- 1
    ##  2 0001         stars           42.5      5.95     7.14  9.46e-13
    ##  3 0002         (Intercept)    -83.1     18.8     -4.43  9.33e- 6
    ##  4 0002         stars           53.4      3.91    13.7   2.97e-42
    ##  5 0003         (Intercept)     31.0     27.0      1.15  2.50e- 1
    ##  6 0003         stars           30.0      5.62     5.33  9.72e- 8
    ##  7 0004         (Intercept)    -69.5     18.5     -3.77  1.66e- 4
    ##  8 0004         stars           50.3      3.85    13.1   6.84e-39
    ##  9 0005         (Intercept)    -65.6     18.5     -3.54  4.09e- 4
    ## 10 0005         stars           49.2      3.86    12.7   5.57e-37
    ## # ... with 1,990 more rows
