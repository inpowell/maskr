
<!-- README.md is generated from README.Rmd. Please edit that file -->

# maskr

<!-- badges: start -->

[![R-CMD-check](https://github.com/inpowell/maskr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/inpowell/maskr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

maskr introduces the `masked` class that suppresses or “masks” printing
of certain elements of an atomic vector while keeping the underlying
data available for computation. Masked vectors can include numeric,
logical character or factor data.

## Installation

maskr can be installed from CRAN using:

``` r
install.packages('maskr')
```

You can also install the development version of maskr from GitHub:

``` r
devtools::install_github('inpowell/maskr')
```

## Example

``` r
library(maskr)
```

Masked vectors can be built from atomic vectors using `masked()`:

``` r
x <- 0:8
masked(x, 0 < x & x < 5)
#> <integer+masked[9]>
#>    0 n.p. n.p. n.p. n.p.    5    6    7    8
```

Here, the masked cells are indicated by “n.p.” (not published) by
default. We can change how they are presented using the
`maskr.replacement` option:

``` r
options(maskr.replacement = '*')
masked(x, 0 < x & x < 5)
#> <integer+masked[9]>
#> 0 * * * * 5 6 7 8
options(maskr.replacement = NULL)
```

Other types of atomic vectors can be masked as well:

``` r
masked(letters, letters %in% c('a', 'e', 'i', 'o', 'u'))
#> <character+masked[26]>
#> n.p. b    c    d    n.p. f    g    h    n.p. j    k    l    m    n    n.p. p    
#> q    r    s    t    n.p. v    w    x    y    z
```

We can also use this to control which data gets displayed in data frames
and cross-tables.

``` r
tabular <- tibble::tibble(
  Activity = gl(4, 4, 16, labels = c("I", "II", "III", "Total")),
  Region = gl(4, 1, 16, labels = c("A", "B", "C", "Total")),
  Count = as.integer(c(
    10,  25,   5,  40,
    16,  13,  11,  40,
    17,  20,  24,  61,
    43,  58,  40, 141
  ))
)
suppress <- rep(FALSE, 16L)
suppress[c(5, 8, 9, 12)] <- TRUE
tabular$Count <- masked(tabular$Count, suppress)
tabular
#> # A tibble: 16 × 3
#>    Activity Region     Count
#>    <fct>    <fct>  <int+msk>
#>  1 I        A             10
#>  2 I        B             25
#>  3 I        C              5
#>  4 I        Total         40
#>  5 II       A           n.p.
#>  6 II       B             13
#>  7 II       C             11
#>  8 II       Total       n.p.
#>  9 III      A           n.p.
#> 10 III      B             20
#> 11 III      C             24
#> 12 III      Total       n.p.
#> 13 Total    A             43
#> 14 Total    B             58
#> 15 Total    C             40
#> 16 Total    Total        141
```

This works with tidyverse reshaping functions like `pivot_wider()`:

``` r
tidyr::pivot_wider(tabular, names_from = 'Region', values_from = 'Count')
#> # A tibble: 4 × 5
#>   Activity         A         B         C     Total
#>   <fct>    <int+msk> <int+msk> <int+msk> <int+msk>
#> 1 I               10        25         5        40
#> 2 II            n.p.        13        11      n.p.
#> 3 III           n.p.        20        24      n.p.
#> 4 Total           43        58        40       141
```

Masked vectors support basic arithmetic, so for example we can find
percentages while maintaining the correct masking pattern.

``` r
tabular |>
  dplyr::group_by(Activity) |>
  dplyr::mutate(Percent = 100 * Count / Count[Region == 'Total'])
#> # A tibble: 16 × 4
#> # Groups:   Activity [4]
#>    Activity Region     Count   Percent
#>    <fct>    <fct>  <int+msk> <dbl+msk>
#>  1 I        A             10      25  
#>  2 I        B             25      62.5
#>  3 I        C              5      12.5
#>  4 I        Total         40     100  
#>  5 II       A           n.p.      n.p.
#>  6 II       B             13      n.p.
#>  7 II       C             11      n.p.
#>  8 II       Total       n.p.      n.p.
#>  9 III      A           n.p.      n.p.
#> 10 III      B             20      n.p.
#> 11 III      C             24      n.p.
#> 12 III      Total       n.p.      n.p.
#> 13 Total    A             43      30.5
#> 14 Total    B             58      41.1
#> 15 Total    C             40      28.4
#> 16 Total    Total        141     100
```

Notice that where we have divided by a masked cell, the percentage is
also masked.

Using masked vectors, as opposed to just replacing values we want to
suppress with missing values, means we can always recover our data
before we publish it:

``` r
tabular$Count <- unmask(tabular$Count)
tabular
#> # A tibble: 16 × 3
#>    Activity Region Count
#>    <fct>    <fct>  <int>
#>  1 I        A         10
#>  2 I        B         25
#>  3 I        C          5
#>  4 I        Total     40
#>  5 II       A         16
#>  6 II       B         13
#>  7 II       C         11
#>  8 II       Total     40
#>  9 III      A         17
#> 10 III      B         20
#> 11 III      C         24
#> 12 III      Total     61
#> 13 Total    A         43
#> 14 Total    B         58
#> 15 Total    C         40
#> 16 Total    Total    141
```
