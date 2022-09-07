
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FAMS

<!-- badges: start -->
<!-- badges: end -->

Fisheries Analysis and Modeling Simulator

## Purpose

To recreate and expand/improve upon the FAMS/FAST software functionality
for equilibrium and dynamic pool models, utilizing existing fishR and
FSA components wherever possible

## Status

Just starting to code!

## Process

### Equilibrium Modeling

1.  Evaluate steps in the analysis.
2.  Determine whether any steps need updating in analytical approach to
    be current with fisheries science.
3.  Determine whether any steps need expanding upon to better
    incorporate current fisheries science.
4.  Begin coding, concurrently documenting and establishing error and
    validation tests.

### RShiny Development

1.  Determine which variables should be allowed to vary in interface.
2.  Build a Shiny app for testing different length regulation scenarios.

### Dynamic Pool Modeling

1.  Not there yet.

## Installation (Section Under Development)

You can install the development version of FAMS from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fishR-Core-Team/FAMS")
```

## Example (Section Under Development)

This is a basic example which shows you how to solve a common problem:

``` r
library(FAMS)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
