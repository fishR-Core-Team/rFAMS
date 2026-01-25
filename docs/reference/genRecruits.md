# Generate a vector of recruitment abundance for the dynamic pool model.

These function is used to generate recruitment abundances across
multiple years using different random function.

## Usage

``` r
genRecruits(
  method = c("fixed", "uniform", "normal", "StrYC_Nth", "StrYC_randInt"),
  simyears = 50,
  Nrec = NULL,
  MinR = NULL,
  MaxR = NULL,
  meanR = NULL,
  sdR = NULL,
  Nthyr = NULL,
  sizeStr = NULL,
  avgFreq = NULL
)
```

## Arguments

- method:

  A single string to call the method of generating a vector of recruits.
  `fixed` generate recruitment based on a fixed value for each year of
  simyears, `uniform` generates recruitment based on random values from
  a unifrom distribution for each year of simyears, `normal` generates
  recruitment based on random values from a unifrom distribution for
  each year of simyears, `StrYC_Nth` generates recruitment based on a
  strong year class every Nth year, and `StrYC_randInt` generates
  recruitment based on a strong year classes at random intervals.

- simyears:

  A single numeric that sets the number of years to simulate recruitment

- Nrec:

  A single numeric that sets the fixed number of recruitment

- MinR:

  A single numeric that sets the minimum recruitment abundance during
  simulations.

- MaxR:

  A single numeric that sets the maximum recruitment abundance during
  simulations.

- meanR:

  A single numeric that sets the mean recruitment abundance.

- sdR:

  A single numeric that sets the standard deviation of recruitment
  abundance

- Nthyr:

  A single numeric that sets the Nth year that a strong year class will
  occur

- sizeStr:

  A single numeric that sets the multiplier for the strong year class
  relative to meanR

- avgFreq:

  A single numeric that sets the average frequency of a strong year
  class.

## Value

A vector that contains the given recruitment options that can be used
directly in the dynamic pool model (e.g.,
[`dpmBH`](https://fishr-core-team.github.io/rFAMS/reference/dpmBH.md)).

## Details

This function is used internally and not generally used interactively

## Author

Jason C. Doll, <jason.doll@fmarion.edu>

## Examples

``` r
# To be filled out later
```
