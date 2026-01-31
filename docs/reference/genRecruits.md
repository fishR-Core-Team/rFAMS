# Generate a vector of number of recruits for the dynamic pool model.

This function is used to generate number of recruits across multiple
years using different random functions.

## Usage

``` r
genRecruits(
  method = c("fixed", "uniform", "normal", "StrYC_Nth", "StrYC_randInt"),
  simyears = 50,
  nR = NULL,
  minR = NULL,
  maxR = NULL,
  meanR = NULL,
  sdR = NULL,
  nStr = NULL,
  sizeStr = NULL,
  avgFreq = NULL
)
```

## Arguments

- method:

  A single string to call the method of generating a vector of recruits.
  `fixed` generates recruitment based on a fixed value for each year of
  simyears, `uniform` generates recruitment based on random values from
  a uniform distribution for each year of simyears, `normal` generates
  recruitment based on random values from a normal distribution for each
  year of simyears, `StrYC_Nth` generates recruitment based on a strong
  year class every Nth year, and `StrYC_randInt` generates recruitment
  based on a strong year classes at random intervals.

- simyears:

  A single numeric that sets the number of years to simulate recruitment

- nR:

  A single numeric that sets the fixed number of recruitment. Used when
  `method="fixed"`.

- minR:

  A single numeric that sets the minimum number of recruits during
  simulations. Used when `method="uniform"` or `method="normal"`.

- maxR:

  A single numeric that sets the maximum number of recruits during
  simulations. Used when `method="uniform"` or `method="normal"`.

- meanR:

  A single numeric that sets the mean number of recruits. Used when
  `method="normal"`, `method="StrYC_Nth"`, or `method="StrYC_randInt"`.

- sdR:

  A single numeric that sets the standard deviation of number of
  recruits. Used when `method="normal"`.

- nStr:

  A single numeric that sets the Nth year that a strong year class will
  occur. Used when `method="StrYC_Nth"`.

- sizeStr:

  A single numeric that sets the multiplier for the strong year class
  relative to meanR. Used when `method="StrYC_Nth"`, or
  `method="StrYC_randInt"`.

- avgFreq:

  A single numeric that sets the average frequency of a strong year
  class. Used when `method="StrYC_randInt"`.

## Value

A vector that contains the number of recruits for each simulation that
can be used directly in the dynamic pool model (e.g.,
[`dpmBH`](https://fishr-core-team.github.io/rFAMS/reference/dpmBH.md)).

## Author

Jason C. Doll, <jason.doll@fmarion.edu>

## Examples

``` r
# Genearte recruits based on a fixed number
genRecruits("fixed",nR=50)
#>  [1] 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50
#> [26] 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50

# Generate recruits from a uniform distribution bound by 25 and 75
genRecruits("uniform",minR=25,maxR=75)
#>  [1] 51 67 60 69 26 69 75 50 43 64 54 57 68 53 38 71 68 37 45 63 31 35 33 58 68
#> [26] 71 53 54 59 37 27 70 29 75 56 34 70 27 55 37 70 66 60 36 61 36 48 42 53 38

# Generate recruits based on a normal distribution with a mean of 50,
# standard deviation of 10, and trucated to be between 25 and 75
genRecruits("normal",minR=25,maxR=75,meanR=50,sdR=10)
#>  [1] 49 69 63 57 56 45 61 25 48 54 46 54 61 60 50 55 67 46 59 63 47 46 42 39 42
#> [26] 32 43 44 45 52 60 48 36 53 46 56 71 54 33 52 61 70 54 64 54 51 50 56 47 45

# Geneate recruits based on a fixed mean recruit number of 50 and a
# strong year class every 5 years with recruits 2 times the mean recruits
genRecruits("StrYC_Nth",meanR=50,sizeStr=2,nStr=5)
#>  [1]  50  50  50  50 100  50  50  50  50 100  50  50  50  50 100  50  50  50  50
#> [20] 100  50  50  50  50 100  50  50  50  50 100  50  50  50  50 100  50  50  50
#> [39]  50 100  50  50  50  50 100  50  50  50  50 100

# Generate recruits based on a fixed mean recruit number of 50 and a
# strong year class at random intervals of size 2 times the mean recruits
# with the random interval averaging every 5 years.
genRecruits("StrYC_randInt",meanR=50,sizeStr=2,avgFreq=5)
#>  [1]  50  50  50 100 100  50  50  50  50  50  50  50  50 100  50 100  50  50 100
#> [20]  50  50  50  50  50  50  50 100  50  50 100 100  50 100  50  50  50  50  50
#> [39]  50  50 100  50  50  50  50 100  50  50  50  50
```
