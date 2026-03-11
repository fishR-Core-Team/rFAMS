# Generate a vector of number of recruits for the dynamic pool model.

This function is used to generate number of recruits across multiple
years using different random functions.

## Usage

``` r
genRecruits(
  simyears,
  method = c("fixed", "uniform", "normal", "StrYC_Nth", "StrYC_randInt"),
  nR = NULL,
  minR = NULL,
  maxR = NULL,
  meanR = NULL,
  sdR = NULL,
  nStr = NULL,
  sizeStr = NULL,
  avgFreq = NULL
)

# S3 method for class 'GENREC'
print(x, ...)
```

## Arguments

- simyears:

  A single numeric that sets the number of years to simulate recruitment

- method:

  A single string to call the method of generating a vector of recruits.
  `fixed` generates recruitment based on a fixed value for each year of
  simyears, `uniform` generates recruitment based on random values from
  a uniform distribution for each year of simyears, `normal` generates
  recruitment based on random values from a normal distribution for each
  year of simyears, `StrYC_Nth` generates recruitment based on a strong
  year class every Nth year, and `StrYC_randInt` generates recruitment
  based on a strong year classes at random intervals.

- nR:

  A single numeric that sets the fixed number of recruitment. Used when
  `method="fixed"` or `method="StrYC_Nth"`.

- minR:

  A single numeric that sets the minimum number of recruits during
  simulations. Used when `method="uniform"` or `method="normal"`.

- maxR:

  A single numeric that sets the maximum number of recruits during
  simulations. Used when `method="uniform"` or `method="normal"`.

- meanR:

  A single numeric that sets the mean number of recruits. Used when
  `method="normal"` or `method="StrYC_randInt"`.

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

- x:

  Object saved from `genRecruits`.

- ...:

  Optional arguments for `print`.

## Value

A vector that contains the number of recruits for each simulation that
can be used directly in the dynamic pool model (e.g.,
[`dpmBH_MinLL`](https://fishr-core-team.github.io/rFAMS/reference/dpmBH_MinLL.md)).

## Author

Jason C. Doll, <jason.doll@fmarion.edu>

## Examples

``` r
# Generate recruits for 20 years based on a fixed number
rec <- genRecruits(simyears=20,method="fixed",nR=50)
rec
#>  [1] 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50 50

# Generate recruits for 20 years from a uniform distribution bound
# by 25 and 75
rec <- genRecruits(simyears=20,method="uniform",minR=25,maxR=75)
rec
#>  [1] 65 66 45 36 46 58 50 58 51 67 60 69 26 69 75 50 43 64 54 57

# Generate recruits for 20 years based on a normal distribution with a mean
# of 50, standard deviation of 10, and trucated to be between 25 and 75
rec <- genRecruits(simyears=20,method="normal",minR=25,maxR=75,meanR=50,sdR=10)
rec
#>  [1] 61 43 61 48 38 40 61 51 55 33 35 53 63 52 63 55 56 49 52 49

# Geneate recruits for 20 years based on a fixed number of recruits at 50 and
# a strong year class every 5 years with recruits 2 times the mean recruits
rec <- genRecruits(simyears=20,method="StrYC_Nth",nR=50,sizeStr=2,nStr=5)
rec
#>  [1]  50  50  50  50 100  50  50  50  50 100  50  50  50  50 100  50  50  50  50
#> [20] 100

# Generate recruits for 20 years based on a fixed number of recruits at 50
# and a strong year class at random intervals of size 2 times the mean
# recruitswith the random interval averaging every 5 years.
rec <- genRecruits(simyears=20,method="StrYC_randInt",nR=50,sizeStr=2,avgFreq=5)
rec
#>  [1] 100 100 100  50  50 100  50  50  50  50 100  50  50 100  50  50  50  50  50
#> [20]  50
```
