# Make a list or vector of life history parameters for yield-per-recruit analyses.

Efficiently construct either a vector or list that contains the seven
life history parameters required for Beverton-Holt yield-per-recruit
analyses. The parameters can be given by the user through function
arguments. Alternativvely, the von Bertalanffy parameters (`Linf`, `K`,
and `t0`) may be extracted from an `nls` object created from fitting the
von Bertalanffy equation to length-at-age data (object created outside
this function). Similarly the log10-transformed weight-length model
coefficients may be extracted from an `lm` object created from fitting
the model to transformed weight-length data (object created outside this
function). All parameter values are checked for sanity (e.g., Linf\>0).

## Usage

``` r
makeLH(N0, tmax, Linf, K, t0, LWalpha, LWbeta, restype = c("list", "vector"))
```

## Arguments

- N0:

  A single numeric that represents the number of fish in the population
  at the hypothetical age of `t0`.

- tmax:

  A single whole number that represents maximum age in the population in
  years.

- Linf:

  A single numeric that represents the point estimate of asymptotic mean
  length from the von Bertalanffy growth model OR an `nls` object
  created from fitting the von Bertalanffy equation to length-at-age
  data.

- K:

  A single numeric that represents the point estimate of the Brody
  growth coefficient from the von Bertalanffy growth model.

- t0:

  A single numeric that represents the point estimate of the x-intercept
  (i.e., theoretical age at a mean length of 0) from the von Bertalanffy
  growth model.

- LWalpha:

  A single numeric that represents the point estimate of alpha from the
  length-weight regression on the log10 scale OR an `lm` object created
  from fitting the model to log10-transformed weight-length data.

- LWbeta:

  A single numeric that represents the point estimate of beta from the
  length-weight regression on the log10 scale.

- restype:

  A character that indicates the type of output (list or vector)
  returned by the function.

## Value

A named list or vector (depending on `restype`) that contains the given
(or extracted) life history parameters values that can be used directly
in the yield-per-recruit calculation functions (e.g.,
[`yprBH_SlotLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_SlotLL.R.md)).

## Details

Use of this function for putting life history parameters into a list or
vector is recommended as (i) values for `Linf`, `K`, `t0`, `LWalpha`,
and `LWbeta` can be extracted from objects from appropriate model
fitting and (ii) checks for impossible or improbable values for each
parameter are performed; i.e.,

## See also

[this demonstration
page](https://fishr-core-team.github.io/rFAMS/articles/MakeLH.html) for
more plotting examples

## Author

Derek Ogle

    # Best practice for entering life history parameter values
    LH <- makeLH(N0=100,tmax=15,Linf=600,K=0.30,t0=-0.6,
                 LWalpha=-5.453,LWbeta=3.10)

    # Works but no checks on the values
    LH <- list(N0=100,tmax=15,Linf=600,K=0.30,t0=-0.6,
               LWalpha=-5.453,LWbeta=3.10)

If a list is returned then values will be displayed with the number of
decimals provided by the user. If a vector is returned then the number
of decimals displayed will be the same for each value and will match the
value supplied by the user with the most decimals. Thus, a list is
preferred as it will be easier to match what was given to what was
expected to be given.

## Examples

``` r
library(FSA)
#> ## FSA v0.10.1. See citation('FSA') if used in publication.
#> ## Run fishR() for related website and fishR('IFAR') for related book.
library(FSAdata)
#> ## FSAdata v0.4.1. See ?FSAdata to find data for specific fisheries analyses.
library(dplyr)

# ----- Simple examples with explicity arguments for each -------------------
makeLH(N0=100,tmax=15,Linf=500,K=0.3,t0=-0.5,LWalpha=-5.613,LWbeta=3.1)
#> $N0
#> [1] 100
#> 
#> $tmax
#> [1] 15
#> 
#> $Linf
#> [1] 500
#> 
#> $K
#> [1] 0.3
#> 
#> $t0
#> [1] -0.5
#> 
#> $LWalpha
#> [1] -5.613
#> 
#> $LWbeta
#> [1] 3.1
#> 
makeLH(N0=100,tmax=15,Linf=500,K=0.3,t0=-0.5,LWalpha=-5.613,LWbeta=3.1,
       restype="vector")
#>      N0    tmax    Linf       K      t0 LWalpha  LWbeta 
#> 100.000  15.000 500.000   0.300  -0.500  -5.613   3.100 

# ----- Example of extracting values from model fits ------------------------
# N0 and tmax provided as arguments ... Linf, K, and t0 extracted from nls
#   output and LWalpha and LWbeta extracted from lm output. Note that nls
#   and lm output here are just examples of the function, they should be
#   calculated for the same species from the same waterbody, etc.

# Load data from FSAdata package, restrict to one location and year,
# create log10 values of weight and length
data(WalleyeErie2,package="FSAdata")
tmp <- WalleyeErie2 |>
  filter(loc==2,year==2010) |>
  mutate(logW=log10(w),
         logL=log10(tl))

# Generate LVB results
vb1 <- FSA::makeGrowthFun(type="von Bertalanffy")
fit1 <- nls(tl~vb1(age,Linf,K,t0),data=tmp,
            start=FSA::findGrowthStarts(tl~age,data=tmp))

# Generate length-weight regression results
fit2 <- lm(logW~logL,data=tmp)

# Make life-history list with those results
waeLH <- makeLH(N0=100,tmax=15,Linf=fit1,LWalpha=fit2)
waeLH
#> $N0
#> [1] 100
#> 
#> $tmax
#> [1] 15
#> 
#> $Linf
#> [1] 587.9484
#> 
#> $K
#> [1] 0.4851255
#> 
#> $t0
#> [1] -1.050782
#> 
#> $LWalpha
#> [1] -5.857295
#> 
#> $LWbeta
#> [1] 3.329038
#> 
```
