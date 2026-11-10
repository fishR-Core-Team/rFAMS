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
makeSPR(
  FLR = "log10",
  FLRint,
  FLRslope,
  MatAge,
  percF,
  percFSpawn,
  ageInterval = 0.1
)
```

## Arguments

- FLR:

  A character string to indicate if the fecundity-length relationship is
  `log10` (logarithmic base 10) or `linear`

- FLRint:

  A single numeric that represents intercept of the fecundity-length
  relationship.

- FLRslope:

  A single numeric that represents slope of the fecundity-length
  relationship

- MatAge:

  A single numeric that represent the maximum age.

- percF:

  A vector of the percent of all fish that are females at age starting
  at age-1.

- percFSpawn:

  A vector of the percent of females that spawn annually starting at
  age-1.

- ageInterval:

  A single numeric that represents the age interval to calculate SPR in
  the yield-per-recruit simulation. An interval less than 1 permits
  calculating fecundity at finer time steps than annual.

## Value

A named list that contains the given reproduction options that can be
used directly in the yield-per-recruit and dynamic pool models (e.g.,
[`yprBH_SlotLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_SlotLL.md)
and
[`dpmBH_MinLL`](https://fishr-core-team.github.io/rFAMS/reference/dpmBH_MinLL.md)).

## Author

Jason C. Doll, <jason.doll@fmarion.edu>

## Examples

``` r
# This example creates a list where fish mature at age-4. Leading 0's are used in
# the vectors for percF and percFSpawn because they do not contribute to eggs produced

SPRdat<- makeSPR(FLR = "linear", FLRint = -1057029, FLRslope = 2777.08, MatAge = 4,
                 percF=c(0,0,0,rep(0.50,27)),
                 percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)))
```
