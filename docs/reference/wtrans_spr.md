# Function to calculate weighted transitional spawning potential ratio

An INTERNAL function used by
[`yprBH_MinLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_MinLL.md)
and
[`yprBH_SlotLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_SlotLL.md)
to calculate weighted transitional spawning potential ratio with the
dynamic pool model

## Usage

``` r
wtrans_spr(df, SPRdat)
```

## Arguments

- df:

  A data frame containing results from the dynamic pool model by age.

- SPRdat:

  A named list that contains values for each `FLR`, `FLRint`,
  `FLRslope`, and `MatAge`. See
  [`makeSPR`](https://fishr-core-team.github.io/rFAMS/reference/makeSPR.md)
  for definitions of these parameters.

## Value

A vector

- `wtSPR` is a vector containing weighted transitional spawning
  potential ratio for each year of the simulation

## Details

This function is generally not used independently. It is called when
requesting the spawning potential ratio while calculating
yield-per-recruit.

## Author

Derek Ogle, <jason.doll@fmarion.edu>

Jason C. Doll, <DerekOgle51@gmail.com>
