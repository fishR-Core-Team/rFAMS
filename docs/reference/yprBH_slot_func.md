# Simulate expected yield under slot length limits using the Beverton-Holt Yield-per-Recruit model

An INTERNAL function used by
[`yprBH_SlotLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_SlotLL.md)
to estimate yield under slot (protected or inverse/harvest) length limit
regulations using the Beverton-Holt Yield-per-Recruit (YPR) model with
one value each of `cm` (and `lowerSL`, `upperSL`, `cfunder`, `cfin`, and
`cfabove`). This is the base function for
[`yprBH_SlotLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_SlotLL.md),
is NOT exported, and is NOT expected to be used directly by the user.

## Usage

``` r
yprBH_slot_func(
  lowerSL,
  upperSL,
  cfunder,
  cfin,
  cfabove,
  cm,
  lhparms,
  recruitmentTL,
  loi,
  matchRicker
)
```

## Arguments

- lowerSL:

  A single numeric representing the length of the lower slot limit
  in mm. See details. Must be less than `upperSL`.

- upperSL:

  A single numeric representing the length of the upper slot limit
  in mm. See details. Must be less than `Linf` in `lhparms`.

- cfunder:

  A single numeric representing conditional fishing mortality under the
  lower slot limit length. Must be between 0 and 1 (inclusive).

- cfin:

  A single numeric representing conditional fishing mortality between
  the lower and upper slot limit lengths (i.e., "in the slot"). Must be
  between 0 and 1 (inclusive).

- cfabove:

  A single numeric representing conditional fishing mortality above the
  upper slot limit length. Must be between 0 and 1 (inclusive).

- cm:

  A SINGLE numeric representing conditional natural mortality.

- lhparms:

  A named vector or list that contains values for each `N0`, `tmax`,
  `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta`. See
  [`makeLH`](https://fishr-core-team.github.io/rFAMS/reference/makeLH.md)
  for definitions of these life history parameters. Also see details.

- recruitmentTL:

  A single numeric that represents the minimum length (in mm) for
  recruiting to the fishery. Cannot be greater than `lowerSL`.

- loi:

  A numeric vector of lengths (in mm) of interest. Used to determine
  number of fish that reach these lengths. All must be less than `Linf`
  in `lhparms`.

- matchRicker:

  A logical that indicates whether the yield function should match that
  in Ricker (1975). Defaults to `TRUE`. The only reason to changed to
  `FALSE` is to try to match output from FAMS. See the [FAMS vs Ricker
  article](https://fishr-core-team.github.io/rFAMS/articles/YPR_FAMSvRICKER.html).

## Value

A one row data.frame with the items described in
[`yprBH_SlotLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_SlotLL.md).

## Details

See details in
[`yprBH_SlotLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_SlotLL.md).

## Author

Jason C. Doll, <jason.doll@fmarion.edu>
