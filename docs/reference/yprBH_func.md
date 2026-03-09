# Simulate expected yield under minimum length regulations using the Beverton-Holt Yield-per-Recruit model

An INTERNAL function used by
[`yprBH_MinLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_MinLL.md)
to estimate yield under minimum length limit regulations using the
Beverton-Holt Yield-per-Recruit (YPR) model with one value each of
`minLL`, `cf`, and `cm`. This is the base function for
[`yprBH_MinLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_MinLL.md),
is NOT exported, and is NOT expected to be used directly by the user.

## Usage

``` r
yprBH_func(minLL, cf, cm, lhparms, loi, matchRicker)
```

## Arguments

- minLL:

  A SINGLE numeric representing the minimum length limit for harvest in
  mm.

- cf:

  A SINGLE numeric representing conditional fishing mortality.

- cm:

  A SINGLE numeric representing conditional natural mortality.

- lhparms:

  A named vector or list that contains values for each `N0`, `tmax`,
  `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta`. See
  [`makeLH`](https://fishr-core-team.github.io/rFAMS/reference/makeLH.md)
  for definitions of these life history parameters. Also see details.

- loi:

  A numeric vector of lengths (in mm) of interest. Used to determine
  number of fish that reach these lengths. All must be less than `Linf`
  in `lhparms`.

- matchRicker:

  A logical that indicates whether the yield function should match that
  in Ricker (1975). Defaults to `FALSE`. See the [FAMS vs Ricker
  article](https://fishr-core-team.github.io/rFAMS/articles/YPR_FAMSvRICKER.html).

## Value

A one row data.frame with the items described in
[`yprBH_MinLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_MinLL.md).

## Details

See details in
[`yprBH_MinLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_MinLL.md).

## Author

Jason C. Doll, <jason.doll@fmarion.edu>
