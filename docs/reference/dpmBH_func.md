# Simulate yield under minimum length regulations using the dynamic pool model.

An INTERNAL function used by
[`dpmBH_MinLL`](https://fishr-core-team.github.io/rFAMS/reference/dpmBH_MinLL.md)
to estimate yield under minimum length limit regulations using the
Dynamic Pool (DPM) model with a provided minimum length limit for
harvest (`minLL`), vector for conditional fishing mortality (`cf`),
vector of conditional natural mortality (`cm`), vector of recruitment
abundance (`rec`). This is the base function for
[`dpmBH_MinLL`](https://fishr-core-team.github.io/rFAMS/reference/dpmBH_MinLL.md),
is NOT exported, and is NOT expected to be used directly by the user.

## Usage

``` r
dpmBH_func(minLL, cf, cm, rec, lhparms, matchRicker)
```

## Arguments

- minLL:

  A single numeric representing the minimum length limit for harvest in
  mm.

- cf:

  A matrix of conditional fishing mortality where each row represents a
  year and each column represents an age (age-0 through maximum age;
  i.e., `tmax` in `lhparms`). All values must be between 0 and 1
  (inclusive).

- cm:

  A matrix of conditional natural mortality where each row represents a
  year and each column represents an age (age-0 through maximum age;
  i.e., `tmax` in `lhparms`). All values must be between 0 and 1
  (inclusive).

- rec:

  A numeric vector with length `simyears` that specifies the number of
  recruits each year. This vector is best generated using the
  [`genRecruits`](https://fishr-core-team.github.io/rFAMS/reference/genRecruits.md).
  All values must be greater than 0.

- lhparms:

  A named vector or list that contains values for each `N0`, `tmax`,
  `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta`. See
  [`makeLH`](https://fishr-core-team.github.io/rFAMS/reference/makeLH.md)
  for definitions of these life history parameters. Also see details.

- matchRicker:

  A logical that indicates whether the yield function should match that
  in Ricker (1975). Defaults to `FALSE`. See the [FAMS vs Ricker
  article](https://fishr-core-team.github.io/rFAMS/articles/YPR_FAMSvRICKER.html).

## Value

A one row data.frame with the items described for the first data.frame
returned by
[`dpmBH_MinLL`](https://fishr-core-team.github.io/rFAMS/reference/dpmBH_MinLL.md).

## Details

See details in
[`dpmBH_MinLL`](https://fishr-core-team.github.io/rFAMS/reference/dpmBH_MinLL.md).

## Author

Jason C. Doll, <jason.doll@fmarion.edu>
