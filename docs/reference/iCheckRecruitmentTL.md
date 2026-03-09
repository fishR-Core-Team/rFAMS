# Make checks of recruitment total length

Make checks of recruitment total length

## Usage

``` r
iCheckRecruitmentTL(x, Linf, lowerSL)
```

## Arguments

- x:

  A recruitment total length value.

- Linf:

  A value of Linf.

- lowerSL:

  A value for the lower slot limit length.

## Details

Don't check for missing as `recruitmentTL` is `NULL` by default in the
major functions or the user changed it to something (very unlikely they
changed it to missing). Thus, don't need `optname=` argument used in
other functions.

Tests of `recruitmentTL` relative to the type of slot limit are in
[`iCheckSlotType()`](https://fishr-core-team.github.io/rFAMS/reference/iCheckSlotType.md).

If `recruitmentTL=NULL`, just pass through, don't do any tests.
