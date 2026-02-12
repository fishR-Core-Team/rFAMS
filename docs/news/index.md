# Changelog

## rFAMS 0.0.2.9000

- This is the start of the next version
- Removed `yprBH_MinLL_fixed()` and `yprBH_Min_var()`, and replaced with
  a single function `ypr_BH_MinLL()`. The new function will allow for
  single or multiple minimum length limits with the same function.
- Modified `ypr_BH_MinLL()` to require vector input for minLL, cf,
  and cm. Instead of asking users to supply min, max, and increment for
  each, the user now supplies a single or multiple values in a vector.
  The vector can be of any sequence.
- Modified `ypr_BH_SlotLL()` to require vector input for cm. Instead of
  asking users to supply min, max, and increment for each, the user now
  supplies a single or multiple values in a vector. The vector can be of
  any sequence.
- Renamed `dpmBH_MinLL_fixed()` to
  [`dpmBH_MinLL()`](https://fishr-core-team.github.io/rFAMS/reference/dpmBH_MinLL.md)
  for consistent function names.

## rFAMS 0.0.2

- Addressed comments in DESCRIPTION file from CRAN reviewer.

## rFAMS 0.0.1

### Thank you to our contributors to Version 0.0.1!

- @jcdoll79
- @madelinelewis230
- @hiaboehm
- @droglenc
