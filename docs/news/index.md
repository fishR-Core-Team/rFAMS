# Changelog

## rFAMS 0.0.3

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
- [`iCheckCondMort()`](https://fishr-core-team.github.io/rFAMS/reference/iCheckCondMort.md):
  Added. Currently only used in `showMorts()`. However, I think that
  this is more general than some other functions and, thus, could
  replace `iCheckcf()`, `iCheckcm()`, `iCheckcfVect()`,
  `iCheckcmVect()`, and possibly a few others.
- [`iErrGT()`](https://fishr-core-team.github.io/rFAMS/reference/iErrGT.md):
  Modified to allow for checks on a vector rather than just a single
  value, which it still works for.
- [`iErrLT()`](https://fishr-core-team.github.io/rFAMS/reference/iErrLT.md):
  Modified to allow for checks on a vector rather than just a single
  value, which it still works for.
- [`seeMorts()`](https://fishr-core-team.github.io/rFAMS/reference/seeMorts.md):
  Added.
- Internal checks have been expanded to cover more user inputs and
  provide better warnings.
- `_func()` have are no longer “exported”. They remain in the package
  but a help document is not created for them. These functions are not
  intended to be directly interacted with by the user.

## rFAMS 0.0.2

CRAN release: 2026-02-10

- Addressed comments in DESCRIPTION file from CRAN reviewer.

## rFAMS 0.0.1

### Thank you to our contributors to Version 0.0.1!

- @jcdoll79
- @madelinelewis230
- @hiaboehm
- @droglenc
