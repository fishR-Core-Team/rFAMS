# Make checks of combinations of `cf` values and `recruitmentTL` for slot limits

Make checks of combinations of `cf` values and `recruitmentTL` for slot
limits

## Usage

``` r
iCheckSlotType(cfu, cfi, cfa, rtl, strict = TRUE)
```

## Arguments

- cfu:

  A `cfBelow` value.

- cfi:

  A `cfIn` value.

- cfa:

  A `cfAbove` value.

- rtl:

  A `recruitmentTL` value.

- strict:

  A logical that indicates how strict the test should be. See details.

## Details

`strict` is a logical that indicates whether strict criterion for values
of `recruitmentTL`, `cfBelow`, `cfIn`, and `cfAbove` should be used. If
`strict=TRUE` then the only accepted combinations are that a
`recruitmentTL` is given (i.e., not `NULL`), `cfBelow`\>0, `cfAbove`\>0,
and `cfIn`=0 (i.e., simulating a protected slot) or `recruitmentTL` is
`NULL`, `cfBelow`=0, `cfAbove`=0, and `cfIn`\>0 (i.e., simulating an
inverse/harvest slot). If `strict=FALSE` then the only restrictions are
that the three `cf`s cannot all =0, and that if `cfBelow` is given them
`recruitmentTL` cannot be `NULL`. **This argument allows us to model
each type of restrictions while we ultimately decide which one to use.**
