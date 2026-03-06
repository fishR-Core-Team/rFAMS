# Make checks of combinations of `cf` values and `recruitmentTL` for slot limits

Make checks of combinations of `cf` values and `recruitmentTL` for slot
limits

## Usage

``` r
iCheckSlotType(cfu, cfi, cfa, rtl, strict = FALSE)
```

## Arguments

- cfu:

  A `cfunder` value.

- cfi:

  A `cfin` value.

- cfa:

  A `cfabove` value.

- rtl:

  A `recruitmentTL` value.

- strict:

  A logical that indicates how strict the test should be. See details.

## Details

`strict` is a logical that indicates whether strict criterion for values
of `recruitmentTL`, `cfunder`, `cfin`, and `cfabove` should be used. If
`strict=TRUE` then the only accepted combinations are that a
`recruitmentTL` is given (i.e., not `NULL`), `cfunder`\>0, `cfabove`\>0,
and `cfin`=0 (i.e., simulating a protected slot) or `recruitmentTL` is
`NULL`, `cfunder`=0, `cfabove`=0, and `cfin`\>0 (i.e., simulating an
inverse/harvest slot). If `strict=FALSE` then the only restrictions are
that the three `cf`s cannot all =0, and that if `cfunder` is given them
`recruitmentTL` cannot be `NULL`. **This argument allows us to model
each type of restrictions while we ultimately decide which one to use.**
