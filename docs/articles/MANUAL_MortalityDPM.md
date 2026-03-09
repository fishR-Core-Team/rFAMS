# Mortality matrices with the dynamic pool model

> **Warning**
>
> This is a work-in-progress.

## Mortality Matrices for Dynamic Pool Models

Simulations with Dynamic Pool Models (DPM) in `rFAMS` (e.g., using
`dpmBH_minLL()`) require inputs *matrices* that describe conditional
natural and fishing mortalities for **each age and year** in the
simulation. For example, if simulating a fish population with a maximum
age (i.e., `tmax`) of 5 for 10 years (i.e., `simyears`) then a matrix is
required that describe conditional natural mortality (i.e., $`cm`$) for
age-0 fish in year 1, age-1 fish in year 1, age-2 fish in year 1, and so
on through to age-5 fish in year 10.[^1] It is unlikely and unrealistic
to simulate different mortality rates for each age-year combination, so
the matrix will often contain many repeated values. Below we describe
some methods for entering these values into a matrix for use in DPM
simulations.

The matrix required by the DPM functions in `rFAMS` is very specific.
Each column corresponds to an age in the simulation, **beginning with
age-0** and ending with the maximum age.[^2] The first column, for
example, is $`cm`$ of age-0 fish for each year in the simulation. In
contrast, each row corresponds to a year in the simulation. The first
row, for example, is $`cm`$ for all ages for the first year of the
simulation. Given these requirements, there **will be `tmax`+1 columns
and `simyears` rows in the matrix**.

### Examples

Values can be entered into a matrix in `R` with
[`matrix()`](https://rdrr.io/r/base/matrix.html). The first argument to
[`matrix()`](https://rdrr.io/r/base/matrix.html) is a *vector* (i.e.,
the set) of values to be placed into the matrix. The values can be
placed into the matrix by moving across the rows first and then down the
columns (i.e., fill first row, then fill second row, etc.) by including
`byrow=TRUE`. Alternatively, the values can be placed into the matrix by
moving down columns first and then across rows (i.e., fill first column,
then fill second column, etc.) by including `byrow=FALSE`. The function
knows when to stop filling a row or column based on the requested number
of rows in `nrow=` or columns in `ncol=`. Finally, the rows and columns
can be named with a list in `dimnames=`, which will be demonstrated
below.

For all examples below, assume that the maximum age is 5 and 10 years
will be simulated with the DPM. These values are entered into constants
below to facilitate working through the different examples. Note that
`tmax` will usually be extracted from the life history parameters in
`lhparms` with `lhparms[["tmax"]]` or `lhparms$tmax`.

``` r
tmax <- 5
simyears <- 10
```

Also note that all examples below are shown for conditional natural
mortality ($`cm`$). The same procedures would be used to make matrices
of conditional fishing mortality ($`cf`$) across ages and simulation
years.

#### Example 1

As a first simple example, assume that $`cm`$ is a constant 0.3 for all
ages and years. A vector with this value repeated for the product of
`tmax`+1 and `simyears` (=60) is created to serve as the first argument
to [`matrix()`](https://rdrr.io/r/base/matrix.html).

``` r
tmp <- rep(0.3,(tmax+1)*simyears)
tmp
#>  [1] 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3
#> [20] 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3
#> [39] 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3 0.3
#> [58] 0.3 0.3 0.3
```

Given that $`cm`$ is constant across all age-year combinations the
values in this vector can be placed into the matrix either by row or by
column, but is by row below. Note that the number of rows is defined by
`simyears`.[^3]

``` r
cm <- matrix(tmp,byrow=TRUE,nrow=simyears)
cm
#>       [,1] [,2] [,3] [,4] [,5] [,6]
#>  [1,]  0.3  0.3  0.3  0.3  0.3  0.3
#>  [2,]  0.3  0.3  0.3  0.3  0.3  0.3
#>  [3,]  0.3  0.3  0.3  0.3  0.3  0.3
#>  [4,]  0.3  0.3  0.3  0.3  0.3  0.3
#>  [5,]  0.3  0.3  0.3  0.3  0.3  0.3
#>  [6,]  0.3  0.3  0.3  0.3  0.3  0.3
#>  [7,]  0.3  0.3  0.3  0.3  0.3  0.3
#>  [8,]  0.3  0.3  0.3  0.3  0.3  0.3
#>  [9,]  0.3  0.3  0.3  0.3  0.3  0.3
#> [10,]  0.3  0.3  0.3  0.3  0.3  0.3
```

The matrix may be easier to read if the rows and columns are labeled as
shown below with `dimnames=` (note that row names are given first in the
list, followed by column names).[^4]

``` r
cm <- matrix(tmp,byrow=TRUE,nrow=simyears,
             dimnames=list(paste0("yr_",1:simyears),paste0("age_",0:tmax)))
cm
#>       age_0 age_1 age_2 age_3 age_4 age_5
#> yr_1    0.3   0.3   0.3   0.3   0.3   0.3
#> yr_2    0.3   0.3   0.3   0.3   0.3   0.3
#> yr_3    0.3   0.3   0.3   0.3   0.3   0.3
#> yr_4    0.3   0.3   0.3   0.3   0.3   0.3
#> yr_5    0.3   0.3   0.3   0.3   0.3   0.3
#> yr_6    0.3   0.3   0.3   0.3   0.3   0.3
#> yr_7    0.3   0.3   0.3   0.3   0.3   0.3
#> yr_8    0.3   0.3   0.3   0.3   0.3   0.3
#> yr_9    0.3   0.3   0.3   0.3   0.3   0.3
#> yr_10   0.3   0.3   0.3   0.3   0.3   0.3
```

#### Example 2

Suppose that $`cm`$ varies by age but is constant across all years.
Further suppose that the age variability is very simple in that age-0
fish have a $`cm`$ of 0.1 whereas all other ages are 0.2.

The $`cm`$ values for each age for one year are entered into a vector by
combining (i.e., [`c()`](https://rdrr.io/r/base/c.html)) a 0.05 for
age-0 fish with a set of 0.2 values repeated (with
[`rep()`](https://rdrr.io/r/base/rep.html)) for ages 1-`tmax`..

``` r
tmp <- c(0.05,rep(0.2,tmax))
tmp
#> [1] 0.05 0.20 0.20 0.20 0.20 0.20
```

This vector is then repeated for each simulation year for input into
[`matrix()`](https://rdrr.io/r/base/matrix.html).

``` r
tmp <- rep(tmp,simyears)
tmp
#>  [1] 0.05 0.20 0.20 0.20 0.20 0.20 0.05 0.20 0.20 0.20 0.20 0.20 0.05 0.20 0.20
#> [16] 0.20 0.20 0.20 0.05 0.20 0.20 0.20 0.20 0.20 0.05 0.20 0.20 0.20 0.20 0.20
#> [31] 0.05 0.20 0.20 0.20 0.20 0.20 0.05 0.20 0.20 0.20 0.20 0.20 0.05 0.20 0.20
#> [46] 0.20 0.20 0.20 0.05 0.20 0.20 0.20 0.20 0.20 0.05 0.20 0.20 0.20 0.20 0.20
```

Finally, these values are entered by row into a matrix.

``` r
cm <- matrix(tmp,byrow=TRUE,nrow=simyears,
             dimnames=list(paste0("yr_",1:simyears),paste0("age_",0:tmax)))
cm
#>       age_0 age_1 age_2 age_3 age_4 age_5
#> yr_1   0.05   0.2   0.2   0.2   0.2   0.2
#> yr_2   0.05   0.2   0.2   0.2   0.2   0.2
#> yr_3   0.05   0.2   0.2   0.2   0.2   0.2
#> yr_4   0.05   0.2   0.2   0.2   0.2   0.2
#> yr_5   0.05   0.2   0.2   0.2   0.2   0.2
#> yr_6   0.05   0.2   0.2   0.2   0.2   0.2
#> yr_7   0.05   0.2   0.2   0.2   0.2   0.2
#> yr_8   0.05   0.2   0.2   0.2   0.2   0.2
#> yr_9   0.05   0.2   0.2   0.2   0.2   0.2
#> yr_10  0.05   0.2   0.2   0.2   0.2   0.2
```

#### Example 3

Suppose that $`cm`$ does not vary by age, but that it does vary by year
in a fairly patterned way – $`cm`$ is 0.2 for the first three years,
0.15 for the next four years, and 0.10 for the next three years. Here,
the values for a column should be entered and then those values repeated
for the other columns (as the values are constant across columns). The
values for one column can either be entered directly (which is easy
enough here) or by combining the results of multiple uses of
[`rep()`](https://rdrr.io/r/base/rep.html) (based on the pattern for
$`cm`$ described above).

``` r
tmp <- c(0.2,0.2,0.2,0.15,0.15,0.15,0.15,0.1,0.1,0.1)   # either this or next line works
tmp <- c(rep(0.2,3),rep(0.15,4),rep(0.1,3))             #   don't need both
```

For longer vectors such as this it may be worthwhile to make sure you
have entered the required number of values.

``` r
length(tmp)==simyears    # Should be TRUE if simyears values entered
#> [1] TRUE
```

The values in this vector are now repeated for each age (i.e., `tmax`+1)
for use in [`matrix()`](https://rdrr.io/r/base/matrix.html). Note
`byrow=FALSE` here because the values should be placed by column first.

``` r
tmp <- rep(tmp,tmax+1)
cm <- matrix(tmp,byrow=FALSE,nrow=simyears,
             dimnames=list(paste0("yr_",1:simyears),paste0("age_",0:tmax)))
cm
#>       age_0 age_1 age_2 age_3 age_4 age_5
#> yr_1   0.20  0.20  0.20  0.20  0.20  0.20
#> yr_2   0.20  0.20  0.20  0.20  0.20  0.20
#> yr_3   0.20  0.20  0.20  0.20  0.20  0.20
#> yr_4   0.15  0.15  0.15  0.15  0.15  0.15
#> yr_5   0.15  0.15  0.15  0.15  0.15  0.15
#> yr_6   0.15  0.15  0.15  0.15  0.15  0.15
#> yr_7   0.15  0.15  0.15  0.15  0.15  0.15
#> yr_8   0.10  0.10  0.10  0.10  0.10  0.10
#> yr_9   0.10  0.10  0.10  0.10  0.10  0.10
#> yr_10  0.10  0.10  0.10  0.10  0.10  0.10
```

#### Example 4

Suppose that there is variability both by age and by year, but it is
still fairly patterned - first four years are the same, next four years
are the same, and last two years are the same. Here, $`cm`$ values *by
age* are entered separately for the three blocks of years for which
$`cm`$ does not change by age.

``` r
tmp1 <- c(0.1,0.20,0.20,0.15,0.15,0.10)
tmp2 <- c(0.1,0.15,0.15,0.15,0.15,0.15)
tmp3 <- c(0.1,0.15,0.15,0.10,0.10,0.10)
```

Each of these is then repeated by how many years are in the block where
those $`cm`$ values don’t vary by age.

``` r
tmp1 <- rep(tmp1,4)
tmp2 <- rep(tmp2,4)
tmp3 <- rep(tmp3,2)
```

Those three vectors are then combined together for
[`matrix()`](https://rdrr.io/r/base/matrix.html).

``` r
tmp <- c(tmp1,tmp2,tmp3)
cm <- matrix(tmp,byrow=TRUE,nrow=simyears,
             dimnames=list(paste0("yr_",1:simyears),paste0("age_",0:tmax)))
cm
#>       age_0 age_1 age_2 age_3 age_4 age_5
#> yr_1    0.1  0.20  0.20  0.15  0.15  0.10
#> yr_2    0.1  0.20  0.20  0.15  0.15  0.10
#> yr_3    0.1  0.20  0.20  0.15  0.15  0.10
#> yr_4    0.1  0.20  0.20  0.15  0.15  0.10
#> yr_5    0.1  0.15  0.15  0.15  0.15  0.15
#> yr_6    0.1  0.15  0.15  0.15  0.15  0.15
#> yr_7    0.1  0.15  0.15  0.15  0.15  0.15
#> yr_8    0.1  0.15  0.15  0.15  0.15  0.15
#> yr_9    0.1  0.15  0.15  0.10  0.10  0.10
#> yr_10   0.1  0.15  0.15  0.10  0.10  0.10
```

#### Example 5

While it is unlikely to have information at a resolution fine enough to
simulate different $`cm`$ for each age-year combination (with no
discernible pattern), it is possible to use such information in `rFAMS`.
Below values for each age-year combination are entered. Note that using
similar digits and a new line after each age when creating the vector
help with data entry as the entered vector will look similar to the
desired matrix.

``` r
vals <- c(0.00,0.10,0.15,0.15,0.15,0.10,
          0.05,0.10,0.15,0.10,0.10,0.10,
          0.05,0.15,0.20,0.15,0.15,0.05,
          0.05,0.20,0.15,0.15,0.10,0.10,
          0.05,0.10,0.15,0.10,0.10,0.10,
          0.05,0.15,0.20,0.15,0.15,0.05,
          0.05,0.10,0.15,0.10,0.10,0.10,
          0.05,0.20,0.15,0.15,0.10,0.10,
          0.05,0.20,0.15,0.15,0.10,0.10,
          0.05,0.15,0.20,0.15,0.15,0.05)
```

It is a good idea to make sure the required number of values have been
entered.

``` r
length(vals)==simyears*(tmax+1)
#> [1] TRUE
```

This vector is then given to
[`matrix()`](https://rdrr.io/r/base/matrix.html).

``` r
cm <- matrix(vals,byrow=TRUE,nrow=simyears,
             dimnames=list(paste0("yr_",1:simyears),paste0("age_",0:tmax)))
cm
#>       age_0 age_1 age_2 age_3 age_4 age_5
#> yr_1   0.00  0.10  0.15  0.15  0.15  0.10
#> yr_2   0.05  0.10  0.15  0.10  0.10  0.10
#> yr_3   0.05  0.15  0.20  0.15  0.15  0.05
#> yr_4   0.05  0.20  0.15  0.15  0.10  0.10
#> yr_5   0.05  0.10  0.15  0.10  0.10  0.10
#> yr_6   0.05  0.15  0.20  0.15  0.15  0.05
#> yr_7   0.05  0.10  0.15  0.10  0.10  0.10
#> yr_8   0.05  0.20  0.15  0.15  0.10  0.10
#> yr_9   0.05  0.20  0.15  0.15  0.10  0.10
#> yr_10  0.05  0.15  0.20  0.15  0.15  0.05
```

[^1]: Sixty combinations in this example, just for conditional natural
    mortality.

[^2]: Usually given in `tmax` as part of the life history parameters in
    `lhparms`.

[^3]: Or the number of columns could have been defined by `tmax`+1 with
    `ncol=tmax+1`.

[^4]: The row and column names are not required by
    [`matrix()`](https://rdrr.io/r/base/matrix.html) or `rFAMS`. They
    just make the result easier to interpret.
