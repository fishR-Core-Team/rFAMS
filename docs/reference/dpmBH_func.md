# Simulate yield using the dynamic pool model.

Estimate yield-at-age using the Beverton-Holt Yield-per-Recruit (YPR)
model for a single year-class. This main function accepts a minimum
length limit for harvest (`minLL`), a vector for conditional fishing
mortality (`cf`), a vector of conditional natural mortality (`cm`), a
vector of recruitment abundance (`rec`), and life history parameters
(`lhparams`).

## Usage

``` r
dpmBH_func(minLL, cf, cm, rec, lhparms, matchRicker = FALSE)
```

## Arguments

- minLL:

  A single numeric representing the minimum length limit for harvest in
  mm.

- cf:

  A matrix of conditional fishing mortality where each row represents a
  year and each column represents age. Ages are age-0 through maximum
  age.

- cm:

  A matrix of conditional natural mortality where each row represents a
  year and each column represents age. Ages are age-0 through maximum
  age.

- rec:

  A numeric vector of length `simyears` to specify recruitment each
  year. The vector can be geneated using the
  [`genRecruits()`](https://fishr-core-team.github.io/rFAMS/reference/genRecruits.md)
  function.

- lhparms:

  A named vector or list that contains values for each `N0`, `tmax`,
  `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta`. See
  [`makeLH`](https://fishr-core-team.github.io/rFAMS/reference/makeLH.md)
  for definitions of these life history parameters. Also see details.

- matchRicker:

  A logical that indicates whether the yield function should match that
  in Ricker (). Defaults to `TRUE`. The only reason to changed to
  `FALSE` is to try to match output from FAMS. See the "YPR_FAMSvRICKER"
  article.

## Value

A data.frame with the following calculated values:

- `age` is the age of the year class

- `length` is the mean length at age calculated using the von
  Bertalanffy growth model and provided parameters

- `weight` is the mean weight at age calculated using the log10
  length-weight regression using the provided parameters

- `N_start` is the number of individuals at age at the start of the
  year.

- `exploitation` is the exploitation rate.

- `expect_nat_death` is the expectation of natural death.

- `cf` is the conditional fishing mortality at age.

- `cm` is the conditional natural mortality at age

- `F` is the instantaneous rate of fishing mortality.

- `M` is the instantaneous rate of natural mortality.

- `Z` is the instantaneous rate of total mortality.

- `S` is the (total) annual rate of survival

- `tr` is the time for a fish to recruit to a minimum length limit
  (i.e., time to enter fishery).

- `Nt` is the number of fish at time tr (time they become harvestable
  size).

- `biomass` is the total biomass at age (g)

- `N_harvest` is the total number harvested at age

- `N_die` is the total number that die at age

- `yield` is the estimated yield (in g).

For convenience the data.frame also contains the model input values
(`minLL`, `N0`, `N0`, `Linf`, `K`, `t0`, `LWalpha`, `LWbeta`, and
`tmax`).

The data.frame also contains a `notes` value which may contain
abbreviations for "issues" that occurred when computing the results and
were adjusted for. The possible abbreviates are as follows:

- `minLL>=Linf`: The minimum length limit (minLL) being explored was
  greater than the given asymptotic mean length (Linf). For the purpose
  (only) of computing the time at recruitment to the fishery (tr) the
  Linf was set to minLL+0.1.

- `tr<t0`: The age at recruitment to the fishery (tr) was less than the
  hypothetical time when the mean length is zero (t0). The fish can't
  recruit to the fishery prior to having length 0 so tr was set to t0.
  This also assures that the time it takes to recruit to the fishery is
  greater than 0.

- `Y=Infinite`: The calculated yield (Y) was inifinity, which is
  impossible and suggests some other propblem. Yield was set to NA.

- `Y<0`: The calculated yield (Y) was negative, which is impossible.
  Yield was set to 0.

- `Nharv<0`: The calculated number of fish harvested (Nharv) was
  negative, which is not possible. Number harvested was set to 0.

- `Ndie<0`: The calculated number of fish recruiting to the fishery that
  died naturally (Ndie) was negative, which is not possible. Number that
  died was set to 0.

- `agvglen<minLL`: The average length of harvested fish was less than
  the given minimum length limit being explored, which is not possible
  (with only legal harvest). The average length was set to the minimum
  length limit.

## Details

Details will be filled out later

## References

Ricker, W.E. 1975. Computation and interpretation of biological
statistics of fish populations. Technical Report Bulletin 191, Bulletin
of the Fisheries Research Board of Canada. Was (is?) from
<https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/1485.pdf>.

Slipke, J.W., and M.J. Maceina. 2014. Fishery analysis and modeling
simulator. v1.64. American Fisheries Society, Bethesda, MD.

## See also

[`yprBH_func`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_func.md)
for simulating yield using the dynamic pool model.

See [this demonstration
page](file:///C:/Users/jason.doll/Documents/fishRCoreTeam/rFAMS/docs/articles/dpmBH.md)
for more plotting examples

## Author

Jason C. Doll, <jason.doll@fmarion.edu>

## Examples

``` r
lhparms <- makeLH(N0=100,tmax=30,Linf=1349.5,K=0.111,t0=0.065,LWalpha=-5.2147,LWbeta=3.153)

# simulate yield from a single year-class
cm <- rep(0.18,(lhparms$tmax+1))
cf <- c(rep(0,3), rep(0.33,(lhparms$tmax+1) - 3))

Res_1 <- dpmBH_func(minLL=400,cm=cm,cf=cf,rec=1000,lhparms=lhparms,matchRicker=FALSE)

Res_1
#>    age    length      weight      N_start exploitation expect_nat_death   cf
#> 1    0    0.0000     0.00000 1.000000e+03    0.0000000        0.1800000 0.00
#> 2    1  133.0349    30.35037 8.200000e+02    0.0000000        0.1800000 0.00
#> 3    2  260.8383   253.58225 6.724000e+02    0.0000000        0.1800000 0.00
#> 4    3  375.2145   798.00101 5.513680e+02    0.3012967        0.1493033 0.33
#> 5    4  477.5741  1707.32249 3.324355e+02    0.3012967        0.1493033 0.33
#> 6    5  569.1797  2968.94019 1.826400e+02    0.3012967        0.1493033 0.33
#> 7    6  651.1612  4537.95096 1.003424e+02    0.3012967        0.1493033 0.33
#> 8    7  724.5295  6354.14055 5.512814e+01    0.3012967        0.1493033 0.33
#> 9    8  790.1897  8353.08648 3.028740e+01    0.3012967        0.1493033 0.33
#> 10   9  848.9515 10472.91968 1.663990e+01    0.3012967        0.1493033 0.33
#> 11  10  901.5398 12658.06651 9.141959e+00    0.3012967        0.1493033 0.33
#> 12  11  948.6030 14860.98066 5.022592e+00    0.3012967        0.1493033 0.33
#> 13  12  990.7218 17042.59566 2.759412e+00    0.3012967        0.1493033 0.33
#> 14  13 1028.4155 19172.00767 1.516021e+00    0.3012967        0.1493033 0.33
#> 15  14 1062.1490 21225.73407 8.329020e-01    0.3012967        0.1493033 0.33
#> 16  15 1092.3385 23186.77433 4.575963e-01    0.3012967        0.1493033 0.33
#> 17  16 1119.3562 25043.61681 2.514034e-01    0.3012967        0.1493033 0.33
#> 18  17 1143.5354 26789.27749 1.381210e-01    0.3012967        0.1493033 0.33
#> 19  18 1165.1743 28420.41856 7.588370e-02    0.3012967        0.1493033 0.33
#> 20  19 1184.5398 29936.56947 4.169051e-02    0.3012967        0.1493033 0.33
#> 21  20 1201.8707 31339.45745 2.290476e-02    0.3012967        0.1493033 0.33
#> 22  21 1217.3808 32632.44444 1.258388e-02    0.3012967        0.1493033 0.33
#> 23  22 1231.2614 33820.06270 6.913582e-03    0.3012967        0.1493033 0.33
#> 24  23 1243.6837 34907.63808 3.798322e-03    0.3012967        0.1493033 0.33
#> 25  24 1254.8009 35900.98951 2.086798e-03    0.3012967        0.1493033 0.33
#> 26  25 1264.7501 36806.19325 1.146487e-03    0.3012967        0.1493033 0.33
#> 27  26 1273.6540 37629.40137 6.298799e-04    0.3012967        0.1493033 0.33
#> 28  27 1281.6225 38376.70479 3.460560e-04    0.3012967        0.1493033 0.33
#> 29  28 1288.7538 39054.03278 1.901232e-04    0.3012967        0.1493033 0.33
#> 30  29 1295.1359 39667.08149 1.044537e-04    0.3012967        0.1493033 0.33
#> 31  30 1300.8474 40221.26576 5.738685e-05    0.3012967        0.1493033 0.33
#>      cm         F         M         Z      S      biomass    N_harvest
#> 1  0.18 0.0000000 0.1984509 0.1984509 0.8200 0.000000e+00 0.000000e+00
#> 2  0.18 0.0000000 0.1984509 0.1984509 0.8200 2.488730e+04 0.000000e+00
#> 3  0.18 0.0000000 0.1984509 0.1984509 0.8200 1.705087e+05 0.000000e+00
#> 4  0.18 0.4004776 0.1984509 0.5989285 0.5494 4.399922e+05 1.297908e+02
#> 5  0.18 0.4004776 0.1984509 0.5989285 0.5494 5.675745e+05 1.001617e+02
#> 6  0.18 0.4004776 0.1984509 0.5989285 0.5494 5.422474e+05 5.502884e+01
#> 7  0.18 0.4004776 0.1984509 0.5989285 0.5494 4.553491e+05 3.023285e+01
#> 8  0.18 0.4004776 0.1984509 0.5989285 0.5494 3.502919e+05 1.660993e+01
#> 9  0.18 0.4004776 0.1984509 0.5989285 0.5494 2.529933e+05 9.125493e+00
#> 10 0.18 0.4004776 0.1984509 0.5989285 0.5494 1.742683e+05 5.013546e+00
#> 11 0.18 0.4004776 0.1984509 0.5989285 0.5494 1.157195e+05 2.754442e+00
#> 12 0.18 0.4004776 0.1984509 0.5989285 0.5494 7.464065e+04 1.513291e+00
#> 13 0.18 0.4004776 0.1984509 0.5989285 0.5494 4.702755e+04 8.314018e-01
#> 14 0.18 0.4004776 0.1984509 0.5989285 0.5494 2.906517e+04 4.567722e-01
#> 15 0.18 0.4004776 0.1984509 0.5989285 0.5494 1.767896e+04 2.509506e-01
#> 16 0.18 0.4004776 0.1984509 0.5989285 0.5494 1.061018e+04 1.378723e-01
#> 17 0.18 0.4004776 0.1984509 0.5989285 0.5494 6.296051e+03 7.574703e-02
#> 18 0.18 0.4004776 0.1984509 0.5989285 0.5494 3.700163e+03 4.161542e-02
#> 19 0.18 0.4004776 0.1984509 0.5989285 0.5494 2.156647e+03 2.286351e-02
#> 20 0.18 0.4004776 0.1984509 0.5989285 0.5494 1.248071e+03 1.256121e-02
#> 21 0.18 0.4004776 0.1984509 0.5989285 0.5494 7.178229e+02 6.901130e-03
#> 22 0.18 0.4004776 0.1984509 0.5989285 0.5494 4.106427e+02 3.791481e-03
#> 23 0.18 0.4004776 0.1984509 0.5989285 0.5494 2.338178e+02 2.083040e-03
#> 24 0.18 0.4004776 0.1984509 0.5989285 0.5494 1.325905e+02 1.144422e-03
#> 25 0.18 0.4004776 0.1984509 0.5989285 0.5494 7.491812e+01 6.287454e-04
#> 26 0.18 0.4004776 0.1984509 0.5989285 0.5494 4.219782e+01 3.454327e-04
#> 27 0.18 0.4004776 0.1984509 0.5989285 0.5494 2.370200e+01 1.897807e-04
#> 28 0.18 0.4004776 0.1984509 0.5989285 0.5494 1.328049e+01 1.042655e-04
#> 29 0.18 0.4004776 0.1984509 0.5989285 0.5494 7.425077e+00 5.728349e-05
#> 30 0.18 0.4004776 0.1984509 0.5989285 0.5494 4.143372e+00 3.147155e-05
#> 31 0.18 0.4004776 0.1984509 0.5989285 0.5494 2.308172e+00 1.729047e-05
#>           N_die        yield minLL   N0   Linf     K    t0 LWalpha LWbeta tmax
#> 1  1.800000e+02 0.000000e+00   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 2  1.476000e+02 0.000000e+00   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 3  1.210320e+02 0.000000e+00   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 4  8.914171e+01 1.682142e+05   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 5  4.963371e+01 2.251468e+05   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 6  2.726876e+01 2.009899e+05   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 7  1.498146e+01 1.613851e+05   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 8  8.230812e+00 1.202871e+05   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 9  4.522008e+00 8.486876e+04   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 10 2.484391e+00 5.742378e+04   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 11 1.364925e+00 3.759943e+04   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 12 7.498895e-01 2.398064e+04   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 13 4.119893e-01 1.497108e+04   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 14 2.263469e-01 9.182960e+03   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 15 1.243550e-01 5.550320e+03   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 16 6.832064e-02 3.313353e+03   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 17 3.753536e-02 1.957245e+03   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 18 2.062193e-02 1.145817e+03   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 19 1.132969e-02 6.656198e+02   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 20 6.224530e-03 3.840927e+02   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 21 3.419757e-03 2.203582e+02   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 22 1.878814e-03 1.257856e+02   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 23 1.032221e-03 7.148564e+01   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 24 5.671020e-04 4.046972e+01   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 25 3.115658e-04 2.283335e+01   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 26 1.711743e-04 1.284440e+01   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 27 9.404314e-05 7.206358e+00   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 28 5.166730e-05 4.033755e+00   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 29 2.838601e-05 2.253260e+00   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 30 1.559528e-05 1.256386e+00   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#> 31 8.568045e-06 6.994151e-01   400 1000 1349.5 0.111 0.065 -5.2147  3.153   30
#>    notes
#> 1       
#> 2       
#> 3       
#> 4       
#> 5       
#> 6       
#> 7       
#> 8       
#> 9       
#> 10      
#> 11      
#> 12      
#> 13      
#> 14      
#> 15      
#> 16      
#> 17      
#> 18      
#> 19      
#> 20      
#> 21      
#> 22      
#> 23      
#> 24      
#> 25      
#> 26      
#> 27      
#> 28      
#> 29      
#> 30      
#> 31      
```
