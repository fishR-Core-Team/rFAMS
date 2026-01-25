# Simulate expected yield using the Beverton-Holt Yield-per-Recruit model for single input parameters

Estimate yield using the Beverton-Holt Yield-per-Recruit (YPR) model.
This main function accepts only single values for conditional fishing
mortalitiy (`cf`), conditional natural mortality (`cm`), and a minimum
length limit for harvest (`minLL`).

## Usage

``` r
yprBH_func(minLL, cf, cm, loi = NA, lhparms, matchRicker = FALSE)
```

## Arguments

- minLL:

  A single numeric representing the minimum length limit for harvest in
  mm.

- cf:

  A single numeric representing conditional fishing mortality.

- cm:

  A single numeric representing conditional natural mortality.

- loi:

  A numeric vector for lengths of interest. Used to determine number of
  fish that reach desired lengths.

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

- `yield` is the estimated yield (in g).

- `exploitation` is the exploitation rate.

- `Nharvest` is the number of harvested fish.

- `Ndie` is the number of fish that die of natural deaths.

- `Nt` is the number of fish at time tr (time they become harvestable
  size).

- `avgwt` is the average weight of fish harvested.

- `avglen` is the average length of fish harvested.

- `tr` is the time for a fish to recruit to a minimum length limit
  (i.e., time to enter fishery).

- `F` is the instantaneous rate of fishing mortality.

- `M` is the instantaneous rate of natural mortality.

- `Z` is the instantaneous rate of total mortality.

- `S` is the (total) annual rate of survival.

- `N at xxx mm` is the number that reach the length of interest
  supplied. There will be one column for each length of interest.

For convenience the data.frame also contains the model input values
(`minLL`, `cf`, `cm`, `N0`, `Linf`, `K`, `t0`, `LWalpha`, `LWbeta`, and
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

- `Nt<0`: The number of fish recruiting to the fishery was less than 0.
  There cannot be negative fish, so Nt was then set to 0.

- `Nt>N0`: The number of fish recruiting to the fishery was more than
  the number of fish recruited to the populations. Fish cannot be added
  to the cohort, so Nt was set to N0.

- `Y=Infinite`: The calculated yield (Y) was inifinity, which is
  impossible and suggests some other propblem. Yield was set to NA.

- `Y<0`: The calculated yield (Y) was negative, which is impossible.
  Yield was set to 0.

- `Nharv<0`: The calculated number of fish harvested (Nharv) was
  negative, which is not possible. Number harvested was set to 0.

- `Nharv>Nt`: The calculated number of fish harvested (Nharv) was
  greater than the number of fish recruiting to the fishery, which is
  impossible. The number harvested was set to the number recruiting to
  the fishery.

- `Ndie<0`: The calculated number of fish recruiting to the fishery that
  died naturally (Ndie) was negative, which is not possible. Number that
  died was set to 0.

- `Ndie>Nt`: The calculated number of fish recruiting to the fishery
  that died naturally (Ndie) was greater than the number of fish
  recruiting to the fishery, which is impossible. The number that died
  was set to the number recruiting to the fishery.

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

[`yprBH_minLL_fixed`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_minLL_fixed.md)
and
[`yprBH_minLL_var`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_minLL_var.md)
for simulating yield with multiple values of `cf`, `cm`, and `minLL`.

## Author

Jason C. Doll, <jason.doll@fmarion.edu>

## Examples

``` r
#' # Life history parameters to be used below
LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)

# Estimate yield with fixed parameters
Res_1 <- yprBH_func(minLL=355,cf=0.45,cm=0.25,
                    loi=c(200,250,300,325,350),lhparms=LH)
Res_1
#>     yield         u Nharvest     Ndie    avgwt   avglen       Nt       tr
#> 1 19606.5 0.3966366  19.7239 9.491238 994.0479 402.5024 29.21514 4.277232
#>          F         M         Z      S   cf   cm minLL  N0 Linf   K   t0 LWalpha
#> 1 0.597837 0.2876821 0.8855191 0.4125 0.45 0.25   355 100  592 0.2 -0.3  -5.528
#>   LWbeta tmax notes N at 200 mm N at 250 mm N at 300 mm N at 325 mm N at 350 mm
#> 1  3.273   15           60.2497    49.51249    39.44369    34.67882    30.10579
```
