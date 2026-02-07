# Function to simulate expected yield using the Beverton-Holt Yield Per Recruit model for single input parameters

Function to estimate yield using the Beverton-Holt YPR model. This main
function accepts only single values for cf, cm, and minlength. Use the
wrapper ypr() function for specifying range of cf, cm, and minlength

## Usage

``` r
yprBH_slot_func(
  recruitmentTL,
  lowerSL,
  upperSL,
  cfunder,
  cfin,
  cfabove,
  cm,
  loi = NULL,
  lhparms,
  matchRicker = FALSE
)
```

## Arguments

- recruitmentTL:

  A numeric representing the minimum length limit for recruiting to the
  fishery in mm.

- lowerSL:

  A numeric representing the length of the lower slot limit in mm.

- upperSL:

  A numeric representing the length of the upper slot limit in mm.

- cfunder:

  Single value, conditional fishing mortality under the lower slot
  limit.

- cfin:

  Single value, conditional fishing mortality within the lower and upper
  slot limit.

- cfabove:

  Single value, conditional fishing mortality over the upper slot limit.

- cm:

  A numeric representing conditional natural mortality

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

the following calculated and input values in a data.frame

- yieldTotal is the calculated total yield

- yieldUnder is the calculated yield under the slot limit

- yieldIn is the calculated yield within the slot limit

- yieldAbove is the calculated yield above the slot limit

- nharvTotal is the calculated total number of harvested fish

- ndieTotal is the calculated total number of fish that die of natural
  death

- nharvestUnder is the number of harvested fish under the slot limit

- nharvestIn is the number of harvested fish within the slot limit

- nharvestAbove is the number of harvested fish above the slot limit

- n0die is the number of fish that die of natural death before entering
  the fishery at a minimum length

- ndieUnder is the number of fish that die of natural death between
  entering the fishery and the lower slot limit

- ndieIn is the number of fish that die of natural deaths within the
  slot limit

- ndieAbove is the number of fish that die of natural deaths above the
  slot limit

- nrUnder is the number of fish at time trUnder (time they become
  harvestable size under the slot limit)

- nrIn is the number of fish at time trIn (time they reach the lower
  slot limit size)

- nrAbove is the number of fish at time trAbove (time they reach the
  upper slot limit size)

- trUnder is the time for a fish to recruit to a minimum length limit
  (i.e., time to enter fishery)

- trIn is the time for a fish to recruit to a lower length limit of the
  slot limit

- trOver is the time for a fish to recruit to a upper length limit of
  the slot limit

- avglenUnder is the average length of fish harvested under the slot
  limit

- avglenIn is the average length of fish harvested within the slot limit

- avglenAbove is the average length of fish harvested above the slot
  limit

- avgwtUnder is the average weight of fish harvested under the slot
  limit

- avgwtIn is the average weight of fish harvested within the slot limit

- avgwtAbove is the average weight of fish harvested above the slot
  limit

- `nAtxxx` is the number that reach the length of interest supplied.
  There will be one column for each length of interest.

- cm A numeric representing conditional natural mortality

- expUnder is the exploitation rate under the slot limit

- expIn is the exploitation rate within the slot limit

- expAbove is the exploitation rate above the slot limit

- FUnder is the estimated instantaneous rate of fishing mortality under
  the slot limit

- FIn is the estimated instantaneous rate of fishing mortality within
  the slot limit

- FAbove is the estimated instantaneous rate of fishing mortality above
  the slot limit

- MUnder is the estimated instantaneous rate of natural mortality under
  the slot limit

- MIn is the estimated instantaneous rate of natural mortality within
  the slot limit

- MAbove is the estimated instantaneous rate of natural mortality above
  the slot limit

- ZUnder is the estimated instantaneous rate of total mortality under
  the slot limit

- ZIn is the estimated instantaneous rate of total mortality within the
  slot limit

- ZAbove is the estimated instantaneous rate of total mortality above
  the slot limit

- SUnder is the estimated total survival under the slot limit

- SIn is the estimated total survival within the slot limit

- SAbove is the estimated total survival above the slot limit

- cfUnder A numeric representing conditional fishing mortality

- cfIn A numeric representing conditional fishing mortality

- cfOver A numeric representing conditional fishing mortality

- recruitmentTL A numeric representing the minimum length limit for
  recruiting to the fishery in mm.

- lowerSL A numeric representing the length of the lower slot limit in
  mm.

- upperSL A numeric representing the length of the upper slot limit in
  mm.

- N0 A numeric representing the initial number of new recruits entering
  the fishery OR a vector or list that contains named values for each
  `N0`, `Linf`, `K`, `t0`, `LWalpha`, `LWbeta`, and `tmax`

- Linf A numeric representing the point estimate of the asymptotic mean
  length (L-infinity) from the von Bertalanffy growth model in mm

- K A numeric representing the point estimate of the Brody growth
  coefficient from the von Bertalanffy growth model

- t0 A numeric representing the point estimate of the x-intercept (i.e.,
  theoretical age at a mean length of 0) from the von Bertalanffy growth
  model

- LWalpha A numeric representing the point estimate of alpha from the
  length-weight regression on the log10 scale.

- LWbeta A numeric representing the point estimate of beta from the
  length-weight regression on the log10 scale.

- tmax An integer representing maximum age in the population in years
  \#'

## Details

Details will be filled out later

## Author

Jason C. Doll, <jason.doll@fmarion.edu>

## Examples

``` r
# Life history parameters to be used below
LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)

# Estimate yield with fixed parameters
Res_1 <- yprBH_slot_func(recruitmentTL=200,lowerSL=250,upperSL=325,
                       cfunder=0.25,cfin=0.6,cfabove=0.15,cm=0.4,
                       loi=c(200,250,300,325,350),lhparms=LH)
Res_1
#>   yieldTotal yieldUnder  yieldIn yieldAbove nharvTotal ndieTotal nharvestUnder
#> 1   5611.796   903.2554 3813.586   894.9547   19.68287   20.9852      6.154504
#>   nharvestIn nharvestAbove    n0die ndieUnder   ndieIn ndieAbove  nrUnder
#> 1   12.55573     0.9726349 59.32995  10.92831 6.999731  3.057163 40.67005
#>       nrIn  nrAbove  trUnder     trIn  trOver avglenUnder avglenIn avglenAbove
#> 1 23.58723 4.031773 1.761224 2.443479 3.68129    224.3558 280.1859    393.1118
#>   avgwtUnder  avgwtIn avgwtAbove   nAt200   nAt250   nAt300   nAt325   nAt350
#> 1   146.7633 303.7327   920.1343 40.67005 23.58723 7.636027 4.031773 2.895681
#>    cm  expUnder     expIn  expAbove    FUnder       FIn    FAbove    MUnder
#> 1 0.4 0.1981511 0.4879637 0.1182668 0.2876821 0.9162907 0.1625189 0.5108256
#>         MIn    MAbove    ZUnder      ZIn    ZAbove SUnder  SIn SAbove cfUnder
#> 1 0.5108256 0.5108256 0.7985077 1.427116 0.6733446   0.45 0.24   0.51    0.25
#>   cfIn cfOver recruitmentTL lowerSL upperSL  N0 Linf   K   t0 LWalpha LWbeta
#> 1  0.6   0.15           200     250     325 100  592 0.2 -0.3  -5.528  3.273
#>   tmax
#> 1   15

```
