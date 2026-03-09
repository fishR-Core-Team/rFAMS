# Simulate expected yield under minimum length regulations using the Dynamic Pool model for a range of input parameters

Simulate yield under minimum length regulations using the Dynamic Pool
(DPM) model with (possibly) multiple values for conditional fishing
mortality (`cf`) and conditional natural mortality (`cm`).

## Usage

``` r
dpmBH_MinLL(
  minLL,
  cf,
  cm,
  rec,
  lhparms,
  simyears,
  species = NULL,
  group = NULL,
  matchRicker = FALSE
)
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

- simyears:

  A single numeric for the number of years to simulate. Value must be a
  whole number greater than 1.

- species:

  A single character to specify the species used in the simulation. This
  will define the length for `stock`, `quality`, `preferred`,
  `memorable`, and `trophy` lengths from the FSA package. See the
  [`PSDlit`](https://fishr-core-team.github.io/FSA/reference/PSDlit.html)
  documentation.

- group:

  A single character to specify the sub-group name for `species` which
  may be required when defining the `stock`, `quality`, `preferred`,
  `memorable`, and `trophy` length categories from the FSA package. See
  the
  [`PSDlit`](https://fishr-core-team.github.io/FSA/reference/PSDlit.html)
  documentation.

- matchRicker:

  A logical that indicates whether the yield function should match that
  in Ricker (1975). Defaults to `FALSE`. See the [FAMS vs Ricker
  article](https://fishr-core-team.github.io/rFAMS/articles/YPR_FAMSvRICKER.html).

## Value

A list with two data.frame object. The first list item named `sumbyAge`
contains a data.frame with the following calculated values in a summary
by age:

- `year` is the year number for the simulation

- `yc`is the year class number for the simulation

- `age` is the age of fish from the year class

- `length` is the length-at-age at the beginning of the year based on
  parameters supplied for the von Bertlanffy growth model.

- `weight` is the total weight at the beginning of the year for
  length-at-age based on the parameters supplied for the weight-length
  model.

- `N_start` is the number of fish alive at the start of the year for the
  given age and year class.

- `exploitation` is the exploitation rate at age based on the supplied
  conditional fishing mortality rate.

- `expect_nat_death` is the expectation of natural death based on the
  supplied conditional natural mortality rate.

- `cf` is the supplied conditional fishing mortality rate.

- `cm` is the supplied conditional natural mortality rate.

- `F` is the instantaneous rate of fishing mortality.

- `M` is the instantaneous rate of natural mortality.

- `Z` is the instantaneous rate of total mortality.

- `S` is the (total) annual rate of survival.

- `biomass` is the total biomass of fish at age and year.

- `N_harvest` is the total number of fish harvested at age and year.

- `N_die` is the total number of fish that die at age and year.

- `yield` is the estimated yield (in g).

- `minLL` is the minimum length limit specified in the simulation.

For convenience the data.frame also contains the model input values
(`N0`, `Linf`, `K`, `t0`, `LWalpha`, `LWbeta`, and `tmax`).

The second list item named `sumbyYear` contains a data.frame with the
following calculated values in a summary by year:

- `year` is the year number for the simulation

- `Age_1plus` is the total number of fish age-1 plus per year.

- `Yield_Age_1plus` is the total year of age-1 plus fish per year.

- `Total_biomass` is the total biomass of age-1 plus fish per year.

- `N_harvest_Age_1plus` is the number of age-1 plus fish that are
  harvested per year.

- `N_die_Age_1plus` is the number of age-1 plus fish that die per year.

- `substock` is the number of substock sized fish at age and year at the
  beginning of the year.

- `stock` is the number of stock sized fish at age and year at the
  beginning of the year.

- `quality` is the number of quality sized fish at age and year at the
  beginning of the year.

- `preferred` is the number of preferred sized fish at age and year at
  the beginning of the year.

- `memorable` is the number of memorable sized fish at age and year at
  the beginning of the year.

- `trophy` is the number of trophy sized fish at age and year at the
  beginning of the year.

- `PSD` is the number of quality sized fish divided by the number of
  stock sized multiplied by 100.

- `PSD_P` is the number of preferred sized fish divided by the number of
  stock sized multiplied by 100.

- `PSD_M` is the number of memorable sized fish divided by the number of
  stock sized multiplied by 100.

- `PSD_T` is the number of trophy sized fish divided by the number of
  stock sized multiplied by 100.

PSD-X are calculated based on the number of fish in each category
(`stock`, `quality`, `preferred`, `memorable`, and `trophy`) at the
beginning of the year. That is, the length-at-age during the start of
the year is used to assign PSD-X categories at age. For example, if
Quality size is 300mm, an age-1 fish at 275mm at the start of the year
would not be counted as a quality-sized fish, but an age-2 fish at 325mm
at the start of the year would be counted as a quality-sized fish.

## Details

Details will be filled out later.

Note that the main calculations are in the internal `dpmBH_func` (use
`rFAMS:::dpmBH_func` to see that source code).

## See also

[`yprBH_MinLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_MinLL.md)
for estimating yield with a yield-per-recruit model using a minimum
length limit and
[`yprBH_SlotLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_SlotLL.md)
for estimating yield with the yield-per-recruit model and a slot limit.

See [this demonstration
page](https://fishr-core-team.github.io/rFAMS/articles/dpmBH.html) for
more examples of this function.

## Author

Jason C. Doll, <jason.doll@fmarion.edu>

## Examples

``` r
#load required library
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(ggplot2)

# Example of simulating yield with the dynamic pool model,

lhparms <- makeLH(N0=100,tmax=30,Linf=1349.5,K=0.111,t0=0.065,
            LWalpha=-5.2147,LWbeta=3.153)
simyears <- 50
minLL <- 400
rec <- genRecruits(method = "fixed", nR = 100, simyears = simyears)
cm <- matrix(rep(c(rep(0,1), rep(0.18,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)
cf <- matrix(rep(c(rep(0,1), rep(0.33,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)

out<-dpmBH_MinLL(simyears = simyears, minLL = minLL, cf = cf,
                 cm = cm, rec = rec, lhparms = lhparms,
                 matchRicker=FALSE,species="Striped Bass",group="landlocked")

#Use summary by year data frame to plot yield vs year
ggplot(data=out[[2]],mapping=aes(x=year,y=Yield_age_1plus)) +
  geom_point() +
  geom_line() +
  labs(y="Total yield (g)",x="Year") +
  theme_bw()


#Plot date using summary by age
#filter for year class = 1
plotdat<- out[[1]] |> filter(yc==1)
#Plot yield vs age
ggplot(data=plotdat,mapping=aes(x=age,y=yield)) +
  geom_point() +
  geom_line() +
  labs(y="Total yield (g)",x="Age") +
  theme_bw()


#Recruitment based on a normal distribution
rec <- genRecruits(method = "normal", simyears = simyears,
                   meanR = 1000, sdR = 500, minR = 100, maxR =2500)
cm <- matrix(rep(c(rep(0,1), rep(0.18,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)
cf <- matrix(rep(c(rep(0,1), rep(0.33,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)

out_2<-dpmBH_MinLL(minLL = minLL, cf = cf, cm = cm,
                   rec = rec, lhparms = lhparms,simyears = simyears,
                   species="Striped Bass",group="landlocked",matchRicker=FALSE)

#Use summary by year data frame to plot yield vs year
ggplot(data=out_2[[2]],mapping=aes(x=year,y=PSD)) +
  geom_point() +
  geom_line() +
  labs(y="PSD",x="Year") +
  theme_bw()


#Plot date using summary by age
#Plot yield vs age for each year class
ggplot(data=out_2[[1]],mapping=aes(x=age,y=yield,group=yc,color=yc)) +
  geom_point() +
  geom_line() +
  labs(y="Total yield (g)",x="Age") +
  theme_bw()

```
