# Main function to simulate expected yield using the Beverton-Holt Yield-per-Recruit model for a range of input parameters, including minimum length limits for harvest

Estimate yield using the Beverton-Holt Yield-per-Recruit (YPR) model
using ranges of values for conditional fishing mortality (`cf`),
conditional natural mortality (`cm`), and minimum length limits for
harvest (`minLL`).

## Usage

``` r
yprBH_minLL_var(
  lengthmin,
  lengthmax,
  lengthinc,
  cfmin,
  cfmax,
  cfinc,
  cmmin,
  cmmax,
  cminc,
  loi = NULL,
  lhparms,
  matchRicker = FALSE
)
```

## Arguments

- lengthmin:

  A single numeric for the lower limit of minimum length limit for
  harvest in mm.

- lengthmax:

  A single numeric for the upper limit of minimum length limit for
  harvest in mm.

- lengthinc:

  A single numeric for the increment to cycle from lower to upper
  minimum length limit for harvest in mm.

- cfmin:

  A single numeric for minimum conditional fishing mortality.

- cfmax:

  A single numeric for maximum conditional fishing mortality.

- cfinc:

  A single numeric for increment to cycle from minimum to maximum
  conditional fishing mortality.

- cmmin:

  A single numeric for minimum conditional natural mortality.

- cmmax:

  A single numeric for maximum conditional natural mortality.

- cminc:

  A single numeric for increment to cycle from minimum to maximum
  conditional natural mortality.

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

- `N at xxx mm` is the number that reach the length of interest
  supplied. There will be one column for each length of interest.

- `F` is the instantaneous rate of fishing mortality.

- `M` is the instantaneous rate of natural mortality.

- `Z` is the instantaneous rate of total mortality.

- `S` is the (total) annual rate of survival.

For convenience the data.frame also contains the model input values
(`minLL` derived from `lengthmin`, `lengthmax`, and `lengthinc`; `cf`
derived from `cfmin`, `cfmax`, and `cfinc`; `cm` derived from `cmmin`,
`cmmax`, and `cminc`; `N0`; `Linf`; `K`; `t0`; `LWalpha`; `LWbeta`; and
`tmax`).

The data.frame also contains a `notes` value which may contain
abbreviations for "issues" that occurred when computing the results and
were adjusted for. The possible abbreviates are defined under "values"
in the documentation for
[`yprBH_func`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_func.md).

## Details

Details will be filled out later

## See also

[`yprBH_func`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_func.md)
for estimating yield from single values of `cf`, `cm`, and `minLL`, and
[`yprBH_minLL_fixed`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_minLL_fixed.md)
for simulating yield with multiple values of `cf` and `cm` but a fixed
value for `minLL`.

See [this demonstration
page](https://fishr-core-team.github.io/rFAMS/articles/YPR_VarMLL.html)
for more plotting examples

## Author

Jason C. Doll, <jason.doll@fmarion.edu>

## Examples

``` r
# Load other required packages for organizing output and plotting
library(dplyr)    ## for filter
library(ggplot2)  ## for ggplot et al.
library(metR)     ## geom_text_contour

# Life history parameters to be used below
LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)

# Estimate yield for multiple values of minLL, cf, and cm
# # This is a minimal example, lengthinc, cfinc, cminc would likely be smaller
# #   to produce finer-scaled results.
Res_1 <- yprBH_minLL_var(lengthmin=200,lengthinc=50,lengthmax=550,
                       cfmin=0.1,cfmax=0.9,cfinc=0.1,
                       cmmin=0.1,cmmax=0.9,cminc=0.1,
                       loi=c(400,450,500,550),lhparms=LH)

# Yield curves (yield vs exploitation) by varying minimum lengths,
# using cm=40
plot_dat <- Res_1 |> filter(cm==0.40)

ggplot(data=plot_dat,mapping=aes(y=yield,x=exploitation,
                                 group=minLL,color=minLL)) +
  geom_line(linewidth=1) +
  scale_color_gradient2(high="black") +
  labs(y="Yield (g)",x="Exploitation (u)",color="Min Length Limit") +
  theme_bw()


# Yield isopleths for varying minLL and exploitation with cm=0.40
# # Using same data as previous example
ggplot(data=plot_dat,mapping=aes(x=exploitation,y=minLL,z=yield)) +
  geom_contour2(aes(label = after_stat(level))) +
  xlab("Exploitation (u)") +
  ylab("Minimum length limit (mm)") +
  theme_bw()


```
