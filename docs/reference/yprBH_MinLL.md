# Simulate expected yield using the Beverton-Holt Yield-per-Recruit model for a range of input parameters, including minimum length limits for harvest

Estimate yield using the Beverton-Holt Yield-per-Recruit (YPR) model
using ranges of values for conditional fishing mortality (`cf`),
conditional natural mortality (`cm`), and minimum length limits for
harvest (`minLL`).

## Usage

``` r
yprBH_MinLL(minLL, cf, cm, lhparms, loi = NULL, matchRicker = FALSE)
```

## Arguments

- minLL:

  A numeric vector of minimum length limits.

- cf:

  A numeric vector of conditional fishing mortality.

- cm:

  A numeric vector of conditional natural mortality.

- lhparms:

  A named vector or list that contains values for each `N0`, `tmax`,
  `Linf`, `K`, `t0`, `LWalpha`, and `LWbeta`. See
  [`makeLH`](https://fishr-core-team.github.io/rFAMS/reference/makeLH.md)
  for definitions of these life history parameters. Also see details.

- loi:

  A numeric vector for lengths of interest. Used to determine number of
  fish that reach desired lengths.

- matchRicker:

  A logical that indicates whether the yield function should match that
  in Ricker (1975). Defaults to `TRUE`. The only reason to changed to
  `FALSE` is to try to match output from FAMS. See the [FAMS vs Ricker
  article](https://fishr-core-team.github.io/rFAMS/articles/YPR_FAMSvRICKER.html).

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

- `nAtxxx` is the number that reach the length of interest supplied.
  There will be one column for each length of interest.

- `F` is the instantaneous rate of fishing mortality.

- `M` is the instantaneous rate of natural mortality.

- `Z` is the instantaneous rate of total mortality.

- `S` is the (total) annual rate of survival.

For convenience the data.frame also contains the model input values
(`minLL`, `cf`, and`cm` from input vectors; `N0`; `Linf`; `K`; `t0`;
`LWalpha`; `LWbeta`; and `tmax`).

The data.frame also contains a `notes` value which may contain
abbreviations for "issues" that occurred when computing the results and
were adjusted for. The possible abbreviates are defined under "values"
in the documentation for
[`yprBH_func`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_func.md).

## Details

Details will be filled out later

## See also

[`yprBH_func`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_func.md)
for estimating yield from single values of `cf`, `cm`, and `minLL` for
simulating yield with multiple values of `cf` and `cm` but a fixed value
for `minLL`.

See [this demonstration
page](https://fishr-core-team.github.io/rFAMS/articles/YPR_MinLL.html)
for more plotting examples

## Author

Jason C. Doll, <jason.doll@fmarion.edu>

## Examples

``` r
# Load other required packages for organizing output and plotting
library(dplyr)    ## for filter
library(ggplot2)  ## for ggplot et al.
library(metR)     ## geom_contour2

# Life history parameters to be used below
LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)

# Estimate yield for multiple values of minLL, cf, and cm
# # This is a minimal example, increments for minLL, cf, and cm would likely be smaller
# #   to produce finer-scaled results.

minLL <- seq(from = 200, to = 550, by = 50)
cf <- seq(from = 0.1, to = 0.9, by = 0.1)
cm <- seq(from = 0.1, to = 0.9, by = 0.1)
loi <- c(400,450,500,550)

Res_1 <- yprBH_MinLL(minLL = minLL, cf = cf, cm = cm,
                     lhparms=LH, loi=loi)

# Yield curves (yield vs exploitation) by varying minimum lengths,
# using cm=40
plot_dat <- Res_1 |> filter(cm==0.40)

ggplot(data=plot_dat,mapping=aes(y=yield,x=exploitation,
                                 group=minLL,color=minLL)) +
  geom_line(linewidth=1) +
  scale_color_gradient2(high="black") +
  xlab("Exploitation (u)")+
  ylab("Yield (g)")+
  labs(color="Min Length Limit") +
  theme_bw()


# Yield isopleths for varying minLL and exploitation with cm=0.40
# # Using same data as previous example
ggplot(data=plot_dat,mapping=aes(x=exploitation,y=minLL,z=yield)) +
  geom_contour2(aes(label = after_stat(level))) +
  xlab("Exploitation (u)") +
  ylab("Minimum length limit (mm)") +
  theme_bw()

```
