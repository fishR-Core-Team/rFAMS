# Main function to simulate expected yield using the Beverton-Holt Yield-per-Recruit model for a range of input parameters

Estimate yield using the Beverton-Holt Yield-per-Recruit (YPR) model
using a range of values for conditional fishing (`cf`) and natural
(`cm`) mortality and a single fixed minimum length limit for harvest
(`minLL`).

## Usage

``` r
yprBH_minLL_fixed(
  minLL,
  cfmin,
  cfmax,
  cfinc,
  cmmin,
  cmmax,
  cminc,
  loi = NA,
  lhparms,
  matchRicker = FALSE
)
```

## Arguments

- minLL:

  The minimum length limit for harvest in mm

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

- `F` is the instantaneous rate of fishing mortality.

- `M` is the instantaneous rate of natural mortality.

- `Z` is the instantaneous rate of total mortality.

- `S` is the (total) annual rate of survival.

- `N at xxx mm` is the number that reach the length of interest
  supplied. There will be one column for each length of interest.

For convenience the data.frame also contains the model input values
(`minLL`; `cf` derived from `cfmin`, `cfmax`, and `cfinc`; `cm` derived
from `cmmin`, `cmmax`, and `cminc`; `N0`; `Linf`; `K`; `t0`; `LWalpha`;
`LWbeta`; and `tmax`).

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
[`yprBH_minLL_var`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_minLL_var.md)
for simulating yield with multiple values of `cf`, `cm`, and `minLL`.

## Author

Jason C. Doll, <jason.doll@fmarion.edu>

## Examples

``` r
# Life history parameters to be used below
LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)

# Estimate yield for multiple values of minLL, cf, and cm
# # This is a minimal example, lengthinc, cfinc, cminc would likely be smaller
# #   to produce finer-scaled results
Res_1 <- yprBH_minLL_fixed(minLL=200,
                         cfmin=0.1,cfmax=0.9,cfinc=0.1,
                         cmmin=0.1,cmmax=0.9,cminc=0.1,
                         loi=c(200,250,300,350),lhparms=LH)

# Load other required packages for organizing output and plotting
library(dplyr)    ## for filter
library(ggplot2)  ## for ggplot et al.
library(tidyr)    ## for pivot_longer

# Custom theme for plots (to make look nice)
theme_FAMS <- function(...) {
  theme_bw() +
  theme(
    panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
    axis.text=element_text(size=14,color="black"),
    axis.title=element_text(size=16,color="black"),
    axis.title.y=element_text(angle=90),
    axis.line=element_line(color="black"),
    panel.border=element_blank()
  )
}

# Yield curve (yield vs exploitation)
# Extract results for cm=0.40
plot_dat <- Res_1 |> dplyr::filter(cm==0.40)

ggplot(data=plot_dat,mapping=aes(x=u,y=yield)) +
  geom_point() +
  geom_line() +
  labs(y="Yield (g)",x="Exploitation (u)") +
  theme_FAMS()


# Plot number of fish reaching 300 mm as a function of exploitation with cm = 0.40
ggplot(data=plot_dat,mapping=aes(x=u,y=`N at 300 mm`)) +
  geom_point() +
  geom_line() +
  labs(y="Number of fish at 300 mm",x="Exploitation (u)") +
  theme_FAMS()


# Plot number of fish reaching multiple monitored lengths as a
# function of exploitation with cm = 0.40
# Select columns for plotting and convert to long
plot_data_long <- plot_dat %>%
  select(u,`N at 200 mm`, `N at 250 mm`, `N at 300 mm`, `N at 350 mm`) %>%
  pivot_longer(!u, names_to="loi",values_to="number")

# Generate plot
ggplot(data=plot_data_long,mapping=aes(x=u,y=number,group=loi,color=loi)) +
  geom_point() +
  scale_color_discrete(name="Yield",labels=c("N at 200 mm",
                       "N at 250 mm", "N at 300 mm", "N at 350 mm"))+
  geom_line() +
  labs(y="Number of fish",x="Exploitation (u)") +
  theme_FAMS() +
  theme(legend.position = "top")+
  guides(color=guide_legend(title="Length of interest"))

```
