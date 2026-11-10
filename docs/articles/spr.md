# Spawning Potential Ratio Calculations in rFAMS

## Overview

Spawning Potential Ratio (SPR) is the proportion of reproductive output
produced by an exploited population relative to the reproductive output
expected from an unfished population (Goodyear 1993). SPR provides a
biological measure of the effect of fishing mortality on the
reproductive capacity of a stock and is commonly used as a biological
reference point in fisheries management. Once a target SPR benchmark has
been selected, managers can estimate the fishing mortality rate required
to achieve that benchmark. Target SPR values are often species-specific;
however, a threshold of SPR = 0.40 (40%) has been adopted for several
groundfish stocks in the northeastern United States (NEFSC 2008).
Because SPR is directly related to the reproductive capacity of a
population, it is commonly used to evaluate the risk of recruitment
overfishing. Management measures such as minimum length limits, slot
length limits, and harvest restrictions can be used to achieve desired
SPR benchmarks by protecting sexually mature individuals and preserving
spawning output.

Although SPR has historically been used extensively in marine fisheries
management (Cooper et al. 2013; Goethel et al. 2018; Mu et al. 2021;
Willems et al. 2023; Guo et al. 2025), applications in freshwater
fisheries have become increasingly common. For example, LaBrie et
al. (2025) used SPR to develop harvest commendations for Blue Sucker
*Cycleptus elongatus*, a species not traditionally managed as a sport
fish. Similarly, Slipke, et al. (2002) proposed a target SPR of 10-20%
for Channel Catfish *Ictalurus punctatus* for the Upper Mississippi
River commercial fishery. Kern et al. (2020) estimated SPR for Shoal
Bass *Micropterus cataractae* and concluded that the population was
experiencing recruitment overfishing under existing regulations.

The **rFAMS** package implements two related measures of spawning
potential:

1.  **Static SPR (SPR)**, calculated within the yield-per-recruit (YPR)
    framework. This metric compares expected lifetime reproductive
    output under fishing mortality with expected lifetime reproductive
    output in an unfished population.

2.  **Weighted Transitional SPR (wtSPR)**, calculated within the dynamic
    pool model. This metric measures annual spawning potential during
    the transition from one population state to another and accounts for
    changes in age structure through time.

## Static Spawning Potential Ratio

### Conceptual Framework

The yield-per-recruit model follows a single recruit throughout its
lifetime and estimates cumulative reproductive output under specified
mortality schedules. Reproductive output is calculated both with and
without fishing mortality. The calculation of SPR requires length at age
(derived from the von Bertalanfffy growth model), fecundity at length
(based on a linear or logarithmic length-fecundity model), percentage of
the proportion that are female, proportion of the females that are
spawning, fishing, and natural mortality.

The spawning potential ratio is defined as

``` math
SPR=\frac{P_{fished}}{P_{unfished}}
```

where:

- $`P_{fished}`$ is the total lifetime spawning potential under the
  specified fishing mortality schedule.
- $`P_{unfished}`$ is the total lifetime spawning potential in the
  absence of fishing mortality.

### Length-at-Age

Length at age is predicted using the von Bertalanffy Growth Function:

``` math
L_t=L_{\infty}\left(1-e^{-K(t-t_0)}\right)
```

where:

- $`L_t`$ = expected length at age $`t`$
- $`L_{\infty}`$ = asymptotic length
- $`K`$ = Brody growth coefficient
- $`t_0`$ = theoretical age at which length equals zero

### Age-Specific Fecundity

Age-specific fecundity is estimated from fish length using a
user-specified fecundity-length relationship. Two model forms are
supported.

For a linear relationship:

``` math
 Fec_t=\beta_0 + \beta_1 L_t 
```

where:

- $`Fec_t`$ = fecundity at age $`t`$
- $`L_t`$ = mean length at age $`t`$
- $`\beta_0`$ = intercept of the fecundity-length relationship
- $`\beta_1`$ = slope of the fecundity-length relationship

Alternatively, fecundity can be estimated using a log-transformed
relationship.

``` math
 Fec_t=exp(\beta_0 + \beta_1 ln(L_t)) 
```

where $`\beta_0`$ and $`\beta_1`$ are estimated from the fitted
log-linear fecundity model.

Fish younger than the age at maturity are assumed to make no
contribution to reproduction. Thus, $`Fec_t = 0`$ for $`t<T_{mat}`$.

Expected reproductive output at age is then adjusted for sex ratio and
spawning participation because the total number of individuals in the
cohort is constant and not included in the calculation:

``` math
E_t = Fec_t \times pF_t \times pSpawn_t
```

where:

- $`E_t`$ = expected egg production at age $`t`$
- $`pF_t`$ = proportion female at age $`t`$
- $`pSpawn_t`$ = proportion of females that spawn at age $`t`$

### Mortality

Total instantaneous mortality at age is

``` math
Z_t = F_t + M_t
```

where:

- $`F_t`$ = instantaneous fishing mortality
- $`M_t`$ = instantaneous natural mortality

The model allows fishing and natural mortality to vary among length
intervals and management regulations.

### Survival

Survival over a time interval $`\Delta t`$ in the absence of fishing is

``` math
S_{noexp,t}=e^{-M_t\Delta t}
```

and survival under fishing is

``` math
S_{exp,t}=e^{-Z_t\Delta t}
```

where $`\Delta t`$ is the chosen age interval.

Cumulative survival to age $`t`$ is

``` math
CS_{noexp,t}=\prod_{i=1}^{t} S_{noexp,i}
```

for the unfished population and

``` math
CS_{exp,t}=\prod_{i=1}^{t} S_{exp,i}
```

for the fished population.

### Spawning Potential

Age-specific spawning potential in the absence of fishing is

``` math
P_{noexp,t}=E_t \times CS_{noexp,t}
```

and age-specific spawning potential under fishing is

``` math
P_{exp,t}=E_t \times CS_{exp,t}
```

Total lifetime spawning potential is obtained by summing across ages:

``` math
P_{unfished}=\sum_t P_{noexp,t}
```

and

``` math
P_{fished}=\sum_t P_{exp,t}
```

The spawning potential ratio is then

``` math
SPR=\frac{P_{fished}}{P_{unfished}}
```

Values range from 0 to 1.

- SPR = 1 indicates no reduction in lifetime reproductive output.
- SPR = 0.40 indicates that reproductive output has been reduced to 40%
  of unfished levels.
- SPR approaching 0 indicates severe depletion of spawning capacity.

## Weighted Transitional Spawning Potential Ratio

### Conceptual Framework

The dynamic pool model explicitly tracks changes in population abundance
and age structure through time. Consequently, a single equilibrium SPR
is insufficient to describe spawning potential during periods when
populations are transitioning toward a new equilibrium.

To characterize reproductive capacity during these transition periods,
rFAMS calculates a weighted transitional spawning potential ratio
(wtSPR) for each simulation year. This metric compares annual egg
production under the simulated fishing regime with the egg production
expected if those same fish experienced only natural mortality.

### Age-Specific Fecundity

Age-specific fecundity is estimated from fish length using a
user-specified fecundity-length relationship in the same manner as with
the static SPR. Fish younger than the age at maturity are assumed to
have zero fecundity. The resulting age-specific fecundity values are
combined with age-specific sex composition, spawning proportion, and
survivorship to estimate annual egg production.

### Survivorship

Cohort cumulative survivorship under natural mortality is

``` math
S_{noexp,t}
=
\prod_{i=1}^{t}
e^{-M_i}
```

and cumulative survivorship under total mortality is

``` math
S_{exp,t}
=
\prod_{i=1}^{t}
e^{-Z_i}
```

where

``` math
Z_i = M_i + F_i
```

### Egg Production

Expected egg production under natural mortality is

``` math
Eggs_{noexp,t}
=
N_{0,t}
\times pF_t
\times pSpawn_t
\times Fec_t
\times S_{noexp,t}
```

Expected egg production under fishing mortality is

``` math
Eggs_{exp,t}
=
N_{0,t}
\times pF_t
\times pSpawn_t
\times Fec_t
\times S_{exp,t}
```

where:

- $`N_{0,t}`$ = number of fish entering age class $`t`$
- $`Fec_t`$ = fecundity at age $`t`$
- $`pF_t`$ = proportion female at age $`t`$
- $`pSpawn_t`$ = proportion of females that spawn at age $`t`$
- $`S_{noexp,t}`$ = cumulative survival under natural mortality
- $`S_{exp,t}`$ = cumulative survival under total mortality

### Annual Weighted Transitional SPR

For each simulation year, egg production is summed across all age
classes:

``` math
TE_{noexp}
=
\sum_t Eggs_{noexp,t}
```

and

``` math
TE_{exp}
=
\sum_t Eggs_{exp,t}
```

where:

- $`TE_{noexp}`$ = total annual egg production expected under natural
  mortality only.
- $`TE_{exp}`$ = total annual egg production under the specified fishing
  regime.

The weighted transitional spawning potential ratio is then

``` math
wtSPR=
\frac{TE_{exp}}
{TE_{noexp}}
```

and is calculated separately for every simulated year.

## Interpretation

Although both metrics quantify spawning potential relative to an
unfished population, they represent different biological perspectives.

### Static SPR

Static SPR is an equilibrium measure derived from the yield-per-recruit
model. It represents the expected lifetime reproductive contribution of
a recruit under a specified mortality schedule and management
regulation.

### Weighted Transitional SPR

Weighted transitional SPR is an annual population-level measure derived
from the dynamic pool model. It reflects realized reproductive output
during periods when population age structure is changing following
implementation of a management action.

As the population approaches a stable age distribution under constant
mortality conditions, wtSPR is expected to converge toward the
equilibrium SPR value estimated from the yield-per-recruit model.

## Summary

Both SPR metrics quantify the proportion of spawning output retained
relative to an unfished population:

``` math
SPR \; \text{or} \; wtSPR
=
\frac{\text{Spawning Output Under Fishing}}
{\text{Spawning Output Without Fishing}}
```

For example, a value of 0.35 indicates that the population is expected
to produce 35% of the reproductive output that would occur in the
absence of fishing mortality.

Static SPR estimates equilibrium reproductive potential at the recruit
level, whereas weighted transitional SPR measures annual population-wide
reproductive potential during transitions between population states.

## Important note

Both the static and weighted transitional SPR calculated in rFAMS do not
match exactly match FAMS. Every effort was made to replicate the output,
however, discrepancies between the equations used in FAMS documentation
and Parkes (2001) and Goodyear (1993) produced challenges. In addition,
the closed source of FAMS prohibits the authors from reviewing the exact
methods that were used. We have provided a full description above on how
rFAMS calculates SPR and the source code can be viewed on GitHub.

## Literature Cited

Cooper, W.T., Barbieri, L.R., Murphy, M.D., and Lowerre-Barbieri, S.K.
(2013). Assessing stock reproductive potential in species with
indeterminate fecundity: Effects of age truncation and size-dependent
reproductive timing. Fisheries Research. 138:31-41.
<https://doi.org/10.1016/j.fishres.2012.05.016>

Goethel, D.R., Smith, M.W., Cass-Calay, S.L., and Porch, C.E. (2018).
Establishing stock status determination criteria for, fisheries with
high discards and uncertain recruitment. North American Journal of
Fisheries Management. 38(1):120-139.
<https://doi.org/10.1002/nafm.10007>

Goodyear, C.P. (1993). Spawning stock biomass per recruit in fisheries
management: foundation and current use. Pages 67-71 in S.J. Smith, J.J.
Hunt, and D. Rivard, editors. Risk evaluation and biological reference
points for fisheries management. Canadian Journal of Fisheries and
Aquatic Sciences Special Publication 120.

Guo, R., Han, Q. and Fang, Z. (2025). Stock assessment of *Uroteuthis
edulis* in the East China Sea based on spawning potential ratio.
Fisheries Science. 91:679–689.
<https://doi.org/10.1007/s12562-025-01875-5>

Kern, A.I., Sammons, S.M., and Ingram, T.R. (2020). Fecundity and
spawning potential ratio of shoal bass *Micropterus cataractae* in the
lower Flint River, Georgia, USA. Fisheries Research. 231:105687.
<https://doi.org/10.1016/j.fishres.2020.105687>

LaBrie, L.A.P., Carlson, T.L., Wesner, J.S., Pasbrig, C.A., Chipps,
S.R., and Schall, B.J. 2025. Reproductive characteristics and spawning
potential ratio modeling of vulnerable riverine specialist in the lower
unchannelized Missouri River, USA. Fisheries Research. 286:107386.
<https://doi.org/10.1016/j.fishres.2025.107386>

Mu, X., Zhang, C., Xu, B., Ji, Y., Xue, Y., and Ren, Y. (2021).
Accounting for the fish condition in assessing the reproductivity of a
marine eel to achieve fishery sustainability. Ecological Indicators.
130:108116. <https://doi.org/10.1016/j.ecolind.2021.108116>

NEFSC (Northeast Fisheries Science Center). 2008. Assessment of 19
Northeast Groundfish Stocks through 2007: Report of the 3rd Groundfish
Assessment Review Meeting (GARM III), Northeast Fisheries Science
Center, Woods Hole, Massachusetts, August 4–8, 2008, pg. 884, US
Department of Commerce, NOAA Fisheries, Northeast Fisheries Science
Center Reference Document 08-15.

Parkes, G. (2001). Understanding SPR and its use in U.S. fisheries
management. Report of MRAG Americas, Inc. to The Ocean Conservancy,
Washington, D.C.

Slipke, J.W., Martin, A.D., Pitlo, Jr., J., and Maceina, M.J. (2002).
Use of the spawning potential ratio for the upper Mississippi River
channel catfish fishery. North American Journal of Fisheries Management.
22(4):1295-1300.
<https://doi.org/10.1577/1548-8675(2002)022%3C1295:UOTSPR%3E2.0.CO;2>

Willems, T., Liverpool, E., and Hiwat, M. (2023). A preliminary
assessment of the spawning potential ratio of five target species of the
coastal gillnet fishery in Guyana and Suriname. Gulf and Caribbean
Research. 34(1):29-42. <https://doi.org/10.18785/gcr.3401.06>
