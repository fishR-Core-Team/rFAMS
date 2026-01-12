
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FAMS

<!-- badges: start -->

<!-- badges: end -->

Fisheries Analysis and Modeling Simulator in R

## Purpose

To recreate and expand/improve upon the FAMS/FAST software functionality
for equilibrium and dynamic pool models, utilizing existing fishR and
FSA components wherever possible

## Status

The package currently replicates all yield per recruit and dynamic pool
modeling in FAMS with the exception of spawning potential ratio. That
function will be added at a later date. Life history parameters can be
estimated using FSA.

## Process

### Equilibrium Modeling

1.  Complete
2.  Add spawning potential ratio
3.  Incorporate suggestions from the fisheries community.

### RShiny Development

1.  Determine which variables should be allowed to vary in interface.
2.  Build a Shiny app for testing different length regulation scenarios.

### Dynamic Pool Modeling

1.  Complete
2.  Add spawning potential ratio
3.  Incorporate suggestions from the fisheries community.

## Installation (Section Under Development)

You can install the development version of FAMS from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fishR-Core-Team/FAMS")
```

## Example (Section Under Development)

This is a basic example which shows you how to solve a common problem:

``` r
#Load other required packages for organizing output and plotting
library(rFAMS)
#> Welcome to rFAMS! We are currently in development. Please do not distribute.
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(metR)
library(tidyr)

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

# Life history parameters to be used below
LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)

#Estimate yield
Res_1 <- yprBH_SlotLL(recruitmentTL=200,lowerSL=250,upperSL=325,
                      cfunder=0.25,cfin=0.6,cfabove=0.15,cmmin=0.3,cmmax=0.6,cminc=0.05,
                      lhparms=LH)

Res_1
#>     cm TotalYield TotalNharv TotalNdie yieldUnder  yieldIn yieldAbove    uUnder
#> 1 0.30   9587.852  30.319080  23.01694  1251.5389 6037.309  2299.0040 0.2120703
#> 2 0.35   7351.774  24.574297  22.24638  1069.8047 4839.183  1442.7858 0.2052112
#> 3 0.40   5611.796  19.682869  20.98520   903.2554 3813.586   894.9547 0.1981511
#> 4 0.45   4243.747  15.534616  19.35642   751.6909 2946.258   545.7979 0.1908634
#> 5 0.50   3163.191  12.040623  17.45903   614.9004 2223.163   325.1268 0.1833156
#> 6 0.55   2310.783   9.126475  15.37708   492.6607 1630.491   187.6315 0.1754660
#> 7 0.60   1643.192   6.727817  13.18526   384.7347 1154.658   103.7991 0.1672608
#>         uIn     uAbove NharvestUnder NharvestIn NharvestAbove    N0die
#> 1 0.5182617 0.12677377      8.473698  19.625646     2.2197362 46.64404
#> 2 0.5033542 0.12258047      7.265359  15.827897     1.4810405 53.17276
#> 3 0.4879637 0.11826676      6.154504  12.555729     0.9726349 59.32995
#> 4 0.4720254 0.11381687      5.140149   9.769016     0.6254505 65.10843
#> 5 0.4554588 0.10921127      4.221259   7.428002     0.3913628 70.50022
#> 6 0.4381614 0.10442524      3.396745   5.493290     0.2364398 75.49642
#> 7 0.4200000 0.09942671      2.665456   3.925836     0.1365243 80.08692
#>   NdieUnder   NdieIn NdieAbove avglenUnder avglenIn avglenAbove avgwtUnder
#> 1 10.505888 7.639471 4.8715819    224.7908 281.2776    407.5833   147.6969
#> 2 10.879345 7.441293 3.9257394    224.5816 280.7497    400.0261   147.2473
#> 3 10.928309 6.999731 3.0571627    224.3558 280.1859    393.1118   146.7633
#> 4 10.681831 6.373828 2.3007624    224.1106 279.5811    386.7988   146.2391
#> 5 10.170789 5.619066 1.6691717    223.8426 278.9287    381.0284   145.6675
#> 6  9.428210 4.787165 1.1617049    223.5471 278.2205    375.7343   145.0391
#> 7  8.489694 3.925836 0.7697317    223.2179 277.4456    370.8494   144.3410
#>    avgwtIn avgwtAbove  trUnder     trIn  trOver  NrUnder      NrIn   NrAbove
#> 1 307.6235  1035.7105 1.761224 2.443479 3.68129 53.35596 34.376376 7.1112601
#> 2 305.7376   974.1704 1.761224 2.443479 3.68129 46.82724 28.682532 5.4133413
#> 3 303.7327   920.1343 1.761224 2.443479 3.68129 40.67005 23.587233 4.0317725
#> 4 301.5921   872.6477 1.761224 2.443479 3.68129 34.89157 19.069593 2.9267483
#> 5 299.2950   830.7556 1.761224 2.443479 3.68129 29.49978 15.107731 2.0606627
#> 6 296.8149   793.5699 1.761224 2.443479 3.68129 24.50358 11.678626 1.3981711
#> 7 294.1178   760.2978 1.761224 2.443479 3.68129 19.91308  8.757933 0.9062605
#>      FUnder       FIn    FAbove    MUnder       MIn    MAbove    ZUnder
#> 1 0.2876821 0.9162907 0.1625189 0.3566749 0.3566749 0.3566749 0.6443570
#> 2 0.2876821 0.9162907 0.1625189 0.4307829 0.4307829 0.4307829 0.7184650
#> 3 0.2876821 0.9162907 0.1625189 0.5108256 0.5108256 0.5108256 0.7985077
#> 4 0.2876821 0.9162907 0.1625189 0.5978370 0.5978370 0.5978370 0.8855191
#> 5 0.2876821 0.9162907 0.1625189 0.6931472 0.6931472 0.6931472 0.9808293
#> 6 0.2876821 0.9162907 0.1625189 0.7985077 0.7985077 0.7985077 1.0861898
#> 7 0.2876821 0.9162907 0.1625189 0.9162907 0.9162907 0.9162907 1.2039728
#>        ZIn    ZAbove SUnder  SIn SAbove cfUnder cfIn cfOver recruitmentTL
#> 1 1.272966 0.5191939 0.5250 0.28 0.5950    0.25  0.6   0.15           200
#> 2 1.347074 0.5933018 0.4875 0.26 0.5525    0.25  0.6   0.15           200
#> 3 1.427116 0.6733446 0.4500 0.24 0.5100    0.25  0.6   0.15           200
#> 4 1.514128 0.7603559 0.4125 0.22 0.4675    0.25  0.6   0.15           200
#> 5 1.609438 0.8556661 0.3750 0.20 0.4250    0.25  0.6   0.15           200
#> 6 1.714798 0.9610266 0.3375 0.18 0.3825    0.25  0.6   0.15           200
#> 7 1.832581 1.0788097 0.3000 0.16 0.3400    0.25  0.6   0.15           200
#>   lowerSL upperSL  N0 Linf   K   t0 LWalpha LWbeta tmax
#> 1     250     325 100  592 0.2 -0.3  -5.528  3.273   15
#> 2     250     325 100  592 0.2 -0.3  -5.528  3.273   15
#> 3     250     325 100  592 0.2 -0.3  -5.528  3.273   15
#> 4     250     325 100  592 0.2 -0.3  -5.528  3.273   15
#> 5     250     325 100  592 0.2 -0.3  -5.528  3.273   15
#> 6     250     325 100  592 0.2 -0.3  -5.528  3.273   15
#> 7     250     325 100  592 0.2 -0.3  -5.528  3.273   15

# Plot results
# Total Yield vs Conditional Natural Mortality (cm)
ggplot(data=Res_1,mapping=aes(x=cm,y=TotalYield)) +
 geom_point() +
 geom_line() +
 labs(y="Total Yield (g)",x="Conditional Natural Mortality (cm)") +
 theme_FAMS()
```

<img src="man/figures/README-example-1.png" alt="" width="100%" />

``` r


# Yield under, in, and above the slot limit vs Conditional Natural Mortality (cm)
# Select columns for plotting
plot_data <- Res_1 %>%
 select(cm, yieldUnder, yieldIn, yieldAbove) %>%
 pivot_longer(!cm, names_to="YieldCat",values_to="Yield")

# Generate plot
ggplot(data=plot_data,mapping=aes(x=cm,y=Yield,group=YieldCat,color=YieldCat)) +
 geom_point() +
 scale_color_discrete(name="Yield",labels=c("Above SL","In SL","Under SL"))+
 geom_line() +
 labs(y="Total Yield (g)",x="Conditional Natural Mortality (cm)") +
 theme_FAMS() +
 theme(legend.position = "top")+
 guides(color=guide_legend(title="Yield"))
```

<img src="man/figures/README-example-2.png" alt="" width="100%" />
