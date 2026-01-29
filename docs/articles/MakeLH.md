# Create life history object

``` r
library(rFAMS)
library(FSA)     ## for growth model 
library(FSAdata) ## for data used in example
```

The objective of this article is to demonstrate how to build a life
history object that is used in all of rFAMS core functions
(yield-per-recruit modeling
\[[`yprBH_minLL_fixed()`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_minLL_fixed.md),
[`yprBH_minLL_var()`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_minLL_var.md),
[`yprBH_SlotLL()`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_SlotLL.R.md)\]
and dynamic pool modeling
\[[`dpmBH()`](https://fishr-core-team.github.io/rFAMS/reference/dpmBH.md)\].

## Build a life history parameter object from user specified values

Life history parameters required for yield-per-recruit and dynamic pool
modeling include the initial number of recruits, maximum age, von
Bertalanffy growth model parameters ($`L_\infty`$, $`K`$, and $`t_0`$)
and parameters from the log10-transformed weight length model (alpha and
beta). These must be stored in a list or vector, by default
[`makeLH()`](https://fishr-core-team.github.io/rFAMS/reference/makeLH.md)
returns values in list. Users have the option of building a list
manually but the
[`makeLH()`](https://fishr-core-team.github.io/rFAMS/reference/makeLH.md)
function contains a series of checks that can identify potential errors.

If a list is returned then values will be displayed with the number of
decimals provided by the user. If a vector is returned then the number
of decimals displayed will be the same for each value and will match the
value supplied by the user with the most decimals. Thus, a list is
preferred as it will be easier to match what was given to what was
expected to be given.

The first example explicitly specifies each argument. The two created
objects below (LHparamsList and LHparamsVector) can then be used in any
of the core rFAMS functions.

``` r
#Return parameters as a list
LHparamsList <- makeLH(N0=100,tmax=15,Linf=500,K=0.3,t0=-0.5,LWalpha=-5.613,LWbeta=3.1)
#View life history object
LHparamsList
#> $N0
#> [1] 100
#> 
#> $tmax
#> [1] 15
#> 
#> $Linf
#> [1] 500
#> 
#> $K
#> [1] 0.3
#> 
#> $t0
#> [1] -0.5
#> 
#> $LWalpha
#> [1] -5.613
#> 
#> $LWbeta
#> [1] 3.1

#Return parameters as a vector
LHparamsVector <- makeLH(N0=100,tmax=15,Linf=500,K=0.3,t0=-0.5,LWalpha=-5.613,LWbeta=3.1,
                         restype="vector")
#View life history object
LHparamsVector
#>      N0    tmax    Linf       K      t0 LWalpha  LWbeta 
#> 100.000  15.000 500.000   0.300  -0.500  -5.613   3.100
```

## Build a life history parameter object from model output

The
[`makeLH()`](https://fishr-core-team.github.io/rFAMS/reference/makeLH.md)
function also allows model objects as input for von Bertalanffy growth
and length-weight models. See the [FSA
package](https://fishr-core-team.github.io/FSA/index.html) for more
information about fitting von Bertalanffy growth models and
length-weight models.

``` r
## Generate LVB results using the SptoVA1 data from the FSA package
data(SpotVA1,package="FSA")
SpotVA1 <- SpotVA1 |>
  dplyr::mutate(tl=tl*25.4)
vb1 <- FSA::makeGrowthFun(type="von Bertalanffy")
fit1 <- nls(tl~vb1(age,Linf,K,t0),data=SpotVA1,
            start=FSA::findGrowthStarts(tl~age,data=SpotVA1))

## Generate length-weight regression results using the BluegillLM data from the FSAdata package
data(BluegillLM,package="FSAdata")
BluegillLM <- BluegillLM |>
  dplyr::mutate(logW=log10(wght),
                logL=log10(tl))
fit2 <- lm(logW~logL,data=BluegillLM)
```

The model object created above are then passed through the
[`makeLH()`](https://fishr-core-team.github.io/rFAMS/reference/makeLH.md)
function in the `Linf` and `LWalpha` arguments.

``` r
LHparams <- makeLH(N0=100,tmax=15,Linf=fit1,LWalpha=fit2)
LHparams
#> $N0
#> [1] 100
#> 
#> $tmax
#> [1] 15
#> 
#> $Linf
#> [1] 426.6744
#> 
#> $K
#> [1] 0.2249323
#> 
#> $t0
#> [1] -2.55739
#> 
#> $LWalpha
#> [1] -5.524963
#> 
#> $LWbeta
#> [1] 3.406255
```
