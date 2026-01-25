# Compute meta-analytic estimates of instantaneous and conditional natural mortality

Several methods may be used to estimate instantaneous (M) and
conditional natural mortality (cm) from other types of data, especially
those saved in the life history parameters vector/list from
[`makeLH`](https://fishr-core-team.github.io/rFAMS/reference/makeLH.md).

## Usage

``` r
est_natmort(lhparms = NULL, method = "rFAMS", incl.avg = FALSE, ...)
```

## Arguments

- lhparms:

  A named vector or string returned by `lhparms`.

- method:

  A string that indicates what methods to use to estimate M (see
  [`metaM`](https://fishr-core-team.github.io/FSA/reference/metaM.html)).

- incl.avg:

  A logical that indicates whether the average cm should be computed
  from the estimated M of all methods.

- ...:

  Option arguments for parameter values required by methods using
  parameters other than those in `lhparms`. See examples.

## Value

A data.frame with the following items:

- `method`: The name for the method within the function (as given in
  `method`).

- `M`: The estimated instantaneous natural mortality rate (from `metaM`)

- `cm`: The estimated conditional natural mortality rate (computed
  directly from `M`).

- `givens`: A string that contains the input values required by the
  method to estimate M.

## Details

The default `method`s to use are all of those listed in
[`Mmethods`](https://fishr-core-team.github.io/FSA/reference/metaM.html)
that use some of the life history parameters required by `makeLH`. These
methods are not all equally useful or robust, so the user may want to
select a subset of them for use after learning more about them. See
references in
[`metaM`](https://fishr-core-team.github.io/FSA/reference/metaM.html).

Other methods that require parameters other than those required by
`makeLH` can be used by providing the name of the method in `method` and
the required parameters as arguments, as defined in
[`metaM`](https://fishr-core-team.github.io/FSA/reference/metaM.html).
See
[`metaM`](https://fishr-core-team.github.io/FSA/reference/metaM.html)
for more details and the examples below for an example.

## Author

Derek Ogle

## Examples

``` r
# An example lhparm as would be returned from makeLH
tmp <- list(N0=100,tmax=15,Linf=500,K=0.3,t0=-0.5,LWalpha=-5.16,LWbeta=3.1)

# All methods in metaM() that use those life history parameters
est_natmort(tmp)
#>            method          M         cm                  givens
#> 1       HoenigNLS 0.41002265 0.33636478                 tmax=15
#> 2         HoenigO 0.29543534 0.25579246                 tmax=15
#> 3        HoenigOF 0.27939445 0.24375845                 tmax=15
#> 4        HoenigOM 0.35947963 0.30196053                 tmax=15
#> 5        HoenigOC 0.24095916 0.21412628                 tmax=15
#> 6        HoenigO2 0.29631969 0.25645032                 tmax=15
#> 7       HoenigO2F 0.25683005 0.22650034                 tmax=15
#> 8       HoenigO2M 0.35214421 0.29682129                 tmax=15
#> 9       HoenigO2C 0.31107738 0.26734282                 tmax=15
#> 10       HoenigLM 0.36126963 0.30320890                 tmax=15
#> 11   HewittHoenig 0.28133333 0.24522330                 tmax=15
#> 12          tmax1 0.34060000 0.28865661                 tmax=15
#> 13      PaulyLNoT 0.47024562 0.37515122          K=0.3, Linf=50
#> 14             K1 0.50760000 0.39806150                   K=0.3
#> 15             K2 0.56300000 0.43050200                   K=0.3
#> 16       JensenK1 0.45000000 0.36237185                   K=0.3
#> 17       JensenK2 0.65100000 0.47847601                   K=0.3
#> 18 AlversonCarney 0.19872105 0.18022146          tmax=15, K=0.3
#> 19   ChenWatanabe 0.07181977 0.06930138 tmax=15, K=0.3, t0=-0.5

# Same but including the average in the last row
est_natmort(tmp,incl.avg=TRUE)
#>            method          M         cm                  givens
#> 1       HoenigNLS 0.41002265 0.33636478                 tmax=15
#> 2         HoenigO 0.29543534 0.25579246                 tmax=15
#> 3        HoenigOF 0.27939445 0.24375845                 tmax=15
#> 4        HoenigOM 0.35947963 0.30196053                 tmax=15
#> 5        HoenigOC 0.24095916 0.21412628                 tmax=15
#> 6        HoenigO2 0.29631969 0.25645032                 tmax=15
#> 7       HoenigO2F 0.25683005 0.22650034                 tmax=15
#> 8       HoenigO2M 0.35214421 0.29682129                 tmax=15
#> 9       HoenigO2C 0.31107738 0.26734282                 tmax=15
#> 10       HoenigLM 0.36126963 0.30320890                 tmax=15
#> 11   HewittHoenig 0.28133333 0.24522330                 tmax=15
#> 12          tmax1 0.34060000 0.28865661                 tmax=15
#> 13      PaulyLNoT 0.47024562 0.37515122          K=0.3, Linf=50
#> 14             K1 0.50760000 0.39806150                   K=0.3
#> 15             K2 0.56300000 0.43050200                   K=0.3
#> 16       JensenK1 0.45000000 0.36237185                   K=0.3
#> 17       JensenK2 0.65100000 0.47847601                   K=0.3
#> 18 AlversonCarney 0.19872105 0.18022146          tmax=15, K=0.3
#> 19   ChenWatanabe 0.07181977 0.06930138 tmax=15, K=0.3, t0=-0.5
#> 20        AVERAGE 0.35248695 0.29106797                        

# Selecting just one method
est_natmort(tmp,method="HoenigNLS")
#>      method         M        cm  givens
#> 1 HoenigNLS 0.4100226 0.3363648 tmax=15

# Selecting several methods
est_natmort(tmp,method=c("HoenigNLS","HoenigO","HoenigO2","HoenigLM"))
#>      method         M        cm  givens
#> 1 HoenigNLS 0.4100226 0.3363648 tmax=15
#> 2   HoenigO 0.2954353 0.2557925 tmax=15
#> 3  HoenigO2 0.2963197 0.2564503 tmax=15
#> 4  HoenigLM 0.3612696 0.3032089 tmax=15

# A method that uses a parameter not usually in lhparms
est_natmort(tmp,method="QuinnDeriso",PS=0.05)
#>        method         M        cm           givens
#> 1 QuinnDeriso 0.1997155 0.1810363 PS=0.05, tmax=15

# Selecting all Hoenig methods using Mmethods from FSA
est_natmort(tmp,method=FSA::Mmethods("Hoenig"))
#>          method         M        cm  givens
#> 1     HoenigNLS 0.4100226 0.3363648 tmax=15
#> 2       HoenigO 0.2954353 0.2557925 tmax=15
#> 3      HoenigOF 0.2793944 0.2437585 tmax=15
#> 4      HoenigOM 0.3594796 0.3019605 tmax=15
#> 5      HoenigOC 0.2409592 0.2141263 tmax=15
#> 6      HoenigO2 0.2963197 0.2564503 tmax=15
#> 7     HoenigO2F 0.2568301 0.2265003 tmax=15
#> 8     HoenigO2M 0.3521442 0.2968213 tmax=15
#> 9     HoenigO2C 0.3110774 0.2673428 tmax=15
#> 10     HoenigLM 0.3612696 0.3032089 tmax=15
#> 11 HewittHoenig 0.2813333 0.2452233 tmax=15

# Over-riding the Linf param in parameters list, but others from tmp
est_natmort(tmp,method="PaulyLNoT")              # Linf from tmp
#>      method         M        cm         givens
#> 1 PaulyLNoT 0.4702456 0.3751512 K=0.3, Linf=50
est_natmort(tmp,Linf=1000/10,method="PaulyLNoT") # Linf from Linf= arg
#>      method         M        cm          givens
#> 1 PaulyLNoT 0.3740975 0.3120902 K=0.3, Linf=100
```
