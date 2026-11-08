# Function to calculate static spawning potential ratio

An INTERNAL function used by
[`yprBH_MinLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_MinLL.md)
and
[`yprBH_SlotLL`](https://fishr-core-team.github.io/rFAMS/reference/yprBH_SlotLL.md)
to calculate static spawning potential ratio with the yield-per-recruit
model

## Usage

``` r
static_spr(
  tmax,
  ageInterval,
  Linf,
  K,
  t0,
  FLR,
  FLRint,
  FLRslope,
  MatAge,
  percF,
  percFSpawn,
  L,
  cm,
  cf
)
```

## Arguments

- tmax:

  A single numeric that represents the maximum age of fish.

- ageInterval:

  A single numeric that represents the interval of ages to be used
  (e.g., 0.1 means that ages will be modeled by tenths of a year).

- Linf:

  A single numeric that represents the point estimate of asymptotic mean
  length from the von Bertalanffy growth model OR an `nls` object
  created from fitting the von Bertalanffy equation to length-at-age
  data.

- K:

  A single numeric that represents the point estimate of the Brody
  growth coefficient from the von Bertalanffy growth model.

- t0:

  A single numeric that represents the point estimate of the x-intercept
  (i.e., theoretical age at a mean length of 0) from the von Bertalanffy
  growth model.

- FLR:

  A single character to indicate the fecundity-length relationship;
  either "linear" or "log10".

- FLRint:

  A single numeric that represents the intercept of the fecundity-length
  relationship.

- FLRslope:

  A single numeric that represents the slope of the fecundity-length
  relationship.

- MatAge:

  A single integer that represents the age at maturity.

- percF:

  A numeric vector representing the percentage of females at each age,
  starting at age-1.

- percFSpawn:

  A numeric vector representing the percentage of spawning females at
  each age, starting at age-1.

- L:

  A numeric vector of lengths where mortality may change. Will assume
  first value is 0 if not provided. See details and examples.

- cm:

  A numeric that represents natural mortalities for the length intervals
  in `L`. See details and examples.

- cf:

  A numeric that represents fishing mortalities for the length intervals
  in `L`. See details and examples.

## Value

A list of six elements

- `Regs` is a list containing management regulations:

  - `L` = either minimum length limit or slot limit lengths.

  - `M` = natural mortality.

  - `F` = fishing mortality.

- `intcalcs` is a data frame containing values used in calculating SPR.

  - `t` is the time step in years based on the `ageInterval` used.

  - `L_t` is the Length at time t.

  - `Fec_t` is the fecundity at time t.

  - `percF_t` is the percentage of females as specified in the input
    parameters.

  - `percFSpawn_t` is the percentage of spawning females as specified in
    the input parameters.

  - `E_t` = `Fec_t` x `percF_t` x `percFSpawn_t`

  - `F_t` is the instantaneous fishing mortality at time `t`

  - `M_t` is the instantaneous natural mortality at time `t`

  - `Z_t` is the total instantaneous mortality at time `t`

  - `S_noexp_t` is total annual survival with no exploitation at time
    `t`

  - `S_exp_t` is the total annual survival with exploitation at time `t`

  - `S_noexp_t1` is the cumulative product of total annual survival with
    no exploitation up to time `t`

  - `S_exp_t1` is the cumulative product of total annual survival with
    exploitation up to time `t`

  - `P_noexp_t` is the spawning potential with no exploitation at time
    `t`

  - `P_exp_t` is the spawning potential with exploitation at time `t`

- `P_unfished` is the total spawning potential with NO exploitation.

- `P_fished` is the total spawning potential with exploitation.

- `SPR` is the spawning potential ratio based on specified regulations.

- `Num_Eggs` is the total number of eggs expected based on specified
  regulations

Note that P_fished, P_unfished and SPR are set to 0 when they
calculation yields a number less than 0.0001.

## Details

This function is generally not used independently. It is called when
requesting the spawning potential ratio while calculating
yield-per-recruit.

## Author

Derek Ogle, <jason.doll@fmarion.edu>

Jason C. Doll, <DerekOgle51@gmail.com>

## Examples

``` r
#===== Spawning potential ratio for fixed parameters with a single minimum length limit
#----- FAMS inputs
cf <- 0.1
cm <- 0.18
percF <- c(0,0,0,rep(0.50,27))
percFSpawn <- c(0,0,0,0.24,0.24,0.53,rep(1.00,24))
L <- 280    # MLL

# Adjust ageInterval to test effect of fine-tuning ages on estimates
spr_1a <- static_spr(tmax = 30, ageInterval=0.1,
                     Linf = 1349.5, K = 0.111, t0 = 0.065,
                     FLR = "linear", FLRint = -1057029, FLRslope = 2777.08,
                     MatAge = 4, percF = percF, percFSpawn = percFSpawn,
                     L = L, cm = cm, cf = cf)


#===== spawning potential ratio for fixed parameters with a slot limit
L <- c(200,300,400)  ## Recruitment L, then lower & upper L for harvest slot
cf <- c(0,0.25,0.60,0.15)
cm <- rep(0.30,length(cf))

spr_2a <- static_spr(tmax = 30, ageInterval=0.1,
                     Linf = 1349.5, K = 0.111, t0 = 0.065,
                     FLR = "linear", FLRint = -1057029, FLRslope = 2777.08,
                     MatAge = 4, percF = c(0,0,0,rep(0.50,27)),
                     percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)),
                     L = L, cm= cm, cf = cf)
```
