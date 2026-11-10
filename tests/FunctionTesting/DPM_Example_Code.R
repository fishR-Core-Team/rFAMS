library(rFAMS)
library(ggplot2)  ## to make figures
library(dplyr)    ## for filter
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

# Dynamic Pool Model with a vector of recruitment ####

simyears <- 50
minLL <- 400

# create life history parameter object
LH <- makeLH(N0=100,tmax=15,Linf=1349.5,K=0.111,t0=0.065,LWalpha=-5.2147,LWbeta=3.153)

# create matrices for cm and cf
cm <- matrix(rep(c(rep(0,1), rep(0.18,(LH$tmax))), simyears),nrow=simyears,byrow=TRUE)
cf <- matrix(rep(c(rep(0,1), rep(0.33,(LH$tmax))), simyears),nrow=simyears,byrow=TRUE)

# Spawning potential ratio parameters
# Required for recruitment function
SPRdat<- makeSPR(FLR = "linear", FLRint = -1057029, FLRslope = 2777.08, MatAge = 4,
                 percF=c(rep(0.50,15)), #required to be length of tmax
                 percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,9)))  #required to be length of tmax

# Vector of recruitment
rec <- genRecruits(method = "fixed", nR = 1000, simyears = simyears)

# run dynamic pool simulations
# NOTE NEW ARUGMENTS
#  recruitment_type = c("vector", "stockrecruit")
#  srmod: Not used when recruitment_type = "vector"
#  recv = named vector when recruitment_type = c("vector")
out1<-dpmBH_MinLL(simyears = simyears, minLL = minLL, cf = cf, cm = cm,
                  recruitment_type = "vector", recv=rec, SPRdat = SPRdat,
                  lhparms = LH, matchRicker=FALSE,species="Striped Bass",group="landlocked")

#Plot date using summary by age for a specific year class
#filter for year class = 40
plotdat<- out1[[1]] |> filter(yc==40)
# Plot yield vs age
ggplot(data=plotdat,mapping=aes(x=age,y=yield)) +
  geom_point() +
  geom_line() +
  labs(y="Total yield (g)",x="Age") +
  theme_FAMS()

# Plot SPR by year. Note with fixed recruitment SPR will be a flat line after population has stabilized
ggplot(data=out1[[2]],mapping=aes(x=year,y=wtSPR)) +
  geom_point() +
  geom_line() +
  labs(y="wtSPR",x="Year") +
  theme_FAMS()

# Dynamic Pool Model with a recruitment function AND SPR ####
simyears <- 100
minLL <- 400

# create life history parameter object
LH <- makeLH(N0=100,tmax=15,Linf=1349.5,K=0.111,t0=0.065,LWalpha=-5.2147,LWbeta=3.153)

# create matrices for cm and cf
cm <- matrix(rep(c(rep(0,1), rep(0.18,(LH$tmax))), simyears),nrow=simyears,byrow=TRUE)
cf <- matrix(rep(c(rep(0,1), rep(0.33,(LH$tmax))), simyears),nrow=simyears,byrow=TRUE)


# Spawning potential ratio parameters
# Required for recruitment function
SPRdat<- makeSPR(FLR = "linear", FLRint = -1057029, FLRslope = 2777.08, MatAge = 4,
                 percF=c(rep(0.50,15)), #required to be length of tmax
                 percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,9)))  #required to be length of tmax

# run dynamic pool simulations
# NOTE NEW ARUGMENTS
#  recruitment_type = c("vector", "stockrecruit")
#  srmod = c("Ricker", "BevertonHolt", "Shepherd")
#  recv: Not used when recruitment_type = c("stockrecruit")
#  a, b and sigmaR are parameters for the Ricker model. Only used when recruitment_type = c("stockrecruit") and srmod = c("Ricker")


out1<-dpmBH_MinLL(simyears = simyears, minLL = minLL, cf = cf, cm = cm,
                  recruitment_type = c("stockrecruit"), srmod = c("Ricker"),
                  a = 6.8, b = 0.0025, sigmaR = 0.2, SPRdat = SPRdat,
                  lhparms = LH, matchRicker=FALSE,species="Striped Bass",group="landlocked")

# simyears = simyears
# minLL = minLL
# cf = cf
# cm = cm
# recruitment_type = c("stockrecruit")
# srmod = c("Ricker")
# a = 6.8
# b = 0.0025
# sigmaR = 0.2
# SPRdat = SPRdat
# lhparms = LH
# matchRicker=FALSE
# species="Striped Bass"
# group="landlocked"

# Filter for years where population has stabilized. This is typically several years after the max age
# and plot yeild
out1[[2]] |>
  dplyr::filter(year>24) |>
  ggplot(mapping=aes(x=year,y=Yield_age_1plus)) +
  geom_point() +
  geom_line() +
  labs(y="Total yield (g)",x="Year") +
  theme_FAMS()


# Spawning potential ratio (SPR) across multiple minimum length limits ####
rec <- genRecruits(method = "fixed", nR = 1000, simyears = simyears)
LH <- makeLH(N0=1000,tmax=15,Linf=1349.5,K=0.111,t0=0.065,LWalpha=-5.2147,LWbeta=3.153)
minLL = 457
cf <- seq(from = 0.0, to = 0.70, by = 0.05)
cm <- 0.18
Res_1 <- yprBH_MinLL(minLL = minLL, cf = cf, cm = cm,SPRdat = SPRdat,
                     lhparms=LH)

SPRdat<- makeSPR(FLR = "linear", FLRint = -1057029, FLRslope = 2777.08, MatAge = 4,ageInterval=1,
                 percF=c(rep(0.50,15)), #required to be length of tmax
                 percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,9)))  #required to be length of tmax

out457<-dpmBH_MinLL(simyears = simyears, minLL = 457, cf = cf, cm = cm,
                    recruitment_type = c("vector"),
                    recv = rec,SPRdat = SPRdat,
                    lhparms = LH, matchRicker=FALSE,species="Striped Bass",group="landlocked")


out457<-dpmBH_MinLL(simyears = simyears, minLL = 457, cf = cf, cm = cm,
                    recruitment_type = c("stockrecruit"), srmod = c("Ricker"),
                    a = 6.8, b = 0.0025, sigmaR = 0.2, SPRdat = SPRdat,
                    lhparms = LH, matchRicker=FALSE,species="Striped Bass",group="landlocked")

out559<-dpmBH_MinLL(simyears = simyears, minLL = 559, cf = cf, cm = cm,
                    recruitment_type = c("stockrecruit"), srmod = c("Ricker"),
                    a = 6.8, b = 0.0025, sigmaR = 0.2, SPRdat = SPRdat,
                    lhparms = LH, matchRicker=FALSE,species="Striped Bass",group="landlocked")

out610<-dpmBH_MinLL(simyears = simyears, minLL = 610, cf = cf, cm = cm,
                    recruitment_type = c("stockrecruit"), srmod = c("Ricker"),
                    a = 6.8, b = 0.0025, sigmaR = 0.2, SPRdat = SPRdat,
                    lhparms = LH, matchRicker=FALSE,species="Striped Bass",group="landlocked")

# extract wtSPR from each simulation
SPR_all <- bind_rows(
  out457[[2]] |> select(year, wtSPR) |> mutate(MLL = "457 mm"),
  out559[[2]] |> select(year, wtSPR) |> mutate(MLL = "559 mm"),
  out610[[2]] |> select(year, wtSPR) |> mutate(MLL = "610 mm"))

# Filter years > 24
SPR_filtered <- SPR_all |>
  filter(year > 24)

# Mean and SD by minimum length limit
SPR_summary <- SPR_filtered |>
  group_by(MLL) %>%
  summarise(MeanSPR = mean(wtSPR, na.rm = TRUE),
            SDSPR = sd(wtSPR, na.rm = TRUE),
            .groups = "drop")

# View results
SPR_summary

# Violin plot with embedded boxplot and
# horizontal red line at desired threshold
ggplot(SPR_filtered,
       aes(x = MLL, y = wtSPR, fill = MLL)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1) +
  geom_hline(yintercept = 0.3, linetype = "dashed", color = "red", linewidth = 1)+
  theme_FAMS() +
  labs(x = "Minimum Length Limit",
       y = "Weighted SPR") +
  theme_FAMS()
