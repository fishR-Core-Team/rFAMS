#Dynamic Pool Model Test Code

simyears = 50

#Generate initial recruits
#Fixed
fixedRec <- function(simyears = 50, Nrec = 1000){
  rec <- rep(x = Nrec, times = simyears)
  rec
}

rec <- fixedRec(Nrec = 100, simyears = simyears)

#Random - Uniform
unifRec <- function(simyears = 50, MinR = 10, MaxR = 1000){
  rec <- round(runif(n = simyears, min = MinR, max = MaxR))
  rec
}

rec <- unifRec(simyears = simyears, MinR = 10, MaxR = 1000)

#Random - Normal
rnormRec <- function(simyears = 50, meanR = 1000, sdR = 500, MinR = 100, MaxR = 2500){
  rec <- round(rnorm(n = simyears, mean = meanR, sd = sdR))
  rec[rec < MinR] <- MinR
  rec[rec > MaxR] <- MaxR

  rec
}

rec <- rnormRec(simyears = simyears, meanR = 1000, sdR = 500, MinR = 100, MaxR = 2500)

#Strong year-class every Nth year
StrYC_Nth <- function(simyears = 50, meanR = 1000, Nthyr = 5, sizeStr = 2){
  rec <- rep(c(rep(meanR, (Nthyr - 1)), (meanR * sizeStr)), (simyears/Nthyr))
  rec
}

rec <- StrYC_Nth(simyears = simyears, meanR = 1000, Nthyr = 5, sizeStr = 2)

#Strong year-class at random intervals
StrYC_randInt <- function(simyears = 50, meanR = 1000, avgFreq = 5, sizeStr = 2){
  rec <- rbinom(simyears,1,(1/avgFreq)) + 1
  rec[rec == 1] <- meanR
  rec[rec == 2] <- meanR * sizeStr
  rec
}

rec <- StrYC_randInt(simyears = simyears, meanR = 1000, avgFreq = 5, sizeStr = 1.25)

#DPM Code##################

Linf = 1349.5
K = 0.111
t0 = 0.065
# FLR = "linear"
# FLRint = -1057029
# FLRslope = 2777.08
MatAge = 4
LWalpha = -5.2147
LWbeta = 3.153
#Build cm and cf vectors - supplied by user
rec<-1000
tmax <- 30
cm <- c(0,rep(0.18,(tmax)))
cf <- c(0, rep(0.33,(tmax)))
MLL <- 400

#Generate number at age for each year-class
N_matrix <- matrix(NA,nrow=length(rec), ncol = (tmax+1)) #Define matrix
N_matrix[,1] <-rec #Add recruits to first column


# Maximum theoretical weight derived from L-inf and weight to length regression
#   log10 transformation to linearize it
Winf <- 10^(LWalpha+log10(Linf)*LWbeta)


#This seems to work great!
#N0 <- rec[1]

length <- c(rep(c,tmax+1))
weight <- c(rep(c,tmax+1))
N_start <- c(rep(c,tmax+1))
exploitation <- c(rep(c,tmax+1))
expect_nat_death  <- c(rep(c,tmax+1))
survival <- c(rep(c,tmax+1))
biomass <-c(rep(0,tmax+1))
abundvec <- c(rec[1],rep(0,tmax))
yieldvec <- c(rep(0,tmax+1))

for(x in 1:(tmax+1)){
  # Yield under the slot limit####
  # Instantaneous mortality rates (F,M,Z) ... rearrange of FAMS equations 4:16 & 4:17
  F <- -1*log(1-cf[x])
  M <- -1*log(1-cm[x])
  Z <- F+M
  # Annual survival rate (S)
  survival[x] <- exp(-Z)
  # Exploitation rate (u) ... rearrange of FAMS equation 4:14
  exploitation[x] <- (1-survival[x])*(F/Z)
  expect_nat_death[x] <- (1-survival[x])*(M/Z)

  #Still producing yield too high - try rerunning from the beginning after clearing environment

  # Time (years) when fish recruit to the fishery (tr) ... FAMS equation 6:2
  #   needed adjustment if minlength<Linf
  # and amount of time (years) to recruit to the fishery (r) ... defined in FAMS
  if (MLL<Linf) {
    tr <- ((log(1-MLL/Linf))/-K)+t0
  }else {
    tr <- ((log(1-MLL/(MLL+.1)))/-K)+t0}

  r = tr - floor(tr) #Time to reach fishery, partial year


  # Number recruiting to fishery based on time at minimum length (tr) ...
  #    FAMS equation 6:3
  #Nr <- N0*exp(-M*tr)
  Nr <- abundvec[x]

  # if(tr > (x-1)){
  #   age_enter_fishery <- tr
  # }else{
  #   age_enter_fishery <- x
  # }

  #remove fish lost to natural mortality up to time r
  if(x == (floor(tr)+1) && r > 0){
    Nr <- Nr * exp(-M * (r))
    age_enter_fishery <- tr
  }else{
    age_enter_fishery <- x -1
  }

  # Adjust Nr if less than 0 or greater than start, otherwise keep Nr as calculated
  #    not clear that this is done in FAMS
  if (Nr<0) {
    Nr <- 0
  }else if (Nr>N0) {
    Nr <- N0}

  #Max age at lower slot
  #tmax_lowerSL <- ((log(1-lowerSL/Linf))/-K)+t0
  #age at next step
  # tmax_lowerSL <- x+1

  #Works when fishery starts between x and x+1
  # Convenience calculations for beta function below ... per FAMS definitions
  #Replaced tr with age_enter_fishery
  P <- Z/K
  Q <- LWbeta+1
  X <- exp(-K*(age_enter_fishery-t0)) #age at entering the fishery - t0
  Xi <- exp(-K*(x-t0)) #max age in fishery - t0

  # FAMS equation 6:1
  yieldvec[x] <- (((F)*Nr*exp(Z*(age_enter_fishery-t0))*Winf)/K)*
    (beta(P,Q)*stats::pbeta(X,P,Q)-beta(P,Q)*stats::pbeta(Xi,P,Q))

  #... if matchRicker then Y_under is "corrected" to match equation 10.22 in Ricker
  if (matchRicker) yieldvec[x] <- Y_under*exp(M*t0)

#
#   #When fish enter fishery at x - WORKS
#   P <- Z/K
#   Q <- LWbeta+1
#   X <- exp(-K*(x-1-t0)) #age at entering the fishery - t0
#   Xi <- exp(-K*(x-t0))  #max age in fishery - to
#
#   # FAMS equation 6:1
#   Y_under <- (((F)*Nr*exp(Z*(x-1-t0))*Winf)/K)* #Z * (start age - t0)
#     (beta(P,Q)*stats::pbeta(X,P,Q)-beta(P,Q)*stats::pbeta(Xi,P,Q))

  # ... if matchRicker then Y_under is "corrected" to match equation 10.22 in Ricker
  #if (matchRicker) Y_under <- Y_under*exp(M*t0)

  #these are NOW working! Make sure that fish are removed due to M up to recruiting to fishery above
  #Already removed losses due to M before fish recruited to the fishery in age-4
  #works with age at entering fishery is between x and x+1
  if(x == (floor(tr)+1) && r > 0){
    Nharv_under <- (Nr - (Nr*exp(-Z* (1-r)))) * (F/Z) #1-r is simply number of years in the fishery
    Ndie_under <- (Nr - (Nr*exp(-Z* (1-r)))) * (M/Z)
  }else{
  #this works when age at enterying fishery is x
    Nharv_under <- (Nr - (Nr*exp(-Z* (1)))) * (F/Z) #1-r is simply number of years in the fishery
    Ndie_under <- (Nr - (Nr*exp(-Z* (1)))) * (M/Z)
  }

  if(x<(tmax+1)){
    abundvec[x+1] <- Nr - Nharv_under - Ndie_under
  }

  length[x] <- Linf * (1- exp(-K * (x - 1 -t0)))
  weight[x] <- (10^(LWalpha+log10(length[x])*LWbeta))
  if(MTL>0){
    biomass[x] <- weight[x] * abundvec[x]
  }else{
    biomass[x] <- 0
  }
  N_start<-abundvec[x]

}
