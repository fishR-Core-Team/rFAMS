Linf <- 1349.5
k <- 0.111
t0 <- 0.065
fec_int <- -1057029
fec_slope <- 2777.08
r <- 4
G <- 30
ages <- r:G
P <- c(0.4, 0.3, 0.2, 0.08, 0.02)
Z <- rep(0.23,length(ages))
M <- rep(0.18,length(ages))

#This is starting to look better -
#Make sure only M is applied before time to recruitment
#I think I need to do this after calculating average fecundity at age... Do I need to worry about number at age?
N0 <- 100
tr <- 2.16
num_at_age <- N0*exp(-0.18*tr) #number at start of fishery at time tr
time_to_next_year <- ceiling(tr) - tr
agevec<-c(tr,seq(from = (ceiling(tr)), to = G, by= 1))
num_at_age<- c(num_at_age, num_at_age * (exp(-0.18) * time_to_next_year))

#num_at_age <- c(nrec * exp(-0.18))


# for(x in 2:3){
#   num_at_age <- c(num_at_age, num_at_age[x-1] * exp(-0.18))
# }
for(x in 3:(G-1)){
  num_at_age <- c(num_at_age, num_at_age[x-1] * exp(-0.23))
}


pdat<-data.frame(agevec = agevec, num_at_age =num_at_age) %>%
  filter(agevec>=4) %>%
  mutate(P_fem = c(rep(0.50,27)),
         P_fem_spawn = c(0.24,0.24,0.53,rep(1.00,24)),
         l_at_age = Linf * (1 - exp(-k * (agevec - t0))),
         fec_at_age = fec_int + l_at_age * fec_slope,
         p_at_age = fec_at_age * num_at_age *P_fem *P_fem_spawn)

# P_fem <- c(rep(0.50,27))
# P_fem_spawn <- c(0.24,0.24,0.53,rep(1.00,24))
#

###This seems to work - but need to make a matrix of F and M to accomodate age and year specific mortality####
#This matches EXACTLY! Why isn't it matching in staticSPR.R?
Linf <- 1349.5
k <- 0.111
t0 <- 0.065
fec_int <- -1057029
fec_slope <- 2777.08
r <- 4
G <- 30
ages <- r:G
n<-G
l_at_age <- Linf * (1 - exp(-k * (ages - t0)))
fec_at_age <- c(0,0,0,fec_int + l_at_age * fec_slope)
#fec_at_age <- fec_int + l_at_age * fec_slope

tr <- 2.16
time_to_next_year <- ceiling(tr) - tr
agevec<-c(tr,seq(from = (ceiling(tr)), to = G, by= 1))

#M only from 0 up to tr
F_mort <- 0.051
M_mort <- rep(0.198,G)
F_mort_1 <- rep(0,(floor(tr)-1))
F_mort_2 <- F_mort*time_to_next_year
F_mort_3 <- rep(F_mort,(G-ceiling(tr)+1))

F_mort_all<-c(F_mort_1, F_mort_2, F_mort_3)

S_exp <- exp(-(F_mort_all + M_mort))
S_no_exp<-exp(-(M_mort))

P_fem = c(0,0,0,rep(0.50,27))
P_fem_spawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24))

sum(fec_at_age)
fec_at_age * P_fem * P_fem_spawn

P <- sum(sapply(1:n, function(i) {
  Ei <- fec_at_age[i] * P_fem[i] * P_fem_spawn[i]
  survivals <- S_exp[1:(i-1)]  # Get survival from age 0 to i
  prod_survival <- prod(survivals)
  Ei * prod_survival
}))

Punfished <- sum(sapply(1:n, function(i) {
  Ei <- fec_at_age[i] * P_fem[i] * P_fem_spawn[i]
  survivals <- S_no_exp[1:(i-1)] # Get survival from age 0 to i
  prod_survival <- prod(survivals)
  Ei * prod_survival
}))
P/Punfished

####################
lengthmin=280
lengthinc=60
lengthmax=400
cfmin=0.00
cfmax=0.70
cfinc=0.05
cmmin=0.18
cmmax=0.18
cminc=0.18
lhparms=LH
SPR = T
FLRdat=FLRdat
matchRicker=FALSE
FLR = "Linear"
MatAge = FLRdat$MatAge
FLRint = FLRdat$FLRint
FLRslope = FLRdat$FLRslope
percF = FLRdat$percF
percFSpawn = FLRdat$percFSpawn
N0=100
tmax=30
Linf=1349.5
K=0.111
t0=0.065
LWalpha=-5.2147
LWbeta=3.153
tr=2.16
F = 0.051
M=0.198

source("staticSPR.R")
source("MakeSPR.R")
source("yprBH_MinLL_var.R")
source("yprBH_func.R")
source("MakeLH.R")
source("rFAMS-internals.R")
# Life history parameters to be used below
LH <- makeLH(N0=100,tmax=30,Linf=1349.5,K=0.111,t0=0.065,LWalpha=-5.2147,LWbeta=3.153)
FLRdat<- makeSPR(FLR = "Linear", FLRint = -1057029, FLRslope = 2777.08, MatAge = 4,
                 percF=c(0,0,0,rep(0.50,27)), percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)))

# Estimate yield for multiple values of minLL, cf, and cm
# # This is a minimal example, lengthinc, cfinc, cminc would likely be smaller
# #   to produce finer-scaled results
Res_1 <- yprBH_minLL_var(lengthmin=280,lengthinc=60,lengthmax=400,
                       cfmin=0.00,cfmax=0.70,cfinc=0.05,
                       cmmin=0.18,cmmax=0.18,cminc=0.18,
                       lhparms=LH,
                       SPR = T,
                       FLRdat=FLRdat)




#Build mortality vector - only one year
M_mort <- c(0.18*time_to_next_year,rep(0.18,(length(agevec)-1)))
F_mort <-rep(0.05,length(M_mort))
S_exp <- exp(-(F_mort + M_mort))
S_no_exp<-exp(-(M_mort))

comb<-data.frame(fec_at_age=fec_at_age[2:30],agevec = agevec, S_exp = S_exp,S_no_exp=S_no_exp,M_mort=M_mort,F_mort=F_mort) %>%
  filter(agevec>3)

P <- sum(sapply(1:n, function(i) {
  Ei <- comb[i,1]
  survivals <- comb[1:i, 3]  # Get survival from age 0 to i
  prod_survival <- prod(survivals)
  Ei * prod_survival
}))

Punfished <- sum(sapply(1:n, function(i) {
  Ei <- comb[i,1]
  survivals <- comb[1:i, 4]  # Get survival from age 0 to i
  prod_survival <- prod(survivals)
  Ei * prod_survival
}))
P/Punfished

#
# p_at_age<-fec_at_age * num_at_age *P_fem *P_fem_spawn

# # Helper function: product of exp(-Z or -M) excluding current age
# survivorship_product <- function(mortality_vector, i) {
#   if (i == 1) {
#     return(1)  # No earlier ages to multiply
#   } else {
#     return(prod(exp(-mortality_vector[1:(i - 1)])))
#   }
# }
#
#
# # Numerator: using Z
# numerator <- sum(sapply(seq_along(ages), function(i) {
#  #fec_at_age[i] * survivorship_product(Z, i)
#   survivorship_product(Z, i)
# }))
#
# # Denominator: using M
# denominator <- sum(sapply(seq_along(ages), function(i) {
#   fec_at_age[i] * survivorship_product(M, i)
# }))
#
# # Exploitation index
# exploitation_index <- numerator / denominator


# Inputs
n <- length(ages)
#E <- fec_int + (0.93*l_at_age) * fec_slope #and convert from TL to FL
E<-pdat$p_at_age
E<-fec_at_age
sum(E)
# Natural mortality matrix M[i,j] = M_ij
# M <- matrix(c(
#   0.2, 0.2, 0.2, 0.2,
#   0.2, 0.2, 0.2, 0.2,
#   0.2, 0.2, 0.2, 0.2,
#   0.2, 0.2, 0.2, 0.2
# ), nrow = n, byrow = TRUE)



# M <- matrix(0.18,nrow=length(ages),ncol=length(ages))
#
# F <- matrix(0.05,nrow=length(ages),ncol=length(ages))
# # Unfished case: F = 0
# #F <- matrix(0, nrow = n, ncol = n)
#
# # Compute S_ij
# S <- exp(-(F + M))

# Compute P
P <- sum(sapply(1:n, function(i) {
  Ei <- E[i]
  survivals <- S[i, 1:i]  # Get survival from age 0 to i
  prod_survival <- prod(survivals)
  Ei * prod_survival
}))

P  # Output: potential reproductive output

F <- matrix(0,nrow=length(ages),ncol=length(ages))
# Compute S_ij
S <- exp(-(F + M))
# Compute P
Punfished <- sum(sapply(1:n, function(i) {
  Ei <- E[i]
  survivals <- S[i, 1:i]  # Get survival from age 0 to i
  prod_survival <- prod(survivals)
  Ei * prod_survival
}))

Punfished  # Output: potential reproductive output

P/Punfished
