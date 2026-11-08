#library(rFAMS)

# - Source rFAMS files#############
# Find all files ending in .R or .r in the folder
r_files <- list.files(path = "~/fishRCoreTeam/rFAMS/R/",
                      pattern = "\\.[Rr]$",
                      full.names = TRUE)

# Source all of them
lapply(r_files, source)
###################################


# Life history parameters to be used below
LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
# conditional natural mortality vector
cm <- seq(from = 0.1, to = 0.9, by = 0.1)

Res_1 <- yprBH_SlotLL(lowerSL=250,upperSL=325,
                      cfBelow=0.25,cfIn=0.0,cfAbove=0.15,cm=cm,
                      lhparms=LH,recruitmentTL=200,
                      loi=c(200,250,300,325,350),label="250-325")

cf <- 0.1
cm <- 0.18
percF <- c(0,0,0,rep(0.50,27))
percFSpawn <- c(0,0,0,0.24,0.24,0.53,rep(1.00,24))
L <- 280    # MLL
#----- Convert cf & cm to F and M
F <- c(0,-log(1-cf))
M <- rep(-log(1-cm),length(F))
FLR = "linear"
FLRint = -1057029
FLRslope = 2777.08
MatAge = 4

tmax = 30
ageInterval=0.1
Linf = 1349.5
K = 0.111
t0 = 0.065

minLL <- seq(from = 200, to = 550, by = 50)
cf <- seq(from = 0.1, to = 0.9, by = 0.1)
cm <- seq(from = 0.1, to = 0.9, by = 0.1)

percF <- c(0,0,0,rep(0.50,27))
percFSpawn <- c(0,0,0,0.24,0.24,0.53,rep(1.00,24))
FLR = "linear"
FLRint = -1057029
FLRslope = 2777.08
MatAge = 4
tmax = 30
ageInterval=0.1
Linf = 1349.5
K = 0.111
t0 = 0.065


res <- expand.grid(minLL=minLL,cf=cf,cm=cm)



spr_1a <- static_spr_alt(tmax = 30, ageInterval=0.1,
                         Linf = 1349.5, K = 0.111, t0 = 0.065,
                         FLR = "linear", FLRint = -1057029, FLRslope = 2777.08,
                         MatAge = 4, percF = percF, percFSpawn = percFSpawn,
                         L = L, M = M, F = F)


L <- c(200,300,400)  ## Recruitment L, then lower & upper L for harvest slot
                     ## ... F outside slot is hooking mortality???
F <- c(0,0.28768,0.91629,0.16252)
M <- rep(0.35667,length(F))

spr_2a <- static_spr_alt(tmax = 30, ageInterval=0.1,
                         Linf = 1349.5, K = 0.111, t0 = 0.065,
                         FLR = "linear", FLRint = -1057029, FLRslope = 2777.08,
                         MatAge = 4, percF = c(0,0,0,rep(0.50,27)),
                         percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)),
                         L = L, M = M, F = F)
# spr_2a
# plot_FM(spr_2a)


#----- FAMS inputs
cf <- 0.1
cm <- 0.18
percF <- c(0,0,0,rep(0.50,27))
percFSpawn <- c(0,0,0,0.24,0.24,0.53,rep(1.00,24))
L <- 280    # MLL
#----- Convert cf & cm to F and M
F <- c(0,-log(1-cf))
M <- rep(-log(1-cm),length(F))


# Life history parameters to be used below
LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
# conditional natural mortality vector
L <- seq(from = 200, to = 550, by = 50)
cf <- seq(from = 0.1, to = 0.9, by = 0.1)
cm <- seq(from = 0.1, to = 0.9, by = 0.1)

res <- expand.grid(L=L,cf=cf,cm=cm,tmax=tmax,ageInterval=ageInterval)

SPRdat<- makeSPR(FLR = "linear", FLRint = -1057029, FLRslope = 2777.08, MatAge = 4,
                 percF=c(0,0,0,rep(0.50,27)),
                 percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)))

pmap_inputs <- c(
  as.list(res),             # Unpacks L, cf, cm, tmax, ageInterval
  list(SPRdat = list(SPRdat)) # Wraps the whole SPRdat object so length == 1
)

# final_results <- purrr::pmap(pmap_inputs, static_spr_alt) |>
#   purrr::list_rbind()

final_results <- purrr::pmap(pmap_inputs, function(L, cf, cm, tmax, ageInterval, SPRdat) {

  # Call your actual function, passing the unpacked vectors from SPRdat
  static_spr_alt(
    L = L,
    cf = cf,
    cm = cm,
    Linf = LH$Linf,
    K = LH$K,
    t0 = LH$t0,
    tmax = LH$tmax,
    ageInterval = ageInterval,
    FLR = SPRdat$FLR,
    FLRint = SPRdat$FLRint,
    FLRslope = SPRdat$FLRslope,
    MatAge = SPRdat$MatAge,
    percF = SPRdat$percF,           # Extract vector
    percFSpawn = SPRdat$percFSpawn  # Extract vector
  )

})

# Extract P_fished,P_unfished, SPR, adn Num_Eggs
sapply(final_results, "[[", "SPR")

list_of_frames <- lapply(final_results, function(x) {
  data.frame(P_fished = x$P_fished, P_unfished = x$P_unfished,
             SPR = x$SPR,Num_Eggs = x$Num_Eggs
             , stringsAsFactors = FALSE)
})

# Bind all rows together into one final data frame
df_base <- do.call(rbind, list_of_frames)

# If SPF < 0.001, convert to 0
df_base$SPR[df_base$SPR < 0.001] <- 0

#----- Run DHO's version of the SPR estimating functions with FAMS inputs
#      Adjust ageInterval to test effect of fine-tuning ages on estimates
percF <- c(0,0,0,rep(0.50,12))
percFSpawn <- c(0,0,0,0.24,0.24,0.53,rep(1.00,9))

SPRdat<- makeSPR(FLR = "linear", FLRint = -1057029, FLRslope = 2777.08, MatAge = 4,
                 percF=c(0,0,0,rep(0.50,12)),
                 percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,9)))


# Life history parameters to be used below
LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
# conditional natural mortality vector
minLL <- seq(from = 200, to = 550, by = 25)
cf <- seq(from = 0.1, to = 0.9, by = 0.05)
cm <- seq(from = 0.1, to = 0.9, by = 0.1)
loi <- c(400,450,500,550)

# SPRdat<- makeSPR(FLR = "linear", FLRint = -1057029, FLRslope = 2777.08, MatAge = 4,
#                  percF=c(0,0,0,rep(0.50,27)),
#                  percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)),
#                  ageInterval=0.1)

Res_1 <-yprBH_MinLL(minLL = minLL, cf = cf, cm = cm,
                      lhparms=LH, loi=loi,SPRdat=SPRdat,SaveSPR_int=FALSE)

Res_1 <-yprBH_MinLL(minLL = minLL, cf = cf, cm = cm,
                    lhparms=LH, loi=loi)



plot_dat <- Res_1 |> filter(cm==0.5)

ggplot(data=plot_dat,mapping=aes(y=SPR,x=exploitation,
                                 group=minLL,color=minLL)) +
  geom_line(linewidth=1) +
  scale_color_gradient2() +
  xlab("Exploitation (u)")+
  ylab("SPR")+
  labs(color="Min Length Limit") +
  theme_bw()


# Slot limit
# Life history parameters to be used below
LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
# conditional natural mortality vector
cm <- seq(from = 0.1, to = 0.9, by = 0.1)

# Estimate yield based on a protected slot limit
Res_1 <- yprBH_SlotLL(lowerSL=250,upperSL=325,
                      cfBelow=0.25,cfIn=0.0,cfAbove=0.15,cm=cm,
                      lhparms=LH,recruitmentTL=200,
                      loi=c(200,250,300,325,350),label="250-325")

Res_1

# Plot results
# Total Yield vs Conditional Natural Mortality (cm)
ggplot(data=Res_1,mapping=aes(x=cm,y=yieldTotal)) +
  geom_point() +
  geom_line() +
  labs(y="Total Yield (g)",x="Conditional Natural Mortality (cm)") +
  theme_bw()

# Estimate yield based on a protected slot limit
Res_2 <- yprBH_SlotLL(lowerSL=250,upperSL=325,
                      cfBelow=0.25,cfIn=0.0,cfAbove=0.15,cm=cm,
                      lhparms=LH,recruitmentTL=200,
                      loi=c(200,250,300,325,350),label="250-325",SPRdat=SPRdat,SaveSPR_int=FALSE)

Res_2

# Plot results
# Total Yield vs Conditional Natural Mortality (cm)
ggplot(data=Res_2,mapping=aes(x=cm,y=SPR)) +
  geom_point() +
  geom_line() +
  labs(y="Total Yield (g)",x="Conditional Natural Mortality (cm)") +
  theme_bw()


lowerSL=250
upperSL=325
cfBelow=0.25
cfIn=0.0
cfAbove=0.15
cm=cm
lhparms=LH
recruitmentTL=200
loi=c(200,250,300,325,350)
label="250-325"

# Testing with Derek's function not integrated into code
LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
SPRdat<- makeSPR(FLR = "linear", FLRint = -1057029, FLRslope = 2777.08, MatAge = 4,
                 percF=c(0,0,0,rep(0.50,27)),
                 percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)),
                 ageInterval=0.1)

FLR = "linear"
FLRint = -1057029
FLRslope = 2777.08
MatAge = 4
percF=c(0,0,0,rep(0.50,27))
percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24))


F_ <- c(0,-log(1-cf))
M <- rep(-log(1-cm[1]),length(F))
L <- c(200,250,325)
spr_2a <- static_spr_alt(tmax = 15, ageInterval=0.1,
                         Linf = 592, K = 0.2, t0 = -0.3,
                         FLR = "linear", FLRint = -1057029, FLRslope = 2777.08,
                         MatAge = 4, percF = c(0,0,0,rep(0.50,27)),
                         percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)),
                         L = L, M = rep(M[1],length(F_)), F_ = F_)
spr_2a$SPR


tmax = 15
ageInterval=0.1
Linf = 592
K = 0.2
t0 = -0.3
FLR = "linear"
FLRint = -1057029
FLRslope = 2777.08
MatAge = 4
percF = c(0,0,0,rep(0.50,27))
percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24))
L = L
M = M[1]
F_ = F

