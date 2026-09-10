# Example of simulating yield with the dynamic pool model,

lhparms <- makeLH(N0=100,tmax=30,Linf=1349.5,K=0.111,t0=0.065,
            LWalpha=-5.2147,LWbeta=3.153)
simyears <- 50
minLL <- 400
rec <- genRecruits(method = "fixed", nR = 100, simyears = simyears)
rec <- genRecruits(method = "normal", simyears = simyears,
                   meanR = 1000, sdR = 500, minR = 100, maxR =2500)
cm <- matrix(rep(c(rep(0,1), rep(0.18,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)
cf <- matrix(rep(c(rep(0,1), rep(0.33,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)
matchRicker=FALSE
species="Striped Bass"
group="landlocked"

SPRdat<- makeSPR(FLR = "linear", FLRint = -1057029, FLRslope = 2777.08, MatAge = 4,
                 percF=c(0,0,0,rep(0.50,12)),
                 percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,9)))

out<-dpmBH_MinLL(simyears = simyears, minLL = minLL, cf = cf,
                 cm = cm, rec = rec, lhparms = lhparms,
                 matchRicker=FALSE,species="Striped Bass",group="landlocked", SPRdat = SPRdat)

#Use summary by year data frame to plot yield vs year
ggplot(data=out[[2]],mapping=aes(x=year,y=PSD)) +
  geom_point() +
  geom_line() +
  labs(y="PSD",x="Year") +
  theme_bw()
ggplot(data=out[[2]],mapping=aes(x=year,y=wtSPR)) +
  geom_point() +
  geom_line() +
  labs(y="PSD",x="Year") +
  theme_bw()

#Plot date using summary by age
#Plot yield vs age for each year class
ggplot(data=out[[1]],mapping=aes(x=age,y=yield,group=yc,color=yc)) +
  geom_point() +
  geom_line() +
  labs(y="Total yield (g)",x="Age") +
  theme_bw()

#Use summary by year data frame to plot yield vs year
ggplot(data=out[[2]],mapping=aes(x=year,y=Yield_age_1plus)) +
  geom_point() +
  geom_line() +
  labs(y="Total yield (g)",x="Year") +
  theme_bw()

#Plot date using summary by age
#filter for year class = 1
plotdat<- out[[1]] |> filter(yc==1)
#Plot yield vs age
ggplot(data=plotdat,mapping=aes(x=age,y=yield)) +
  geom_point() +
  geom_line() +
  labs(y="Total yield (g)",x="Age") +
  theme_bw()
