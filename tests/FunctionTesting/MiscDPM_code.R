# Example of simulating yield with the dynamic pool model,

lhparms <- makeLH(N0=1000,tmax=30,Linf=1349.5,K=0.111,t0=0.065,
            LWalpha=-5.2147,LWbeta=3.153)
simyears <- 100
minLL <- 400
rec <- genRecruits(method = "fixed", nR = 100, simyears = simyears)
rec <- genRecruits(method = "normal", simyears = simyears,
                   meanR = 1000, sdR = 500, minR = 100, maxR =2500)
cm <- matrix(rep(c(rep(0,1), rep(0.18,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)
cf <- matrix(rep(c(rep(0,1), rep(0.33,(lhparms$tmax))), simyears),nrow=simyears,byrow=TRUE)
matchRicker=FALSE
species="Striped Bass"
group="landlocked"
alpha = 6.8
beta = 0.0025
sigmaR = 0.4

ssb<-rnorm(30,1000,1000) * c(30,253,798,1707,2968,4537,6354,8353,10472,12658,14860,17042,19172,rep(20,7),rep(30,10))
rec<-alpha * ssb * exp(-beta * ssb + stats::rnorm(1, 0, sigmaR))

SPRdat<- makeSPR(FLR = "linear", FLRint = -1057029, FLRslope = 2777.08, MatAge = 4,
                 percF=c(0,0,0,rep(0.50,27)),
                 percFSpawn = c(0,0,0,0.24,0.24,0.53,rep(1.00,24)))

out<-dpmBH_MinLL(simyears = simyears, minLL = minLL, cf = cf,
                 cm = cm, recruitment_type = c("vector"), recv = rec, lhparms = lhparms,
                 matchRicker=FALSE,species="Striped Bass",group="landlocked", SPRdat = SPRdat)

minLL <- 400
out<-dpmBH_MinLL(simyears = simyears, minLL = minLL, cf = cf,a = 6.8, b = 0.0025, sigmaR = 0.2,
                 cm = cm, recruitment_type = c("stockrecruit"), stockrecruit = c("Ricker"), lhparms = lhparms,recv=1,
                 matchRicker=FALSE,species="Striped Bass",group="landlocked", SPRdat = SPRdat)

minLL <- 600
out2<-dpmBH_MinLL(simyears = simyears, minLL = minLL, cf = cf,a = 6.8, b = 0.0025, sigmaR = 0.2,
                  cm = cm, recruitment_type = c("stockrecruit"), stockrecruit = c("Ricker"), lhparms = lhparms,recv=1,
                 matchRicker=FALSE,species="Striped Bass",group="landlocked", SPRdat = SPRdat)

minLL <- 800
out3<-dpmBH_MinLL(simyears = simyears, minLL = minLL, cf = cf,a = 6.8, b = 0.0025, sigmaR = 0.2,
                  cm = cm, recruitment_type = c("stockrecruit"), stockrecruit = c("Ricker"), lhparms = lhparms,recv=1,
                  matchRicker=FALSE,species="Striped Bass",group="landlocked", SPRdat = SPRdat)

# #Use summary by year data frame to plot yield vs year
# ggplot(data=out[[2]],mapping=aes(x=year,y=PSD)) +
#   geom_point() +
#   geom_line() +
#   labs(y="PSD",x="Year") +
#   theme_bw()
# ggplot(data=out[[2]],mapping=aes(x=year,y=wtSPR)) +
#   geom_point() +
#   geom_line() +
#   labs(y="wtSPR",x="Year") +
#   theme_bw()
#
# out[[2]] |>
#   dplyr::filter(year>40) |>
# ggplot(mapping=aes(x=year,y=Yield_age_1plus)) +
#   geom_point() +
#   geom_line() +
#   labs(y="Yield",x="Year") +
#   theme_bw()
#
# out2[[2]] |>
#   dplyr::filter(year>40) |>
#   ggplot(mapping=aes(x=year,y=Yield_age_1plus)) +
#   geom_point() +
#   geom_line() +
#   labs(y="Yield",x="Year") +
#   theme_bw()

# Combine yield from both

Yield_400<-data.frame(MLL = as.factor(400), Yield_1plus=out[[2]]$Yield_age_1plus, year = out[[2]]$year)
Yield_600<-data.frame(MLL = as.factor(600), Yield_1plus=out2[[2]]$Yield_age_1plus, year = out2[[2]]$year)
Yield_800<-data.frame(MLL = as.factor(800), Yield_1plus=out3[[2]]$Yield_age_1plus, year = out3[[2]]$year)

combined <- rbind(Yield_400,Yield_600,Yield_800)
combined |>
  dplyr::filter(year>45) |>
  ggplot(mapping=aes(x=year,y=Yield_1plus,group=MLL,color=MLL)) +
    geom_point() +
    scale_color_discrete()+
    geom_line() +
    labs(y="Yield",x="Year") +
    theme_bw()

combined |>
  group_by(MLL) |>
  summarize(mean_y = mean(Yield_1plus), sd_y = sd(Yield_1plus))

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
