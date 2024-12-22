

#Need to save input parameters so we have MLL for plotting...

library(ggplot2)
library(tidyverse)
library(metR)  #needed for contour plots

source("ypr.R")
source("ypr_func.R")

#Estimate yield based on a fixed minimum length and range of exploitation with the ypr1 function
# Res_1<-ypr(cfmin=0.05,cfmax=0.60,cfinc=0.05,
#             cm=0.10,
#             minlength=355,
#             initialN=100,
#             linf=2000,
#             K=0.50,
#             t0=-0.616,
#             LWalpha=-5.453,
#             LWbeta=3.10,
#             Mage=15)

#Calculate yield based on one minimum length limit
Res_1<-ypr(cfmin = 0.05,
         cfmax = 0.95,
         cfinc = 0.05,
         cmmin = 0.05,
         cmmax = 0.95,
         cminc = 0.05,
         lengthmin = 200,
         lengthmax = 700,
         lengtinc= 10,
         initialN=100,
         linf=2000,
         K=0.50,
         t0=-0.616,
         LWalpha=-5.453,
         LWbeta=3.10,
         Mage=15)


#Extract exploitation and yield for cm = 0.40 with MLL = 400
exploitation <- Res_1[[3]][[1]][,8]
yield <- Res_1[[3]][[2]][,8]

ggplot() +
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_point(aes(x=exploitation, y= yield)) +
  geom_line(aes(x=exploitation, y= yield)) +
  xlab("Exploitation")+
  ylab("Yield (g)")+
  theme(axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=22),
        axis.title.y=element_text(size=22,angle=90),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")
  )

#Extract exploitation and yield from each MLL and CF with cm = 0.40
#into a dataframe with one column of yield for each row is length limit and each column is exploitation
minlength <- seq(from=200, to=700, by=10)

exploitation <- Res_1[[3]][[1]][,8]

yield_df <- matrix(nrow=length(Res_1),ncol=length(exploitation))
for(x in 1:length(Res_1)){
  yield_df[x,] <- Res_1[[x]][[2]][,8]
}

#Convert to long format
yield_df <- data.frame(yield_df)
names(yield_df) <- exploitation

yield_df <- yield_df %>% pivot_longer(cols = names(yield_df)[1]:names(yield_df)[ncol(yield_df)]) %>%
  mutate(MLL = sort(rep(minlength,ncol(yield_df)))) %>%
  mutate_at(c('name'), as.numeric)

#Yield isopleth
ggplot(data = yield_df) +
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_contour(aes(x=as.numeric(name),y=MLL,z=value))+
  metR::geom_text_contour(aes(x=as.numeric(name),y=MLL,z = value),stroke = 0.15)+
  xlab("Exploitation")+
  ylab("Minimum length limit (mm)")+
  theme(axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=22),
        axis.title.y=element_text(size=22,angle=90),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")
  )


# #Simulate yield based on a range minimum length and range of exploitation
# #minimum length range
# minlength <- seq(from=200, to=700, by=10)
#
# #initialize list
# Res_2 <- list()
#
# #loop through minimum lengths
# for(x in 1:length(minlength)){
#   Res_2[[x]]<-ypr(
#     cfmin=0.05,cfmax=0.95,cfinc=0.05,
#     cm=0.40,
#     minlength=minlength[x],                  #Minimum length limit
#     initialN=100,                   #Number of new recruits entering the fishery
#     linf=2000,                      #Point estimate of Linf from LVB
#     K=0.250,                         #Point estimate of K from LVB
#     t0=-0.616,                      #Point estimate of t0 from LVB
#     LWalpha=-5.453,              #Point estimate of alpha from LW regression
#     LWbeta=3.10,                    #Point estimate of beta from LW regression
#     Mage=15                        #Maximum age of fish in population
#   )
# }
#
# #Pull out yield from each list item
# yield_df <- matrix(nrow=length(minlength),ncol=length(Res_2[[1]]$uvect))
# for(x in 1:length(minlength)){
#   yield_df[x,] <- Res_2[[x]]$yieldall
# }
#
# #Convert to long format
# yield_df <- data.frame(yield_df)
# names(yield_df) <- Res_2[[1]]$uvect
#
# yield_df <- yield_df %>% pivot_longer(cols = names(yield_df)[1]:names(yield_df)[ncol(yield_df)]) %>%
#   mutate(MLL = sort(rep(minlength,ncol(yield_df)))) %>%
#   mutate_at(c('name'), as.numeric)
#
# #Yield isopleth
# ggplot(data = yield_df) +
#   theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
#   geom_contour(aes(x=as.numeric(name),y=MLL,z=value))+
#   metR::geom_text_contour(aes(x=as.numeric(name),y=MLL,z = value),stroke = 0.15)+
#   xlab("Exploitation")+
#   ylab("Minimum length limit (mm)")+
#   theme(axis.text.x=element_text(size=20),
#         axis.text.y=element_text(size=20),
#         axis.title.x=element_text(size=22),
#         axis.title.y=element_text(size=22,angle=90),
#         panel.border = element_blank(),
#         axis.line = element_line(colour = "black")
#   )
