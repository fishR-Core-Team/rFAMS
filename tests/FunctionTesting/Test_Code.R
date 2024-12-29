library(ggplot2)
library(tidyverse)
library(metR)  #needed for contour plots

source("ypr_fixed_MinTL.R")
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

#Calculate yield based on a range of cf, cm and minimum length limit
Res_1<-ypr_fixed_MinTL(cfmin = 0.05,
                       cfmax = 0.95,
                       cfinc = 0.05,
                       cmmin = 0.05,
                       cmmax = 0.95,
                       cminc = 0.05,
                       lengthmin = 100,
                       lengthmax = 600,
                       lengthinc= 50,
                       N0=100,
                       linf=2000,
                       K=0.50,
                       t0=-0.616,
                       LWalpha=-5.453,
                       LWbeta=3.10,
                       maxage=15)


#Extract exploitation and yield for cm = 0.40 with MLL = 400
#Which index has cm = 0.40

plot_dat <- Res_1 %>%
  filter(cm == 0.40, minlength ==400)

ggplot(data = plot_dat, aes(x=exploitation,y=yield)) +
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_point() +
  geom_line() +
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

plot_dat <- Res_1 %>%
  filter(cm == 0.40)

#Yield isopleth
ggplot(data = plot_dat) +
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_contour(aes(x=exploitation,y=minlength,z=yield))+
  metR::geom_text_contour(aes(x=exploitation,y=minlength,z = yield),stroke = 0.15)+
  xlab("Exploitation")+
  ylab("Minimum length limit (mm)")+
  theme(axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=22),
        axis.title.y=element_text(size=22,angle=90),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")
  )


#Number Harvested Isopleth
#Extract exploitation and yield from each MLL and CF with cm = 0.40
#into a dataframe with one column of yield for each row is length limit and each column is exploitation

#Does not match FAMS number harvest isopleth.
#FAMS has number harvested/caught > initial number(N0). This can't be possible...

plot_dat <- Res_1 %>%
  filter(cm == 0.40)

ggplot(data = plot_dat) +
  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_contour(aes(x=exploitation,y=minlength,z=Nharvest))+
  metR::geom_text_contour(aes(x=exploitation,y=minlength,z = Nharvest),stroke = 0.15)+
  xlab("Exploitation")+
  ylab("Minimum length limit (mm)")+
  theme(axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=22),
        axis.title.y=element_text(size=22,angle=90),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black")
  )
