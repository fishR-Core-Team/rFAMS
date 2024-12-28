# library(ggplot2)
# library(tidyverse)
# library(metR)  #needed for contour plots
#
# source("ypr.R")
# source("ypr_func.R")
#
# #Estimate yield based on a fixed minimum length and range of exploitation with the ypr1 function
# # Res_1<-ypr(cfmin=0.05,cfmax=0.60,cfinc=0.05,
# #             cm=0.10,
# #             minlength=355,
# #             initialN=100,
# #             linf=2000,
# #             K=0.50,
# #             t0=-0.616,
# #             LWalpha=-5.453,
# #             LWbeta=3.10,
# #             Mage=15)
#
# #Calculate yield based on a range of cf, cm and minimum length limit
# Res_1<-ypr(cfmin = 0.05,
#            cfmax = 0.95,
#            cfinc = 0.05,
#            cmmin = 0.05,
#            cmmax = 0.95,
#            cminc = 0.05,
#            lengthmin = 100,
#            lengthmax = 600,
#            lengthinc= 50,
#            initialN=100,
#            linf=2000,
#            K=0.50,
#            t0=-0.616,
#            LWalpha=-5.453,
#            LWbeta=3.10,
#            Mage=15)
#
#
# #Extract exploitation and yield for cm = 0.40 with MLL = 400
# #Which index has cm = 0.40
#
# targetcm = 0.40
# exploitation<-Res_1[[9]]$exploitation[,which(Res_1[[9]]$cmvect==targetcm)]
# yield <- Res_1[[9]]$yield[,which(Res_1[[9]]$cmvect==targetcm)]
#
# ggplot() +
#   theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
#   geom_point(aes(x=exploitation, y= yield)) +
#   geom_line(aes(x=exploitation, y= yield)) +
#   xlab("Exploitation")+
#   ylab("Yield (g)")+
#   theme(axis.text.x=element_text(size=20),
#         axis.text.y=element_text(size=20),
#         axis.title.x=element_text(size=22),
#         axis.title.y=element_text(size=22,angle=90),
#         panel.border = element_blank(),
#         axis.line = element_line(colour = "black")
#   )
#
# #Extract exploitation and yield from each MLL and CF with cm = 0.40
# #into a dataframe with one column of yield for each row is length limit and each column is exploitation
#
# targetcm = 0.40
#
# minlength <- Res_1[[1]]$MLvect
#
# exploitation <- Res_1[[3]]$exploitation[,which(Res_1[[3]]$cmvect==targetcm)]
#
# yield_df <- matrix(nrow=length(Res_1),ncol=length(exploitation))
# for(x in 1:length(Res_1)){
#   yield_df[x,] <- Res_1[[x]]$yield[,which(Res_1[[3]]$cmvect==targetcm)]
# }
#
# #Convert to long format
# yield_df <- data.frame(yield_df)
# names(yield_df) <- exploitation
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
#
#
# #Number Harvested Isopleth
# #Extract exploitation and yield from each MLL and CF with cm = 0.40
# #into a dataframe with one column of yield for each row is length limit and each column is exploitation
#
# #Does not match FAMS number harvest isopleth.
# #FAMS has number harvested/caught > initial number(N0). This can't be possible...
#
# targetcm = 0.8
#
# minlength <- Res_1[[1]]$MLvect
#
# exploitation <- Res_1[[3]]$exploitation[,which(Res_1[[3]]$cmvect==targetcm)]
#
# Catch_df <- matrix(nrow=length(Res_1),ncol=length(exploitation))
# for(x in 1:length(Res_1)){
#   Catch_df[x,] <- Res_1[[x]]$Nharvest[,which(Res_1[[3]]$cmvect==targetcm)]
# }
#
# #Convert to long format
# Catch_df <- data.frame(Catch_df)
# names(Catch_df) <- exploitation
#
# Catch_df <- Catch_df %>% pivot_longer(cols = names(Catch_df)[1]:names(Catch_df)[ncol(Catch_df)]) %>%
#   mutate(MLL = sort(rep(minlength,ncol(Catch_df)))) %>%
#   mutate_at(c('name'), as.numeric)
#
# #Yield isopleth
# ggplot(data = Catch_df) +
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
