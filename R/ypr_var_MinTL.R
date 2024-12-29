#' @title Main function to simulate expected yield using the Beverton-Holt Yield Per Recruit model for a range of input parameters
#'
#' @description Main wrapper function to estimate yield using the Beverton-Holt YPR model. This main function accepts a range of values for cf, cm, and minlength (lengthmin, lengthmax, and lengthinc).
#'
#' @param cfmin Single value, minimum conditional fishing mortality
#' @param cfmax Single value, maximum conditional fishing mortality
#' @param cfinc Single value, increment to cycle from minimum to maximum conditional fishing mortality
#' @param cmmin Single value, minimum conditional natural mortality
#' @param cmmax Single value, maximum conditional natural mortality
#' @param cminc Single value, increment to cycle from minimum to maximum conditional natural mortality
#' @param lengthmin The lower limit of minimum length limit for harvest in mm
#' @param lengthmax The upper limit of minimum length limit for harvest in mm
#' @param lengthinc The increment to cycle from lower to upper minimum length limit for harvest in mm
#' @param N0 The initial number of new recruits entering the fishery
#' @param linf Point estimate of Linf from the LVB model in mm
#' @param K Point estimate of k from the LVB model
#' @param t0 Point estimate of t0 from the LVB model
#' @param LWalpha Point estimate of alpha from the length-weight regression on the log10 scale.
#' @param LWbeta Point estimate of beta from the length-weight regression on the log10 scale.
#' @param maxage integer of maximum age in the population in years
#'
#' @details Details will be filled out later
#'
#' @return the following values in a dataframe for each cf, cm, and minimum length limit specified.
#' \itemize{
#' \item exploitation is the exploitation rate
#' \item yield is the calculated yield
#' \item Nharvest is the number of harvested fish
#' \item Ndie is the number of fish that die of natural deaths.
#' \item wt is the average weight of fish harvested
#' \item avgl is the average length of fish harvested
#' \item Nt is the number of fish at time t (time they become harvestable size)
#' \item Fmort is the estimated instantaneous rate of fishing mortality
#' \item Mmort is the estimated  instantaneous rate of natural mortality
#' \item Zmort is the estimated  instantaneous rate of total mortality
#' \item S is the estimated total survival
#' \item cf A numeric representing conditional fishing mortality
#' \item cm A numeric representing conditional natural mortality
#' \item minlength A numeric representing the minimum length limit for harvest in mm
#' \item N0 A numeric representing the initial number of new recruits entering the fishery
#' \item linf A numeric representing the point estimate of Linf from the LVB model in mm
#' \item K A numeric representing the point estimate of k from the LVB model
#' \item t0 A numeric representing the point estimate of t0 from the LVB model
#' \item LWalpha A numeric representing the point estimate of alpha from the length-weight regression on the log10 scale.
#' \item LWbeta A numeric representing the point estimate of beta from the length-weight regression on the log10 scale.
#' \item maxage An integer representing of maximum age in the population in years
#' }
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @examples
#' #Load other required packages for organizing output and plotting
#' library(ggplot2)
#' library(tidyverse)
#' library(metR)
#'
#' #Estimate yield
#' Res_1<-ypr_var_MinTL(cfmin = 0.05,
#'                         cfmax = 0.95,
#'                         cfinc = 0.05,
#'                         cmmin = 0.05,
#'                         cmmax = 0.95,
#'                         cminc = 0.05,
#'                         lengthmin = 200,
#'                         lengthmax = 600,
#'                         lengthinc= 25,
#'                         N0=100,
#'                         linf=2000,
#'                         K=0.50,
#'                         t0=-0.616,
#'                         LWalpha=-5.453,
#'                         LWbeta=3.10,
#'                         maxage=15)
#'
#' #Extract exploitation and yield for cm = 0.40 with minimum length limit = 400
#'
#' #Subset output dataframe to plot results
#' plot_dat <- Res_1 %>%
#'   filter(cm == 0.40, minlength ==400)
#'
#' #Plot yield curve
#' ggplot(data = plot_dat, aes(x=exploitation,y=yield)) +
#'   theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
#'   geom_point() +
#'   geom_line() +
#'   xlab("Exploitation")+
#'   ylab("Yield (g)")+
#'   theme(axis.text.x=element_text(size=20),
#'        axis.text.y=element_text(size=20),
#'        axis.title.x=element_text(size=22),
#'        axis.title.y=element_text(size=22,angle=90),
#'        panel.border = element_blank(),
#'        axis.line = element_line(colour = "black")
#'        )
#'
#' #Plot isopleth of yield for a range of exploitation and cm = 0.40
#'
#' #Subset output dataframe
#' plot_dat <- Res_1 %>%
#'   filter(cm == 0.40)
#'
#' #Plot isopleth
#' ggplot(data = plot_dat) +
#'   theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
#'   geom_contour(aes(x=exploitation,y=minlength,z=yield))+
#'   metR::geom_text_contour(aes(x=exploitation,y=minlength,z = yield),stroke = 0.15)+
#'   xlab("Exploitation")+
#'   ylab("Minimum length limit (mm)")+
#'   theme(axis.text.x=element_text(size=20),
#'         axis.text.y=element_text(size=20),
#'         axis.title.x=element_text(size=22),
#'         axis.title.y=element_text(size=22,angle=90),
#'         panel.border = element_blank(),
#'         axis.line = element_line(colour = "black")
#'         )
#'
#' #Plot isopleth of number harvested/caught for a range of exploitation and cm = 0.40
#'
#' #Subset output dataframe
#' plot_dat <- Res_1 %>%
#'   filter(cm == 0.40)
#'
#'ggplot(data = plot_dat) +
#' theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
#' geom_contour(aes(x=exploitation,y=minlength,z=Nharvest))+
#' metR::geom_text_contour(aes(x=exploitation,y=minlength,z = Nharvest),stroke = 0.15)+
#' xlab("Exploitation")+
#' ylab("Minimum length limit (mm)")+
#' theme(axis.text.x=element_text(size=20),
#'       axis.text.y=element_text(size=20),
#'       axis.title.x=element_text(size=22),
#'       axis.title.y=element_text(size=22,angle=90),
#'       panel.border = element_blank(),
#'       axis.line = element_line(colour = "black")
#'       )
#' @rdname ypr_var_MinTL
#' @export
ypr_var_MinTL<-function(cfmin,cfmax,cfinc=0.1,cmmin,cmmax,cminc=0.1,lengthmin,lengthmax,lengthinc=1,
              N0,linf,K,t0,LWalpha,LWbeta,maxage){

  if (missing(cfmin))
    stop("Need to specify cfmin.")
  if (missing(cfmax))
    stop("Need to specify cfmax.")
  if (missing(cfinc))
    stop("Need to specify cfinc.")
  if (missing(cmmin))
    stop("Need to specify cmmin")
  if (missing(cmmax))
    stop("Need to specify cmmax")
  if (missing(cminc))
    stop("Need to specify cminc")
  if (missing(lengthmin))
    stop("Need to specify minimum lengthmin")
  if (missing(lengthmax))
    stop("Need to specify minimum lengthmax")
  if (missing(lengthinc))
    stop("Need to specify minimum lengthinc")
  if (missing(N0))
    stop("Need to specify N0")
  if (missing(linf))
    stop("Need to specify Linf.")
  if (missing(K))
    stop("Need to specify K.")
  if (missing(t0))
    stop("Need to specify t0.")
  if (missing(LWalpha))
    stop("Need to specify Length-weight intercept, alpha.")
  if (missing(LWbeta))
    stop("Need to specify Length-weight slope, beta.")
  if (missing(maxage))
    stop("Need to specify a maximum age.")

  if(cfmin>cfmax)
    stop("cfmin must be equal to or less than cfmax")
  if(cmmin>cmmax)
    stop("cmmin must be equal to or less than cmmax")
  if(lengthmin>lengthmax)
    stop("lengthmin must be equal to or less than lengthmax")

  #Note to calculate yield with one cf, one cm, or one minimum length limit
  #Set cfmin=cfmax, cmmin=cmmax, or lengthmin=lengthmax. Increment is defaulted to 0.1 and 1

  #Set up cfvector
  cfvect <- seq(from=cfmin,to=cfmax,by=cfinc)
  #Set up cm vector
  cmvect <- seq(from=cmmin,to=cmmax,by=cminc)
  #Set up length vector
  MLvect <- seq(from=lengthmin,to=lengthmax,by=lengthinc)

  #Create a dataframe to hold output
  res_out <- data.frame(exploitation=numeric(),
                        yield=numeric(),
                        Nharvest=numeric(),
                        Ndie=numeric(),
                        wt=numeric(),
                        avgl=numeric(),
                        Nt=numeric(),
                        Fmort=numeric(),
                        Mmort=numeric(),
                        Zmort=numeric(),
                        S=numeric(),
                        cf=numeric(),
                        cm=numeric(),
                        minlength=numeric(),
                        N0=integer(),
                        linf=numeric(),
                        K=numeric(),
                        t0=numeric(),
                        LWalpha=numeric(),
                        LWbeta=numeric(),
                        maxage=numeric()
                        )

  for(z in 1:length(MLvect)){
    for(y in 1:length(cfvect)){ #Row
      for(x in 1:length(cmvect)){ #Column

        #
        res_out <- rbind(res_out,
                         ypr_func(cf=cfvect[y],
                                  cm=cmvect[x],
                                  minlength=MLvect[z],
                                  N0=N0,
                                  linf=linf,
                                  K=K,
                                  t0=t0,
                                  LWalpha=LWalpha,
                                  LWbeta=LWbeta,
                                  maxage=maxage))
      }
    }
  }

  return(res_out)

}
