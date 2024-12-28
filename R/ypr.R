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
#' @param initialN The initial number of new recruits entering the fishery
#' @param linf Point estimate of Linf from the LVB model in mm
#' @param k Point estimate of k from the LVB model
#' @param t0 Point estimate of t0 from the LVB model
#' @param LWalpha Point estimate of alpha from the length-weight regression
#' @param LWbeta Point estimate of beta from the length-weight regression
#' @param Mage integer of maximum age in the population in years
#'
#' @details Details will be filled out later
#'
#' #' @return the following values in a list of length equal to the number of minimum length limit values. The first element is the smallest minimum length and the last element is the largest minimum length.
#' \itemize{
#' \item exploitation is a matrix of exploitation rate with rows = number of cf values and columns = number of cm values
#' \item yield is a matrix of yield with rows = number of cf values and columns = number of cm values
#' \item Nharvest is a matrix of the number of harvested fish with rows = number of cf values and columns = number of cm values
#' \item Ndie is a matrix of the number of fish that die of natural deaths with rows = number of cf values and columns = number of cm values
#' \item wt is a matrix of the average weight of fish harvested with rows = number of cf values and columns = number of cm values
#' \item avgl is a matrix of the average length of fish harvested with rows = number of cf values and columns = number of cm values
#' \item Nt is a matrix of the number of fish at time t (time they become harvestable size) with rows = number of cf values and columns = number of cm values
#' \item Fmort is a matrix of the estimated instantaneous rate of fishing mortality with rows = number of cf values and columns = number of cm values
#' \item Mmort is a matrix of the estimated  instantaneous rate of natural mortality with rows = number of cf values and columns = number of cm values
#' \item Zmort is a matrix of the estimated  instantaneous rate of total mortality with rows = number of cf values and columns = number of cm values
#' \item S is a matrix of the estimated total survival with rows = number of cf values and columns = number of cm values
#' \item cfvect is a vector of cf values used to calculate yield
#' \item cmvect is a vector of cm values used to calculate yield
#' \item MLvect is a vector of minimum length limits used to calculate yield
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
#' Res_1<-ypr(cfmin = 0.05,
#'            cfmax = 0.95,
#'            cfinc = 0.05,
#'            cmmin = 0.05,
#'            cmmax = 0.95,
#'            cminc = 0.05,
#'            lengthmin = 200,
#'            lengthmax = 600,
#'            lengthinc= 25,
#'            initialN=100,
#'            linf=2000,
#'            K=0.50,
#'            t0=-0.616,
#'            LWalpha=-5.453,
#'            LWbeta=3.10,
#'            Mage=15)
#'
#' #Extract exploitation and yield for cm = 0.40 with minimum length limit = 400
#' #MLL of 400mm is the 9th element in the output list
#'
#' #Set target cm = 0.40
#' targetcm = 0.40
#' #Extract exploitation and yield from the list element with output from minimum length limit = 400
#' exploitation<-Res_1[[9]]$exploitation[,which(Res_1[[9]]$cmvect==targetcm)]
#' yield <- Res_1[[9]]$yield[,which(Res_1[[9]]$cmvect==targetcm)]
#'
#' #Plot yield curve
#' ggplot() +
#'  theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
#'  geom_point(aes(x=exploitation, y= yield)) +
#'  geom_line(aes(x=exploitation, y= yield)) +
#'  xlab("Exploitation")+
#'  ylab("Yield (g)")+
#'  theme(axis.text.x=element_text(size=20),
#'        axis.text.y=element_text(size=20),
#'        axis.title.x=element_text(size=22),
#'        axis.title.y=element_text(size=22,angle=90),
#'        panel.border = element_blank(),
#'        axis.line = element_line(colour = "black")
#'        )
#'
#' #Plot isopleth of yield for a range of exploitation and cm = 0.40
#' #This code extracts output into a dataframe.
#'
#' #Set target cm
#' targetcm = 0.40
#'
#' #Extract range of minimum length limits
#' minlength <- Res_1[[1]]$MLvect
#'
#' #Extract range of exploitation rates
#' exploitation <- Res_1[[3]]$exploitation[,which(Res_1[[3]]$cmvect==targetcm)]
#'
#' #Build yield dataframe
#' yield_df <- matrix(nrow=length(Res_1),ncol=length(exploitation))
#' for(x in 1:length(Res_1)){
#'     yield_df[x,] <- Res_1[[x]]$yield[,which(Res_1[[3]]$cmvect==targetcm)]
#'     }
#'
#' #Assign column anmes and convert from wide to long
#' yield_df <- data.frame(yield_df)
#' names(yield_df) <- exploitation
#'
#' yield_df <- yield_df %>% pivot_longer(cols = names(yield_df)[1]:names(yield_df)[ncol(yield_df)]) %>%
#'             mutate(MLL = sort(rep(minlength,ncol(yield_df)))) %>%
#'             mutate_at(c('name'), as.numeric)
#'
#' #Plot isopleth
#' ggplot(data = yield_df) +
#'   theme_bw()+theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
#'   geom_contour(aes(x=as.numeric(name),y=MLL,z=value))+
#'   metR::geom_text_contour(aes(x=as.numeric(name),y=MLL,z = value),stroke = 0.15)+
#'   xlab("Exploitation")+
#'   ylab("Minimum length limit (mm)")+
#'   theme(axis.text.x=element_text(size=20),
#'         axis.text.y=element_text(size=20),
#'         axis.title.x=element_text(size=22),
#'         axis.title.y=element_text(size=22,angle=90),
#'         panel.border = element_blank(),
#'         axis.line = element_line(colour = "black")
#'         )
#' @rdname ypr
#' @export
ypr<-function(cfmin,cfmax,cfinc=0.1,cmmin,cmmax,cminc=0.1,lengthmin,lengthmax,lengthinc=1,
              initialN,linf,K,t0,LWalpha,LWbeta,Mage){

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
  if (missing(initialN))
    stop("Need to specify initialN")
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
  if (missing(Mage))
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

  res_out<-list(length=length(MLvect))



  for(z in 1:length(MLvect)){
    exploitation<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    yield<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    Nharvest<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    Ndie<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    wt<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    avgl<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    Nt<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    Fmort<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    Mmort<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    Zmort<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))
    S<-matrix(NA,nrow=length(cfvect),ncol=length(cmvect))

    for(y in 1:length(cfvect)){ #Row
      for(x in 1:length(cmvect)){ #Column

        Res_1<-ypr_func(cf=cfvect[y],
                        cm=cmvect[x],
                        minlength=MLvect[z],
                        initialN=initialN,
                        linf=linf,
                        K=K,
                        t0=t0,
                        LWalpha=LWalpha,
                        LWbeta=LWbeta,
                        Mage=Mage)

        #Pull out results and add to appropriate location in matrix
        exploitation[y,x] = Res_1$exploitation
        yield[y,x] = Res_1$yield
        Nharvest[y,x] = Res_1$Nharvest
        Ndie[y,x] = Res_1$Ndie
        wt[y,x] = Res_1$wt
        avgl[y,x] = Res_1$avgl
        Nt[y,x] = Res_1$Nt
        Fmort[y,x] = Res_1$Fmort
        Mmort[y,x] = Res_1$Mmort
        Zmort[y,x] = Res_1$Zmort
        S[y,x] = Res_1$S
      }
    }
    res_out[[z]] <-list(exploitation,yield,Nharvest,Ndie,wt,avgl,Nt,Fmort,Mmort,Zmort,S,cfvect,cmvect,MLvect)
    names(res_out[[z]]) <- c("exploitation","yield","Nharvest","Ndie","wt","avgl","Nt","Fmort","Mmort","Zmort","S","cfvect","cmvect","MLvect")
  }

  #Create a list of lists. Each main list is the Minimum Length.
  #Within each list item are matrices of cf x cm
  return(res_out)

}
