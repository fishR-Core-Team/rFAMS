#' @title Function to simulate expected yield using the Beverton-Holt Yield Per Recruit model for single input parameters
#'
#' @description Function to estimate yield using the Beverton-Holt YPR model. This main function accepts only single values for cf, cm, and minlength. Use the wrapper ypr() function for specifying range of cf, cm, and minlength
#'
#' @param cf A numeric representing conditional fishing mortality
#' @param cm A numeric representing conditional natural mortality
#' @param minlength A numeric representing the minimum length limit for harvest in mm
#' @param initialN A numeric representing the initial number of new recruits entering the fishery
#' @param linf A numeric representing the point estimate of Linf from the LVB model in mm
#' @param K A numeric representing the point estimate of k from the LVB model
#' @param t0 A numeric representing the point estimate of t0 from the LVB model
#' @param LWalpha A numeric representing the point estimate of alpha from the length-weight regression
#' @param LWbeta A numeric representing the point estimate of beta from the length-weight regression
#' @param Mage An integer representing of maximum age in the population in years
#'
#' @details Details will be filled out later
#'
#' @return the following values in a list
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
#' }
#'
#' @author Jason C. Doll, \email{jason.doll@fmarion.edu}
#'
#' @examples
#' #Estimate yield with fixed parameters
#' Res_1<-ypr_func(cf = 0.45,
#'                 cm = 0.25,
#'                 minlength = 355,
#'                 initialN = 100,
#'                 linf = 2000,
#'                 K = 0.50,
#'                 t0 = -0.616,
#'                 LWalpha = -5.453,
#'                 LWbeta = 3.10,
#'                 Mage = 15)
#'
#' @rdname ypr_function
#' @export

ypr_func<-function(cf,cm,minlength,initialN,linf,K,t0,LWalpha,LWbeta,Mage){
  if (missing(cf))
    stop("Need to specify cf.")
  if (missing(cm))
    stop("Need to specify cm.")
  if (missing(minlength))
    stop("Need to specify minimum length.")
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

  exploitation <- c()


  #time of initial age used for estimates of natural mortality
  ti <- 1

  #time for fish to recruit to a minimum length limit
  TL <- minlength  #Minimum length limit

  #maximum theoretical weight derived from L-inf and weight to length regression
  #log10 transformation to linearize it
  Winf <-  10^(LWalpha + log10(linf) * LWbeta)

  #Value for incomplete beta functions and other calculations
  # t0 <- rnorm(1, mean = t0vec[1], sd = t0vec[2])

  maxage <- Mage  #maximum age
  #slope of the weight-length relation + 1
  #distribution comes from LW Bayes model
  Q <- LWbeta + 1


  Fmort <- -1 * log(-1*(cf-1))
  Mmort <- -1 * log(-1*(cm-1))
  Zmort <- Fmort+Mmort
  S <- exp(-Zmort)

  exploitation <-(Fmort*(1-S))/Zmort


  #Number of recruits entering the population
  N0 <- initialN

  #time in years to recruit to the fishery (tr - to)

  if(TL<linf){ tr <- ((log(1-TL/linf))/-K) + t0
  } else{ tr <- ((log(1-TL/(TL+.1)))/-K) + t0
  }

  r <- tr - t0

  #Number of recruits entering the fishery at some minimum length at time (t):
  Nt <- N0 * exp(-Mmort * tr)

  if (Nt<0) {Nt=0
  } else if (Nt>N0){Nt=N0
  } else {Nt=Nt}

  Y <- ((Fmort*Nt*exp(Zmort*r) * Winf) / K) *
    (beta(Zmort/K, Q)  * pbeta(exp(-K*r), Zmort/K, Q) -
       beta(Zmort/K, Q)  * pbeta(exp(-K*(maxage-t0)), Zmort/K, Q))

  #Uses Ibeta function from zipfR pacakge - only for testing
  #Y <- ((Fmort*Nt*exp(Z*r) * Winf) / K) * (Ibeta(exp(-K*r), Z/K, Q) - Ibeta(exp(-K*(maxage-t0)), Z/K, Q))

  if (is.na(Y)==T) {Y=NA
  } else if (is.infinite(Y)==T) {Y=NA
  } else if (Y<0){Y=0
  } else {Y=Y}


  #number of fish harvested
  Nharv<-(Nt*Fmort)/Zmort

  if (Nharv<1){
    Nharv=0
    wt=NA
    avgl=NA
    Y=0
  } else if (Nharv>Nt) {
    Nharv=Nt
    wt <- Y/Nharv
    avgl <- 10^((log10(wt) - LWalpha)/LWbeta)
  } else {
    Nharv=Nharv
    wt <- Y/Nharv
    avgl <- 10^((log10(wt) - LWalpha)/LWbeta)
  }

  if (is.na(wt)==F)
  { if (wt<1)
  {wt=NA}
    else
    {wt=wt}
  }

  if (is.na(avgl)==F)
  { if (avgl<1)
  {avgl=NA}
    else if (avgl<minlength)
    {avgl=minlength}
    else
    {avgl=avgl}
  }


  Ndie<-(Nt*Mmort)/Zmort

  if (Ndie<0)
  {Ndie=0
  } else if (Ndie>Nt){Ndie=Nt
  } else {Ndie=Ndie}



  yield<-Y

  Res<-list(exploitation=exploitation,
            yield=Y,
            Nharvest=Nharv,
            Ndie=Ndie,
            wt=wt,
            avgl=avgl,
            Nt= Nt,
            Fmort=Fmort,
            Mmort=Mmort,
            Zmort=Zmort,
            S=S)
  #class(Res) <- "ypr_res"
  return(Res)
}

