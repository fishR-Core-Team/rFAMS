## Note must use rFAMS::: because internal functions are not exported

test_that("is.wholenumber() results",{
  expect_true(rFAMS:::is.wholenumber(17))
  expect_true(rFAMS:::is.wholenumber(-17))
  expect_true(rFAMS:::is.wholenumber(0))
  expect_false(rFAMS:::is.wholenumber(1.7))
  expect_false(rFAMS:::is.wholenumber(-1.7))
})

test_that("iIbeta() messages and results",{
  # ----- error messages
  rFAMS:::iIbeta(x=-1,a=1,b=1) |>
    expect_error("'x' in incomplete beta function must be >=0")
  rFAMS:::iIbeta(x=2,a=1,b=1) |>
    expect_error("'x' in incomplete beta function must be <=1")
  rFAMS:::iIbeta(x=0.5,a=-1,b=1) |>
    expect_error("'a' in incomplete beta function must be >=0")
  rFAMS:::iIbeta(x=0.5,a=0.5,b=-1) |>
    expect_error("'b' in incomplete beta function must be >=0")

  # ----- comparing results to other packages
  df <- expand.grid(x=seq(0.05,0.95,0.1),
                    a=seq(0.1,3.0,0.2),
                    b=seq(0.1,3.0,0.2)) |>
    dplyr::mutate(zipfR=zipfR::Ibeta(x=x,a=a,b=b),
                  rFAMS=rFAMS:::iIbeta(x,a,b))
  expect_equal(df$rFAMS,df$zipfR)
})

test_that("iErrGT() and iErrLT() messages",{
  rFAMS:::iErrGT(1.3,1,"junk") |>
    expect_error("'junk' must be <=1")
  rFAMS:::iErrGT(c(0.3,1,1.3),1,"junk") |>
    expect_error("All 'junk' must be <=1")
  rFAMS:::iErrGT(0.3,1,"junk") |>
    expect_no_error()
  rFAMS:::iErrGT(1,1,"junk") |>
    expect_no_error()
  rFAMS:::iErrGT(c(0.3,0.1,0.9),1,"junk") |>
    expect_no_error()
  rFAMS:::iErrGT(c(0.3,0.1,1),1,"junk") |>
    expect_no_error()

  rFAMS:::iErrLT(-0.3,0,"junk") |>
    expect_error("'junk' must be >=0")
  rFAMS:::iErrLT(c(-0.3,1,1.3),0,"junk") |>
    expect_error("All 'junk' must be >=0")
  rFAMS:::iErrLT(0.3,0,"junk") |>
    expect_no_error()
  rFAMS:::iErrLT(0,0,"junk") |>
    expect_no_error()
  rFAMS:::iErrLT(c(0.3,0.1,0.9),0,"junk") |>
    expect_no_error()
  rFAMS:::iErrLT(c(0.3,0.1,0),0,"junk") |>
    expect_no_error()
})

test_that("iCheckN0() messages",{
  # ----- test that something was sent (optname is used in first ex just to test)
  rFAMS:::iCheckN0(optname="N0") |>
    expect_error("Need to specify an initial number of fish in the population in 'N0'")
  N0 <- NULL
  rFAMS:::iCheckN0(N0) |>
    expect_error("Need to specify an initial number of fish in the population in 'N0'")

  # ----- test wrong input types
  N0 <- -100
  rFAMS:::iCheckN0(N0) |>
    expect_error("'N0' must be >=0")
  N0 <- "a"
  rFAMS:::iCheckN0(N0) |>
    expect_error("'N0' must be a number")
  N0 <- c(300,500)
  rFAMS:::iCheckN0(N0) |>
    expect_error("Only use one value in 'N0'")
})

test_that("iCheckMaxAge() messages",{
  # ----- test that something was sent (optname is used in first ex just to test)
  rFAMS:::iCheckMaxAge(optname="tmax") |>
    expect_error("Need to specify a maximum age in 'tmax'")
  tmax <- NULL
  rFAMS:::iCheckMaxAge(tmax) |>
    expect_error("Need to specify a maximum age in 'tmax'")

  # ----- test wrong input types
  tmax <- -1
  rFAMS:::iCheckMaxAge(tmax) |>
    expect_error("'tmax' must be >=0")
  tmax <- 1.5
  rFAMS:::iCheckMaxAge(tmax) |>
    expect_warning("The maximum age in 'tmax' is not a whole number")
  tmax <- "a"
  rFAMS:::iCheckMaxAge(tmax) |>
    expect_error("'tmax' must be a number")
  tmax <- c(300,500)
  rFAMS:::iCheckMaxAge(tmax) |>
    expect_error("Only use one value in 'tmax'")
})

test_that("iCheckLinf() messages",{
  # ----- test that something was sent (optname is used in first ex just to test)
  rFAMS:::iCheckLinf(optname="Linf") |>
    expect_error("Need to specify a mean asymptotic length \\(mm\\) in 'Linf'")
  Linf <- NULL
  rFAMS:::iCheckLinf(Linf) |>
    expect_error("Need to specify a mean asymptotic length \\(mm\\) in 'Linf'")

  # ----- test wrong input types
  Linf <- -1
  rFAMS:::iCheckLinf(Linf) |>
    expect_error("'Linf' must be >=0")
  Linf <- "a"
  rFAMS:::iCheckLinf(Linf) |>
    expect_error("'Linf' must be a number")
  Linf <- c(300,500)
  rFAMS:::iCheckLinf(Linf) |>
    expect_error("Only use one value in 'Linf'")
  Linf <- 150
  rFAMS:::iCheckLinf(Linf) |>
    expect_warning("A mean asymptotic length of 150 mm seems too small")
  Linf <- 2500
  rFAMS:::iCheckLinf(Linf) |>
    expect_warning("A mean asymptotic length of 2500 mm seems too large")
})

test_that("iCheckK() messages",{
  # ----- test that something was sent (optname is used in first ex just to test)
  rFAMS:::iCheckK(optname="K") |>
    expect_error("Need to specify a Brody growth coefficient in 'K'")
  K <- NULL
  rFAMS:::iCheckK(K) |>
    expect_error("Need to specify a Brody growth coefficient in 'K'")

  # ----- test wrong input types
  K <- -1
  rFAMS:::iCheckK(K) |>
    expect_error("'K' must be >=0")
  K <- "a"
  rFAMS:::iCheckK(K) |>
    expect_error("'K' must be a number")
  K <- c(0.3,0.5)
  rFAMS:::iCheckK(K) |>
    expect_error("Only use one value in 'K'")
  K <- 1
  rFAMS:::iCheckK(K) |>
    expect_warning("A Brody growth coefficient of 1 seems too large")
  K <- 0.01
  rFAMS:::iCheckK(K) |>
    expect_warning("A Brody growth coefficient of 0.01 seems too small")
})

test_that("iCheckt0() messages",{
  # ----- test that something was sent (optname is used in first ex just to test)
  rFAMS:::iCheckt0(optname="t0") |>
    expect_error("Need to specify a time when the mean length is 0 in 't0'")
  t0 <- NULL
  rFAMS:::iCheckt0(t0) |>
    expect_error("Need to specify a time when the mean length is 0 in 't0'")

  # ----- test wrong input types
  t0 <- "a"
  rFAMS:::iCheckt0(t0) |>
    expect_error("'t0' must be a number")
  t0 <- c(0.3,0.5)
  rFAMS:::iCheckt0(t0) |>
    expect_error("Only use one value in 't0'")
})

test_that("iCheckLWb() messages",{
  # ----- test that something was sent (optname is used in first ex just to test)
  rFAMS:::iCheckLWb(optname="LWbeta") |>
    expect_error("Need to specify a weight-length beta coefficient in 'LWbeta'")
  LWbeta <- NULL
  rFAMS:::iCheckLWb(LWbeta) |>
    expect_error("Need to specify a weight-length beta coefficient in 'LWbeta'")

  # ----- test wrong input types
  LWbeta <- -1
  rFAMS:::iCheckLWb(LWbeta) |>
    expect_error("'LWbeta' must be >=0")
  LWbeta <- "a"
  rFAMS:::iCheckLWb(LWbeta) |>
    expect_error("'LWbeta' must be a number")
  LWbeta <- c(0.3,0.5)
  rFAMS:::iCheckLWb(LWbeta) |>
    expect_error("Only use one value in 'LWbeta'")
  LWbeta <- 1
  rFAMS:::iCheckLWb(LWbeta) |>
    expect_warning("A weight-length beta coefficient of 1 seems too small")
  LWbeta <- 4.5
  rFAMS:::iCheckLWb(LWbeta) |>
    expect_warning("A weight-length beta coefficient of 4.5 seems too large")
})

test_that("iCheckLWa() messages",{
  # ----- test that something was sent (optname is used in first ex just to test)
  rFAMS:::iCheckLWa(optname="LWalpha") |>
    expect_error("Need to specify a weight-length alpha coefficient in 'LWalpha'")
  LWalpha <- NULL
  rFAMS:::iCheckLWa(LWalpha) |>
    expect_error("Need to specify a weight-length alpha coefficient in 'LWalpha'")

  # ----- test wrong input types
  LWalpha <- "a"
  rFAMS:::iCheckLWa(LWalpha) |>
    expect_error("'LWalpha' must be a number")
  LWalpha <- c(0.3,0.5)
  rFAMS:::iCheckLWa(LWalpha) |>
    expect_error("Only use one value in 'LWalpha'")
})

test_that("iCheckLHParms() messages",{
  # ----- no errors if created with makeLH() (or vector or list craeted correctly)
  LHparms <- makeLH(N0=100,tmax=15,Linf=300,K=0.3,t0=-0.5,LWalpha=-5.4,LWbeta=3.1)
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_no_error()
  LHparms <- c(N0=100,tmax=15,Linf=300,K=0.3,t0=-0.5,LWalpha=-5.4,LWbeta=3.1)
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_no_error()
  LHparms <- list(N0=100,tmax=15,Linf=300,K=0.3,t0=-0.5,LWalpha=-5.4,LWbeta=3.1)
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_no_error()

  # ----- test if missing argument (optname used in first for testing only)
  rFAMS:::iCheckLHparms(optname="lhparms") |>
    expect_error("Need to specify a list or vector of life history parameters")
  LHparms <- NULL
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("Need to specify a list or vector of life history parameters")

  # ----- test for unnamed vector/list
  LHparms <- c(100,15,300,0.3,-0.5,-5.4,3.1)
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("Life history parameters in 'LHparms' must be named")
  LHparms <- as.list(LHparms)
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("Life history parameters in 'LHparms' must be named")

  # ----- test for missing parameters in vector and then list
  tmp <- c(N0=100,tmax=15,Linf=300,K=0.3,t0=-0.5,LWalpha=-5.4,LWbeta=3.1)
  LHparms <- tmp
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_no_error()
  LHparms <- tmp[-1]
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("'LHparms' is missing these life history parameters: N0")
  LHparms <- tmp[-c(1,3)]
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("'LHparms' is missing these life history parameters: N0, Linf")

  tmp2 <- as.list(tmp)
  LHparms <- tmp2
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_no_error()
  LHparms <- tmp2[-1]
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("'LHparms' is missing these life history parameters: N0")
  LHparms <- tmp2[-c(1,3)]
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("'LHparms' is missing these life history parameters: N0, Linf")

  # ----- test for mis-spelled parameters (treats as if missing)
  LHparms <- tmp
  names(LHparms)[1] <- "no"
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("'LHparms' is missing these life history parameters: N0")
  names(LHparms)[3] <- "LINF"
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("'LHparms' is missing these life history parameters: N0, Linf")

  LHparms <- tmp2
  names(LHparms)[1] <- "no"
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("'LHparms' is missing these life history parameters: N0")
  names(LHparms)[3] <- "LINF"
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("'LHparms' is missing these life history parameters: N0, Linf")

  # ----- test for too many parameters
  LHparms <- c(tmp,"derek"=7)
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("These parameters should not be in 'LHparms': derek")
  LHparms <- as.list(LHparms)
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("These parameters should not be in 'LHparms': derek")

  # ----- test for wrong type
  LHparms <- as.data.frame(tmp)
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("'LHparms' must be a vector or list, not a data.frame")
  LHparms <- as.matrix(tmp,ncol=1)
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("'LHparms' must be a vector or list, not a matrix")
  LHparms <- array(tmp,dim=c(7,1))
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("'LHparms' must be a vector or list, not a matrix")

  # ----- spot test for wrong values type or magnitude ... more thorough tests
  #       are elsewhere; e.g., iCheckLinf(), iCheckN0()
  LHparms <- tmp
  LHparms["N0"] <- -100
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("'N0' must be >=0")
  LHparms <- tmp
  LHparms["LWbeta"] <- 1
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_warning("A weight-length beta coefficient of 1 seems too small")
  LHparms <- tmp2
  LHparms[["Linf"]] <- "Linf"
  rFAMS:::iCheckLHparms(LHparms) |>
    expect_error("'Linf' must be a number")
})


test_that("iCheckCondMort() messages",{
  # ----- test that something was sent (optname is used in first ex just to test)
  rFAMS:::iCheckCondMort(optname="cf") |>
    expect_error("Need to specify a conditional fishing mortality in 'cf'")
  cf <- NULL
  rFAMS:::iCheckCondMort(cf) |>
    expect_error("Need to specify a conditional fishing mortality in 'cf'")

  # ----- test wrong input types or values
  cf <- -1
  rFAMS:::iCheckCondMort(cf) |>
    expect_error("'cf' must be >=0")
  cf <- 2
  rFAMS:::iCheckCondMort(cf) |>
    expect_error("'cf' must be <=1")
  cf <- "a"
  rFAMS:::iCheckCondMort(cf) |>
    expect_error("'cf' must be a number")
  cf <- c(0.3,0.5)
  rFAMS:::iCheckCondMort(cf) |>
    expect_no_error()
  rFAMS:::iCheckCondMort(cf,onlyone=TRUE) |>
    expect_error("Only use one value in 'cf'")
  cf <- c(-0.3,0.5)
  rFAMS:::iCheckCondMort(cf) |>
    expect_error("All 'cf' must be >=0")
  cf <- c(0.3,1.5)
  rFAMS:::iCheckCondMort(cf) |>
    expect_error("All 'cf' must be <=1")

  # ----- test that something was sent
  cm <- NULL
  rFAMS:::iCheckCondMort(cm) |>
    expect_error("Need to specify a conditional natural mortality in 'cm'")

  # ----- test wrong input types or values
  cm <- -1
  rFAMS:::iCheckCondMort(cm) |>
    expect_error("'cm' must be >=0")
  cm <- 2
  rFAMS:::iCheckCondMort(cm) |>
    expect_error("'cm' must be <=1")
  cm <- "a"
  rFAMS:::iCheckCondMort(cm) |>
    expect_error("'cm' must be a number")
  cm <- c(0.3,0.5)
  rFAMS:::iCheckCondMort(cm) |>
    expect_no_error()
  rFAMS:::iCheckCondMort(cm,onlyone=TRUE) |>
    expect_error("Only use one value in 'cm'")
  cm <- c(-0.3,0.5)
  rFAMS:::iCheckCondMort(cm) |>
    expect_error("All 'cm' must be >=0")
  cm <- c(0.3,1.5)
  rFAMS:::iCheckCondMort(cm) |>
    expect_error("All 'cm' must be <=1")

  # ----- test wrong input types or values
  cfunder <- NULL
  rFAMS:::iCheckCondMort(cfunder) |>
    expect_error("Need to specify a conditional fishing mortality under the slot")
  cfin <- NULL
  rFAMS:::iCheckCondMort(cfin) |>
    expect_error("Need to specify a conditional fishing mortality in the slot")
  cfabove <- NULL
  rFAMS:::iCheckCondMort(cfabove) |>
    expect_error("Need to specify a conditional fishing mortality above the slot")

  # ----- test wrong input types or values
  cfunder <- -1
  rFAMS:::iCheckCondMort(cfunder) |>
    expect_error("'cfunder' must be >=0")
  cfunder <- 2
  rFAMS:::iCheckCondMort(cfunder) |>
    expect_error("'cfunder' must be <=1")
  cfunder <- "a"
  rFAMS:::iCheckCondMort(cfunder) |>
    expect_error("'cfunder' must be a number")
  cfunder <- c(0.3,0.5)
  rFAMS:::iCheckCondMort(cfunder) |>
    expect_no_error()
  rFAMS:::iCheckCondMort(cfunder,onlyone=TRUE) |>
    expect_error("Only use one value in 'cfunder'")
  cfunder <- c(-0.3,0.5)
  rFAMS:::iCheckCondMort(cfunder) |>
    expect_error("All 'cfunder' must be >=0")
  cfunder <- c(0.3,1.5)
  rFAMS:::iCheckCondMort(cfunder) |>
    expect_error("All 'cfunder' must be <=1")
})

test_that("iCheckMLH() messages",{
  # ----- test that something was sent (optname is used in first ex just to test)
  rFAMS:::iCheckMLH(Linf=600,optname="minLL") |>
    expect_error("Need to specify a minimum length \\(mm\\) limit for harvest")
  minLL <- NULL
  rFAMS:::iCheckMLH(minLL,Linf=600) |>
    expect_error("Need to specify a minimum length \\(mm\\) limit for harvest")

  # ----- test wrong input types or values
  minLL <- -1
  rFAMS:::iCheckMLH(minLL,Linf=600) |>
    expect_error("'minLL' must be >=0")
  minLL <- "a"
  rFAMS:::iCheckMLH(minLL,Linf=600) |>
    expect_error("'minLL' must be a number")
  minLL <- c(200,300)
  rFAMS:::iCheckMLH(minLL,Linf=600) |>
    expect_no_error()
  rFAMS:::iCheckMLH(minLL,Linf=600,onlyone=TRUE) |>
    expect_error("Only use one value in 'minLL'")
  minLL <- 25
  rFAMS:::iCheckMLH(minLL,Linf=600) |>
    expect_warning("A minimum length limit of harvest of 25 mm seems too small")
  minLL <- c(25,300)
  rFAMS:::iCheckMLH(minLL,Linf=600) |>
    expect_warning("A minimum length limit of harvest of 25 mm seems too small")
  minLL <- c(10,25,300)
  rFAMS:::iCheckMLH(minLL,Linf=600) |>
    expect_warning("A minimum length limit of harvest of 25 mm seems too small")
  minLL <- 2000
  rFAMS:::iCheckMLH(minLL,Linf=2200) |>
    expect_warning("A minimum length limit of harvest of 2000 mm seems too large")
  minLL <- c(300,2000)
  rFAMS:::iCheckMLH(minLL,Linf=2200) |>
    expect_warning("A minimum length limit of harvest of 2000 mm seems too large")
  minLL <- c(300,2000,3000)
  rFAMS:::iCheckMLH(minLL,Linf=3200) |>
    expect_warning("A minimum length limit of harvest of 2000 mm seems too large")
  minLL <- 750
  rFAMS:::iCheckMLH(minLL,Linf=600) |>
    expect_error("A minimum length limit of harvest cannot be more than Linf")
  minLL <- c(300,750)
  rFAMS:::iCheckMLH(minLL,Linf=600) |>
    expect_error("A minimum length limit of harvest cannot be more than Linf")
  minLL <- c(300,750,900)
  rFAMS:::iCheckMLH(minLL,Linf=600) |>
    expect_error("A minimum length limit of harvest cannot be more than Linf")
})

test_that("iCheckloi() messages",{
  # ----- test wrong input types or values
  loi <- -1
  rFAMS:::iCheckloi(loi) |>
    expect_error("'loi' must be >=0")
  loi <- "a"
  rFAMS:::iCheckloi(loi) |>
    expect_error("'loi' must be a number")
  loi <- c(-200,300)
  rFAMS:::iCheckloi(loi) |>
    expect_error("All 'loi' must be >=0")
  loi <- data.frame(loi=200)
  rFAMS:::iCheckloi(loi) |>
    expect_error("'loi' must be a vector")
})

test_that("iCheckSlotType() messages",{
  # !!!!! Checked for these cases ...
  # Case recTL un in ab RESULT
  # 1    any   0  0  0  STOP ... need some mortality
  # 2    ####  ## ## ## STOP ... can't have all, implied protected slot
  # 3    NULL  ## ## ## STOP ... can't have all, implied inverse slot
  # 4    ####  0  ## 0  STOP ... don't use recruitmentTL for harvest slot
  # 5    NULL  ## 0  ## STOP ... need recruitmentTL for protected slot
  # 6    ####  ## ## 0  STOP ... implied protected slot, can't have cfin
  # 7    NULL  ## ## 0  STOP ... implied harvest slot, can't have cfunder
  # 8    ####  0  ## ## STOP ... implied protected slot, can't have cfin
  # 9    NULL  0  ## ## STOP ... implied harvest slot, can't have cfabove
  # 10   ####  ## 0  0  STOP ... implied protected slot, also need cfabove
  # 11   NULL  ## 0  0  STOP ... implied harvest slot, need cfin and no cfunder
  # 12   ####  ## 0  0  STOP ... implied protected slot, also need cfabove
  # 13   NULL  0  0  ## STOP ... implied harvest slot, need cfin and no cfabove
  # 14   ####  0  0  ## STOP ... implied protected slot, also need cfunder
  # 14   ####  ## 0  ## GOOD ... protected slot
  # 15   NULL  0  ## 0  GOOD ... inverse slot

  ## ----- no potential issues
  recruitmentTL <- 200;  cfunder <- 0.2; cfin <- 0;   cfabove <- 0.3  # Case 15
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_no_error()
  recruitmentTL <- NULL; cfunder <- 0;   cfin <- 0.3; cfabove <- 0    # Case 16
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_no_error()

  ## ----- can't be all zeroes
  recruitmentTL <- 200;  cfunder <- 0;   cfin <- 0;   cfabove <- 0    # Case #1a
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_error("'cfunder', 'cfin', and 'cfabove' cannot all =0")
  recruitmentTL <- NULL; cfunder <- 0;   cfin <- 0;   cfabove <- 0    # Case #1b
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_error("'cfunder', 'cfin', and 'cfabove' cannot all =0")

  ## ----- can't have recruitmenT=NULL when cfunder is provided
  recruitmentTL <- NULL; cfunder <- 0.2; cfin <- 0.3; cfabove <- 0.4  # Case #3
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_error("If 'cfunder'>0 then a value must be given to")
  recruitmentTL <- NULL; cfunder <- 0.2; cfin <- 0;   cfabove <- 0.4  # Case #5
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_error("If 'cfunder'>0 then a value must be given to")
  recruitmentTL <- NULL; cfunder <- 0.2; cfin <- 0.3; cfabove <- 0    # Case #7
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_error("If 'cfunder'>0 then a value must be given to")
  recruitmentTL <- NULL; cfunder <- 0.2; cfin <- 0;   cfabove <- 0    # Case #11
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_error("If 'cfunder'>0 then a value must be given to")

  ## ----- check a few that will err when strict=TRUE, but not when strict=FALSE
  recruitmentTL <- 200;  cfunder <- 0.2; cfin <- 0.3; cfabove <- 0.4  # Case #2
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_no_error()
  recruitmentTL <- 200;  cfunder <- 0;   cfin <- 0.3; cfabove <- 0    # Case #4
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_no_error()
  recruitmentTL <- 200;  cfunder <- 0.2; cfin <- 0.3; cfabove <- 0    # Case #6
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_no_error()
  recruitmentTL <- 200;  cfunder <- 0;   cfin <- 0.3; cfabove <- 0.4  # Case #8
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_no_error()
  recruitmentTL <- NULL; cfunder <- 0;   cfin <- 0.3; cfabove <- 0.4  # Case #9
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_no_error()
  recruitmentTL <- 200;  cfunder <- 0.2; cfin <- 0;   cfabove <- 0    # Case #10
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_no_error()
  recruitmentTL <- 200;  cfunder <- 0.2; cfin <- 0;   cfabove <- 0    # Case #12
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_no_error()
  recruitmentTL <- NULL; cfunder <- 0;   cfin <- 0;   cfabove <- 0.3  # Case #13
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_no_error()
  recruitmentTL <- 200;  cfunder <- 0;   cfin <- 0;   cfabove <- 0.3  # Case #14
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL) |>
    expect_no_error()

#!!!!! Did not check against specific error messages for all situations because
#      the messages are long and they wrap differently depending on where the
#      tests are made (i.e., varies by computer).
  recruitmentTL <- 200;  cfunder <- 0;   cfin <- 0;   cfabove <- 0    # Case #1a
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("'cfunder', 'cfin', and 'cfabove' cannot all =0")
  recruitmentTL <- NULL; cfunder <- 0;   cfin <- 0;   cfabove <- 0    # Case #1b
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("'cfunder', 'cfin', and 'cfabove' cannot all =0")
  recruitmentTL <- 200;  cfunder <- 0.2; cfin <- 0.3; cfabove <- 0.4  # Case #2
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("")
  recruitmentTL <- NULL; cfunder <- 0.2; cfin <- 0.3; cfabove <- 0.4  # Case #3
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("")
  recruitmentTL <- 200;  cfunder <- 0;   cfin <- 0.3; cfabove <- 0    # Case #4
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("")
  recruitmentTL <- NULL; cfunder <- 0.2; cfin <- 0;   cfabove <- 0.4  # Case #5
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("")
  recruitmentTL <- 200;  cfunder <- 0.2; cfin <- 0.3; cfabove <- 0    # Case #6
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("")
  recruitmentTL <- NULL; cfunder <- 0.2; cfin <- 0.3; cfabove <- 0    # Case #7
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("")
  recruitmentTL <- 200;  cfunder <- 0;   cfin <- 0.3; cfabove <- 0.4  # Case #8
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("")
  recruitmentTL <- NULL; cfunder <- 0;   cfin <- 0.3; cfabove <- 0.4  # Case #9
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("")
  recruitmentTL <- 200;  cfunder <- 0.2; cfin <- 0;   cfabove <- 0    # Case #10
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("")
  recruitmentTL <- NULL; cfunder <- 0.2; cfin <- 0;   cfabove <- 0    # Case #11
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("")
  recruitmentTL <- 200;  cfunder <- 0.2; cfin <- 0;   cfabove <- 0    # Case #12
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("")
  recruitmentTL <- NULL; cfunder <- 0;   cfin <- 0;   cfabove <- 0.3  # Case #13
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("")
  recruitmentTL <- 200;  cfunder <- 0;   cfin <- 0;   cfabove <- 0.3  # Case #14
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_error("")
  recruitmentTL <- 200;  cfunder <- 0.2; cfin <- 0;   cfabove <- 0.3  # Case #15
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_no_error()
  recruitmentTL <- NULL; cfunder <- 0;   cfin <- 0.3; cfabove <- 0    # Case #16
  rFAMS:::iCheckSlotType(cfunder,cfin,cfabove,recruitmentTL,strict=TRUE) |>
    expect_no_error()
})

test_that("iCheckRecruitmentTL() messages",{
  recruitmentTL <- NULL
  rFAMS:::iCheckRecruitmentTL(recruitmentTL,Linf=250,lowerSL=100) |>
    expect_no_error()

  # ----- test wrong input types or values
  recruitmentTL <- -1
  rFAMS:::iCheckRecruitmentTL(recruitmentTL,Linf=250,lowerSL=100) |>
    expect_error("'recruitmentTL' must be >=0")
  recruitmentTL <- "a"
  rFAMS:::iCheckRecruitmentTL(recruitmentTL,Linf=250,lowerSL=100) |>
    expect_error("'recruitmentTL' must be a number")
  recruitmentTL <- c(200,300)
  rFAMS:::iCheckRecruitmentTL(recruitmentTL,Linf=250,lowerSL=100) |>
    expect_error("Only use one value in 'recruitmentTL'")
  recruitmentTL <- 300
  rFAMS:::iCheckRecruitmentTL(recruitmentTL,Linf=250,lowerSL=100) |>
    expect_error("'recruitmentTL' cannot be greater than 'Linf'")
  recruitmentTL <- 25
  rFAMS:::iCheckRecruitmentTL(recruitmentTL,Linf=250,lowerSL=100) |>
    expect_warning("'recruitmentTL' of 25 mm seems too small")
  recruitmentTL <- 2000
  rFAMS:::iCheckRecruitmentTL(recruitmentTL,Linf=2500,lowerSL=2200) |>
    expect_warning("'recruitmentTL' of 2000 mm seems too large")
  recruitmentTL <- 200
  rFAMS:::iCheckRecruitmentTL(recruitmentTL,Linf=250,lowerSL=150) |>
    expect_error("'recruitmentTL' cannot be greater than 'lowerSL'")
})

test_that("iCheckSlottTL() messages",{
  # ----- test that something was sent (optname is used in first exs just to test)
  rFAMS:::iCheckSlotTL(Linf=250,optname="lowerSL") |>
    expect_error("Need to specify a lower slot limit total length")
  rFAMS:::iCheckSlotTL(Linf=250,optname="upperSL") |>
    expect_error("Need to specify a upper slot limit total length")
  lowerSL <- NULL
  rFAMS:::iCheckSlotTL(lowerSL,Linf=250) |>
    expect_error("Need to specify a lower slot limit total length")
  upperSL <- NULL
  rFAMS:::iCheckSlotTL(upperSL,Linf=250) |>
    expect_error("Need to specify a upper slot limit total length")

  # ----- test wrong input types or values
  # ..... lowerSL values
  lowerSL <- -1
  rFAMS:::iCheckSlotTL(lowerSL,Linf=250) |>
    expect_error("'lowerSL' must be >=0")
  lowerSL <- "a"
  rFAMS:::iCheckSlotTL(lowerSL,Linf=250) |>
    expect_error("'lowerSL' must be a number")
  lowerSL <- c(200,300)
  rFAMS:::iCheckSlotTL(lowerSL,Linf=250) |>
    expect_error("Only use one value in 'lowerSL'")
  lowerSL <- 300
  rFAMS:::iCheckSlotTL(lowerSL,Linf=250) |>
    expect_error("The lower slot limit total length \\(=300\\) mm cannot be greater")
  lowerSL <- 25
  rFAMS:::iCheckSlotTL(lowerSL,Linf=250) |>
    expect_warning("A lower slot limit total length of 25 mm seems too small")
  lowerSL <- 2000
  rFAMS:::iCheckSlotTL(lowerSL,Linf=2500) |>
    expect_warning("A lower slot limit total length of 2000 mm seems too large")

  # ..... upperSL values
  upperSL <- -1
  rFAMS:::iCheckSlotTL(upperSL,Linf=250) |>
    expect_error("'upperSL' must be >=0")
  upperSL <- "a"
  rFAMS:::iCheckSlotTL(upperSL,Linf=250) |>
    expect_error("'upperSL' must be a number")
  upperSL <- c(200,300)
  rFAMS:::iCheckSlotTL(upperSL,Linf=250) |>
    expect_error("Only use one value in 'upperSL'")
  upperSL <- 300
  rFAMS:::iCheckSlotTL(upperSL,Linf=250) |>
    expect_error("The upper slot limit total length \\(=300\\) mm cannot be greater")
  upperSL <- 25
  rFAMS:::iCheckSlotTL(upperSL,Linf=250) |>
    expect_warning("A upper slot limit total length of 25 mm seems too small")
  upperSL <- 2000
  rFAMS:::iCheckSlotTL(upperSL,Linf=2500) |>
    expect_warning("A upper slot limit total length of 2000 mm seems too large")
})




## Continue with the rest of the internals ##


## =============================================================================
## ==== OLD CAN PROBABLY BE DELETED
## =============================================================================

# test_that("iCheckMLHinc() messages and values",{
#   expect_error(rFAMS:::iCheckMLHinc(),
#                "Need to specify an increment for minimum length")
#   ## Set MLHinc to value outside function to test that name is extracted
#   MLHmin <- 100; MLHmax <- 900
#   MLHinc <- -100
#   expect_error(rFAMS:::iCheckMLHinc(MLHinc,MLHmin,MLHmax),"must be >=0")
#   MLHinc <- "a"
#   expect_error(rFAMS:::iCheckMLHinc(MLHinc,MLHmin,MLHmax),"must be a number")
#   MLHinc <- c(300,500)
#   expect_error(rFAMS:::iCheckMLHinc(MLHinc,MLHmin,MLHmax),"Only use one value in")
#
#   ## Problems with MLHmin and MLHmax
#   MLHmin <- 900; MLHmax <- 100; MLHinc <- 100
#   expect_error(rFAMS:::iCheckMLHinc(MLHinc,MLHmin,MLHmax),
#                "'MLHmin' must be equal to or less than 'MLHmax'")
#   MLHmin <- 100; MLHmax <- 900; MLHinc <- 1
#   expect_warning(tmp <- rFAMS:::iCheckMLHinc(MLHinc,MLHmin,MLHmax),
#                  "Choices of 'MLHmin', 'MLHmax', and 'MLHinc' resulted in")
#
#   ## Values returned
#   expect_equal(class(tmp),"numeric")
#   expect_equal(length(tmp),801)
# })

# test_that("iCheckcfminc() messages and values",{
#   expect_error(rFAMS:::iCheckcfminc(),
#                "Need to specify an increment for conditional natural mortality in")
#   ## Set cfinc to value outside function to test that name is extracted
#   cfmin <- 0.1; cfmax <- 0.9
#   cfinc <- -0.1
#   expect_error(rFAMS:::iCheckcfminc(cfinc,cfmin,cfmax),"must be >=0")
#   cfinc <- 2
#   expect_error(rFAMS:::iCheckcfminc(cfinc,cfmin,cfmax),"must be <=1")
#   cfinc <- "a"
#   expect_error(rFAMS:::iCheckcfminc(cfinc,cfmin,cfmax),"must be a number")
#   cfinc <- c(0.3,0.5)
#   expect_error(rFAMS:::iCheckcfminc(cfinc,cfmin,cfmax),"Only use one value in")
#
#   ## Problems with cfmin and cfmax
#   cfmin <- 0.9; cfmax <- 0.1; cfinc <- 0.1
#   expect_error(rFAMS:::iCheckcfminc(cfinc,cfmin,cfmax),
#                "'cfmin' must be equal to or less than 'cfmax'")
#   cfmin <- 0.1; cfmax <- 0.9; cfinc <- 0.001
#   expect_warning(tmp <- rFAMS:::iCheckcfminc(cfinc,cfmin,cfmax),
#                         "Choices of 'cfmin', 'cfmax', and 'cfinc' resulted in")
#
#   ## Values returned
#   expect_equal(class(tmp),"numeric")
#   expect_equal(length(tmp),801)
# })

# test_that("iCheckcf() messages",{
#   rFAMS:::iCheckcf() |>
#     expect_error("Need to specify a conditional fishing mortality in")
#   ## Set cf to value outside function to test that name is extracted
#   cf <- -1
#   rFAMS:::iCheckcf(cf) |>
#     expect_error("'cf' must be >=0")
#   cf <- 2
#   rFAMS:::iCheckcf(cf) |>
#     expect_error("'cf' must be <=1")
#   cf <- "a"
#   rFAMS:::iCheckcf(cf) |>
#     expect_error("'cf' must be a number")
#   cf <- c(0.3,0.5)
#   rFAMS:::iCheckcf(cf) |>
#     expect_error("Only use one value in 'cf'")
#   ## test function for work with cfmin and cfmax
#   cfmin <- -1
#   rFAMS:::iCheckcf(cfmin) |>
#     expect_error("'cfmin' must be >=0")
#   cfmax <- "a"
#   rFAMS:::iCheckcf(cfmax) |>
#     expect_error("'cfmax' must be a number")
# })

# test_that("iCheckcm() messages",{
#   rFAMS:::iCheckcm() |>
#     expect_error("Need to specify aNULL conditional natural mortality in")
#   ## Set cm to value outside function to test that name is extracted
#   cm <- -1
#   rFAMS:::iCheckcm(cm) |>
#     expect_error("'cm' must be >=0")
#   cm <- 2
#   rFAMS:::iCheckcm(cm) |>
#     expect_error("'cm' must be <=1")
#   cm <- "a"
#   rFAMS:::iCheckcm(cm) |>
#     expect_error("'cm' must be a number")
#   cm <- c(0.3,0.5)
#   rFAMS:::iCheckcm(cm) |>
#     expect_error("Only use one value in 'cm'")
#   ## test function for work with cmmin and cmmax
#   cmmin <- -1
#   rFAMS:::iCheckcm(cmmin) |>
#     expect_error("'cmmin' must be >=0")
#   cmmax <- "a"
#   rFAMS:::iCheckcm(cmmax) |>
#     expect_error("'cmmax' must be a number")
# })
