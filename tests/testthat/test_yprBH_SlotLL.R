## ===== Test Message Errors and Warnings ======================================
test_that("yprBH_SlotLL() messages",{
  LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
  cm <- c(0.3,0.4,0.5)

  # ----- no errors or warnings
  # ..... protected slot
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_no_error()
  # ..... inverse/harvest slot
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=0.25,cfabove=0,cm=cm,
               lhparms=LH) |>
    expect_no_error()

  # ----- test for missing arguments
  # ..... protected slot
  yprBH_SlotLL(upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("Need to specify a lower slot limit total length")
  yprBH_SlotLL(lowerSL=250,
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("Need to specify a upper slot limit total length")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("Need to specify a conditional fishing mortality under")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("Need to specify a conditional fishing mortality in")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("Need to specify a conditional fishing mortality above")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.15,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("Need to specify a conditional natural mortality in 'cm'")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               recruitmentTL=200) |>
    expect_error("Need to specify a list or vector of life history parameters")

  # ----- test for bad values
  # ..... in lowerSL
  yprBH_SlotLL(lowerSL=-250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'lowerSL' must be >=0")
  yprBH_SlotLL(lowerSL="a",upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'lowerSL' must be a number")
  yprBH_SlotLL(lowerSL=c(200,250),upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("Only use one value in 'lowerSL'")
  yprBH_SlotLL(lowerSL=350,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'lowerSL' must be less than 'upperSL'")
  yprBH_SlotLL(lowerSL=600,upperSL=625,
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("The lower slot limit total length \\(=600\\) mm cannot be")
  # ..... in upperSL
  yprBH_SlotLL(lowerSL=250,upperSL=-325,
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'upperSL' must be >=0")
  yprBH_SlotLL(lowerSL=250,upperSL="a",
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'upperSL' must be a number")
  yprBH_SlotLL(lowerSL=250,upperSL=c(325,350),
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("Only use one value in 'upperSL'")
  yprBH_SlotLL(lowerSL=350,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'lowerSL' must be less than 'upperSL'")
  yprBH_SlotLL(lowerSL=300,upperSL=600,
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("The upper slot limit total length \\(=600\\) mm cannot be")
  # ..... in cfunder
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=-0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'cfunder' must be >=0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=1.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'cfunder' must be <=1")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder="a",cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'cfunder' must be a number")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=c(0.25,0.3),cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("Only use one value in 'cfunder'")
  # ..... in cfabove
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=-0.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'cfabove' must be >=0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=1.15,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'cfabove' must be <=1")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove="a",cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'cfabove' must be a number")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=c(0.25,0.3),cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("Only use one value in 'cfabove'")
  # ..... in cfin (switch to inverse/harvest slot)
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=-0.3,cfabove=0,cm=cm,
               lhparms=LH) |>
    expect_error("'cfin' must be >=0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=1.3,cfabove=0,cm=cm,
               lhparms=LH) |>
    expect_error("'cfin' must be <=1")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin="a",cfabove=0,cm=cm,
               lhparms=LH) |>
    expect_error("'cfin' must be a number")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=c(0.3,0.4),cfabove=0,cm=cm,
               lhparms=LH) |>
    expect_error("Only use one value in 'cfin'")
  # ..... in cfunder, cfabove, cfin relatedly (did not check against specific
  #       error messages as they are long and wrap diffrently on differnet OS)
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.2,cfin=0.3,cfabove=0.4,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error()
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.2,cfin=0.3,cfabove=0.4,cm=cm,
               lhparms=LH) |>
    expect_error()
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=0,cfabove=0,cm=cm,
               lhparms=LH) |>
    expect_error()
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=0,cfabove=0,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error()
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.2,cfin=0.2,cfabove=0,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error()
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.2,cfin=0.2,cfabove=0,cm=cm,
               lhparms=LH,recruitmentTL=NULL) |>
    expect_error()
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=0.2,cfabove=0.2,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error()
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=0.2,cfabove=0.2,cm=cm,
               lhparms=LH,recruitmentTL=NULL) |>
    expect_error()
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.2,cfin=0,cfabove=0,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error()
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.2,cfin=0,cfabove=0,cm=cm,
               lhparms=LH,recruitmentTL=NULL) |>
    expect_error()
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=0,cfabove=0.2,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error()
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=0,cfabove=0.2,cm=cm,
               lhparms=LH,recruitmentTL=NULL) |>
    expect_error()
  # ..... in cm
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.2,cm=-0.2,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'cm' must be >=0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.2,cm=c(-0.2,0.2),
               lhparms=LH,recruitmentTL=200) |>
    expect_error("All 'cm' must be >=0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.2,cm=1.2,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'cm' must be <=1")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.2,cm=c(0.2,1.2),
               lhparms=LH,recruitmentTL=200) |>
    expect_error("All 'cm' must be <=1")
  # ..... in recruitmentTL
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.2,cm=0.2,
               lhparms=LH,recruitmentTL=-200) |>
    expect_error("'recruitmentTL' must be >=0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.2,cm=0.2,
               lhparms=LH,recruitmentTL=c(200,300)) |>
    expect_error("Only use one value in 'recruitmentTL'")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.2,cm=0.2,
               lhparms=LH,recruitmentTL="a") |>
    expect_error("'recruitmentTL' must be a number")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.2,cm=0.2,
               lhparms=LH,recruitmentTL=600) |>
    expect_error("'recruitmentTL' cannot be greater than 'Linf'")
  # ..... spot tests for bad values in lhparms ... more thorough testing is
  #       elsewhere; e.g., iCheckLinf(), iCheckN0()
  tmp <- list(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
  LH <- tmp
  LH["N0"] <- -100
  yprBH_SlotLL(lowerSL=250,upperSL=325,cfunder=0.25,cfin=0,cfabove=0.2,cm=0.2,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'N0' must be >=0")
  LH <- list(N0=100,tmax=15,Linf=592,K=c(0.3,0.5),t0=-0.3,
             LWalpha=-5.528,LWbeta=3.273)
  yprBH_SlotLL(lowerSL=250,upperSL=325,cfunder=0.25,cfin=0,cfabove=0.2,cm=0.2,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("Only use one value in 'K'")
  LH <- tmp
  LH["LWbeta"] <- 5
  yprBH_SlotLL(lowerSL=250,upperSL=325,cfunder=0.25,cfin=0,cfabove=0.2,cm=0.2,
               lhparms=LH,recruitmentTL=200) |>
    expect_warning("A weight-length beta coefficient of 5 seems too large") |>
    expect_warning("A weight-length beta coefficient of 5 seems too large")
  ## !!! May want to address this double warning
})


## ===== Get Some Results for Use Below ========================================
LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
lowerSL <- 250
upperSL <- 325
cfunder <- c(p=0.25,h=0)
cfin <- c(p=0,h=0.25)
cfabove <- c(p=0.2,h=0)
recruitmentTLp <- 200
recruitmentTLh <- NULL
cm <- c(0.3,0.4,0.5)
lois <- c(300,400)

slotLL1p <- yprBH_SlotLL(lowerSL=lowerSL,upperSL=upperSL,cm=cm[1],
                         cfunder=cfunder["p"],cfin=cfin["p"],cfabove=cfabove["p"],
                         lhparms=LH,loi=lois,recruitmentTL=recruitmentTLp)
slotLL1h <- yprBH_SlotLL(lowerSL=lowerSL,upperSL=upperSL,cm=cm[1],
                         cfunder=cfunder["h"],cfin=cfin["h"],cfabove=cfabove["h"],
                         lhparms=LH,loi=lois,recruitmentTL=recruitmentTLh)
slotLL2p <- yprBH_SlotLL(lowerSL=lowerSL,upperSL=upperSL,cm=cm,
                         cfunder=cfunder["p"],cfin=cfin["p"],cfabove=cfabove["p"],
                         lhparms=LH,loi=lois,recruitmentTL=recruitmentTLp)
slotLL2h <- yprBH_SlotLL(lowerSL=lowerSL,upperSL=upperSL,cm=cm,
                         cfunder=cfunder["h"],cfin=cfin["h"],cfabove=cfabove["h"],
                         lhparms=LH,loi=lois,recruitmentTL=recruitmentTLh)

exp_nms1 <- c("yieldTotal","yieldUnder","yieldIn","yieldAbove",
              "nharvTotal","ndieTotal","nharvestUnder","nharvestIn","nharvestAbove",
              "n0die","ndieUnder","ndieIn","ndieAbove","nrUnder","nrIn","nrAbove",
              "trUnder","trIn","trOver","avglenUnder","avglenIn","avglenAbove",
              "avgwtUnder","avgwtIn","avgwtAbove","nAt300","nAt400","cm","expUnder",
              "expIn","expAbove","FUnder","FIn","FAbove","MUnder","MIn","MAbove",
              "ZUnder","ZIn","ZAbove","SUnder","SIn","SAbove","cfUnder","cfIn",
              "cfOver","recruitmentTL","lowerSL","upperSL","N0","Linf","K","t0",
              "LWalpha","LWbeta","tmax")
exp_rows1 <- 1
exp_rows2 <- length(cm)


## ===== Test Output Types =====================================================
test_that("yprBH_SlotLL() output",{
  # ----- tests of single sim protected slot with lhparms list and loi
  # ..... data types, sizes, and names
  expect_type(slotLL1p,"list")
  expect_equal(class(slotLL1p),"data.frame")
  expect_equal(nrow(slotLL1p),exp_rows1)
  expect_equal(ncol(slotLL1p),length(exp_nms1))
  expect_equal(names(slotLL1p),exp_nms1)

  # ..... test repetitive (non-calculated) values equal what was expected
  expect_equal(unique(slotLL1p$cm),cm[1])
  expect_true(all(slotLL1p$recruitmentTL==recruitmentTLp))
  expect_true(all(slotLL1p$cfUnder==cfunder[["p"]]))
  expect_true(all(slotLL1p$cfIn==cfin[["p"]]))
  expect_true(all(slotLL1p$cfAbove==cfabove[["p"]]))
  expect_true(all(slotLL1p$N0==LH$N0))
  expect_true(all(slotLL1p$Linf==LH$Linf))
  expect_true(all(slotLL1p$K==LH$K))
  expect_true(all(slotLL1p$t0==LH$t0))
  expect_true(all(slotLL1p$LWalpha==LH$LWalpha))
  expect_true(all(slotLL1p$LWbeta==LH$LWbeta))
  expect_true(all(slotLL1p$tmax==LH$tmax))

  # ..... values compute "in" the slot should all be 0
  expect_true(all(slotLL1p$yieldIn==0))
  expect_true(all(slotLL1p$nHarvestIn==0))
  expect_true(all(slotLL1p$avglenIn==0))
  expect_true(all(slotLL1p$avgwtIn==0))
  expect_true(all(slotLL1p$FIn==0))
  expect_true(all(slotLL1p$cmIn==0))

  # ----- tests of single sim inverse/harvest slot with lhparms list and loi
  # ..... data types, sizes, and names
  expect_type(slotLL1h,"list")
  expect_equal(class(slotLL1h),"data.frame")
  expect_equal(nrow(slotLL1h),exp_rows1)
  expect_equal(ncol(slotLL1h),length(exp_nms1))
  expect_equal(names(slotLL1h),exp_nms1)

  # ..... test repetitive (non-calculated) values equal what was expected
  expect_equal(unique(slotLL1h$cm),cm[1])
  expect_true(all(slotLL1h$recruitmentTL==recruitmentTLh))
  expect_true(all(slotLL1h$cfUnder==cfunder[["h"]]))
  expect_true(all(slotLL1h$cfIn==cfin[["h"]]))
  expect_true(all(slotLL1h$cfAbove==cfabove[["h"]]))
  expect_true(all(slotLL1h$N0==LH$N0))
  expect_true(all(slotLL1h$Linf==LH$Linf))
  expect_true(all(slotLL1h$K==LH$K))
  expect_true(all(slotLL1h$t0==LH$t0))
  expect_true(all(slotLL1h$LWalpha==LH$LWalpha))
  expect_true(all(slotLL1h$LWbeta==LH$LWbeta))
  expect_true(all(slotLL1h$tmax==LH$tmax))

  # ..... values compute "under" and "above" the slot should all be 0
  expect_true(all(slotLL1h$yieldUnder==0))
  expect_true(all(slotLL1h$nHarvestUnder==0))
  expect_true(all(slotLL1h$avglenUnder==0))
  expect_true(all(slotLL1h$avgwtUnder==0))
  expect_true(all(slotLL1h$FUnder==0))
  expect_true(all(slotLL1h$cmUnder==0))
  expect_true(all(slotLL1h$yieldabove==0))
  expect_true(all(slotLL1h$nHarvestabove==0))
  expect_true(all(slotLL1h$avglenabove==0))
  expect_true(all(slotLL1h$avgwtabove==0))
  expect_true(all(slotLL1h$Fabove==0))
  expect_true(all(slotLL1h$cmabove==0))

  # ----- tests of multiple sim protected slot with lhparms list and loi
  # ..... data types, sizes, and names
  expect_type(slotLL2p,"list")
  expect_equal(class(slotLL2p),"data.frame")
  expect_equal(nrow(slotLL2p),exp_rows2)
  expect_equal(ncol(slotLL2p),length(exp_nms1))
  expect_equal(names(slotLL2p),exp_nms1)

  # ..... test repetitive (non-calculated) values equal what was expected
  expect_equal(unique(slotLL2p$cm),cm)
  expect_true(all(slotLL2p$recruitmentTL==recruitmentTLp))
  expect_true(all(slotLL2p$cfUnder==cfunder[["p"]]))
  expect_true(all(slotLL2p$cfIn==cfin[["p"]]))
  expect_true(all(slotLL2p$cfAbove==cfabove[["p"]]))
  expect_true(all(slotLL2p$N0==LH$N0))
  expect_true(all(slotLL2p$Linf==LH$Linf))
  expect_true(all(slotLL2p$K==LH$K))
  expect_true(all(slotLL2p$t0==LH$t0))
  expect_true(all(slotLL2p$LWalpha==LH$LWalpha))
  expect_true(all(slotLL2p$LWbeta==LH$LWbeta))
  expect_true(all(slotLL2p$tmax==LH$tmax))

  # ..... values compute "in" the slot should all be 0
  expect_true(all(slotLL2p$yieldIn==0))
  expect_true(all(slotLL2p$nHarvestIn==0))
  expect_true(all(slotLL2p$avglenIn==0))
  expect_true(all(slotLL2p$avgwtIn==0))
  expect_true(all(slotLL2p$FIn==0))
  expect_true(all(slotLL2p$cmIn==0))

  # ----- tests of single sim inverse/harvest slot with lhparms list and loi
  # ..... data types, sizes, and names
  expect_type(slotLL2h,"list")
  expect_equal(class(slotLL2h),"data.frame")
  expect_equal(nrow(slotLL2h),exp_rows2)
  expect_equal(ncol(slotLL2h),length(exp_nms1))
  expect_equal(names(slotLL2h),exp_nms1)

  # ..... test repetitive (non-calculated) values equal what was expected
  expect_equal(unique(slotLL2h$cm),cm)
  expect_true(all(slotLL2h$recruitmentTL==recruitmentTLh))
  expect_true(all(slotLL2h$cfUnder==cfunder[["h"]]))
  expect_true(all(slotLL2h$cfIn==cfin[["h"]]))
  expect_true(all(slotLL2h$cfAbove==cfabove[["h"]]))
  expect_true(all(slotLL2h$N0==LH$N0))
  expect_true(all(slotLL2h$Linf==LH$Linf))
  expect_true(all(slotLL2h$K==LH$K))
  expect_true(all(slotLL2h$t0==LH$t0))
  expect_true(all(slotLL2h$LWalpha==LH$LWalpha))
  expect_true(all(slotLL2h$LWbeta==LH$LWbeta))
  expect_true(all(slotLL2h$tmax==LH$tmax))

  # ..... values compute "under" and "above" the slot should all be 0
  expect_true(all(slotLL2h$yieldUnder==0))
  expect_true(all(slotLL2h$nHarvestUnder==0))
  expect_true(all(slotLL2h$avglenUnder==0))
  expect_true(all(slotLL2h$avgwtUnder==0))
  expect_true(all(slotLL2h$FUnder==0))
  expect_true(all(slotLL2h$cmUnder==0))
  expect_true(all(slotLL2h$yieldabove==0))
  expect_true(all(slotLL2h$nHarvestabove==0))
  expect_true(all(slotLL2h$avglenabove==0))
  expect_true(all(slotLL2h$avgwtabove==0))
  expect_true(all(slotLL2h$Fabove==0))
  expect_true(all(slotLL2h$cmabove==0))
})

## ===== Test Results Accuracy =================================================
# ----- Run code below to create a snapshot of the two data.frames created here.
#       These are loaded in below to compare current output to previous output.
#       If something errs in the accuracy tests, then the reason should be
#       determined. If the changes makes sense, then run this code to make a
#       new data snapshot for future testing. Either delete the old file or
#       move it to "archived" in "datasnaps".
# ..... Use CTRL-SHIFT-C to uncomment/comment selected lines in RStudio
# !!!!! These don't test true accuracy of results, but will detect if anything
#       has changed since the last "thought-to-be-stable" results.
#
# tmpdir <- paste0(testthat::test_path(),"/datasnaps/")
# dt <- format(Sys.Date(),format="%d_%b_%Y")
# saveRDS(slotLL1p,paste0(tmpdir,"slotLL1p_",dt,".rds"))
# saveRDS(slotLL2p,paste0(tmpdir,"slotLL2p_",dt,".rds"))
# saveRDS(slotLL1h,paste0(tmpdir,"slotLL1h_",dt,".rds"))
# saveRDS(slotLL2h,paste0(tmpdir,"slotLL2h_",dt,".rds"))

test_that("yprBH_SlotLL() results",{
  # get list of files in datasnaps folder
  tmpdir <- paste0(testthat::test_path(),"/datasnaps/")
  tmpfns <- list.files(tmpdir)
  # Load snapshots of "old" (i.e., last stable) outputs
  slotLL1p_old <- readRDS(paste0(tmpdir,tmpfns[grepl("slotLL1p",tmpfns)]))
  slotLL2p_old <- readRDS(paste0(tmpdir,tmpfns[grepl("slotLL2p",tmpfns)]))
  slotLL1h_old <- readRDS(paste0(tmpdir,tmpfns[grepl("slotLL1h",tmpfns)]))
  slotLL2h_old <- readRDS(paste0(tmpdir,tmpfns[grepl("slotLL2h",tmpfns)]))

  # Compare new to "old" data.frames
  expect_equal(slotLL1p,slotLL1p_old)
  expect_equal(slotLL2p,slotLL2p_old)
  expect_equal(slotLL1h,slotLL1h_old)
  expect_equal(slotLL2h,slotLL2h_old)
})
