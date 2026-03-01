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
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH) |>
    expect_error("'cfunder'>0 which implies that you wish to simulate")

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
  # ..... in cfunder, cfabove, cfin relatedly
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.2,cfin=0.3,cfabove=0.4,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'cfunder', 'cfin', and 'cfabove' cannot all be >0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.2,cfin=0.3,cfabove=0.4,cm=cm,
               lhparms=LH) |>
    expect_error("'cfunder', 'cfin', and 'cfabove' cannot all be >0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=0,cfabove=0,cm=cm,
               lhparms=LH) |>
    expect_error("'cfunder', 'cfin', and 'cfabove' cannot all =0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=0,cfabove=0,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("'cfunder', 'cfin', and 'cfabove' cannot all =0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.2,cfin=0.2,cfabove=0,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("If 'cfin'>0 then neither 'cfunder' or 'cfabove' may be >0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.2,cfin=0.2,cfabove=0,cm=cm,
               lhparms=LH,recruitmentTL=NULL) |>
    expect_error("If 'cfin'>0 then neither 'cfunder' or 'cfabove' may be >0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=0.2,cfabove=0.2,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("If 'cfin'>0 then neither 'cfunder' or 'cfabove' may be >0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=0.2,cfabove=0.2,cm=cm,
               lhparms=LH,recruitmentTL=NULL) |>
    expect_error("If 'cfin'>0 then neither 'cfunder' or 'cfabove' may be >0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.2,cfin=0,cfabove=0,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("If 'cfin'=0 then both 'cfunder' and 'cfabove' should be >0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.2,cfin=0,cfabove=0,cm=cm,
               lhparms=LH,recruitmentTL=NULL) |>
    expect_error("If 'cfin'=0 then both 'cfunder' and 'cfabove' should be >0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=0,cfabove=0.2,cm=cm,
               lhparms=LH,recruitmentTL=200) |>
    expect_error("If 'cfin'=0 then both 'cfunder' and 'cfabove' should be >0")
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=0,cfabove=0.2,cm=cm,
               lhparms=LH,recruitmentTL=NULL) |>
    expect_error("If 'cfin'=0 then both 'cfunder' and 'cfabove' should be >0")
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
    expect_error("The recruitment total length \\(=600\\) mm cannot be greater")
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

res1p <- yprBH_SlotLL(lowerSL=lowerSL,upperSL=upperSL,cm=cm,
                      cfunder=cfunder["p"],cfin=cfin["p"],cfabove=cfabove["p"],
                      lhparms=LH,recruitmentTL=recruitmentTLp)
res2p <- yprBH_SlotLL(lowerSL=lowerSL,upperSL=upperSL,cm=cm,
                      cfunder=cfunder["p"],cfin=cfin["p"],cfabove=cfabove["p"],
                      lhparms=LH,loi=lois,recruitmentTL=recruitmentTLp)
res1h <- yprBH_SlotLL(lowerSL=lowerSL,upperSL=upperSL,cm=cm,
                      cfunder=cfunder["h"],cfin=cfin["h"],cfabove=cfabove["h"],
                      lhparms=LH,recruitmentTL=recruitmentTLh)
res2h <- yprBH_SlotLL(lowerSL=lowerSL,upperSL=upperSL,cm=cm,
                      cfunder=cfunder["h"],cfin=cfin["h"],cfabove=cfabove["h"],
                      lhparms=LH,loi=lois,recruitmentTL=recruitmentTLh)

exp_nms2 <- c("yieldTotal","yieldUnder","yieldIn","yieldAbove",
              "nharvTotal","ndieTotal","nharvestUnder","nharvestIn","nharvestAbove",
              "n0die","ndieUnder","ndieIn","ndieAbove","nrUnder","nrIn","nrAbove",
              "trUnder","trIn","trOver","avglenUnder","avglenIn","avglenAbove",
              "avgwtUnder","avgwtIn","avgwtAbove","nAt300","nAt400","cm","expUnder",
              "expIn","expAbove","FUnder","FIn","FAbove","MUnder","MIn","MAbove",
              "ZUnder","ZIn","ZAbove","SUnder","SIn","SAbove","cfUnder","cfIn",
              "cfOver","recruitmentTL","lowerSL","upperSL","N0","Linf","K","t0",
              "LWalpha","LWbeta","tmax")
exp_nms1 <- exp_nms2[!startsWith(exp_nms2,"nAt")]
exp_rows <- length(cm)


## ===== Test Output Types =====================================================
test_that("yprBH_SlotLL() output",{
  # ----- tests of protected slot without loi
  # ..... data types, sizes, and names
  expect_type(res1p,"list")
  expect_equal(class(res1p),"data.frame")
  expect_equal(nrow(res1p),exp_rows)
  expect_equal(ncol(res1p),length(exp_nms1))
  expect_equal(names(res1p),exp_nms1)

  # ..... test repetitive (non-calculated) values equal what was expected
  expect_equal(cm,unique(res1p$cm))
  expect_true(all(res1p$recruitmentTL==recruitmentTLp))
  expect_true(all(res1p$cfUnder==cfunder[["p"]]))
  expect_true(all(res1p$cfIn==cfin[["p"]]))
  expect_true(all(res1p$cfAbove==cfabove[["p"]]))
  expect_true(all(res1p$N0==LH$N0))
  expect_true(all(res1p$Linf==LH$Linf))
  expect_true(all(res1p$K==LH$K))
  expect_true(all(res1p$t0==LH$t0))
  expect_true(all(res1p$LWalpha==LH$LWalpha))
  expect_true(all(res1p$LWbeta==LH$LWbeta))
  expect_true(all(res1p$tmax==LH$tmax))

  # ..... values compute "in" the slot should all be 0
  expect_true(all(res1p$yieldIn==0))
  expect_true(all(res1p$nHarvestIn==0))
  expect_true(all(res1p$avglenIn==0))
  expect_true(all(res1p$avgwtIn==0))
  expect_true(all(res1p$FIn==0))
  expect_true(all(res1p$cmIn==0))

  # ----- tests of protected slot with loi
  # ..... data types, sizes, and names
  expect_type(res2p,"list")
  expect_equal(class(res2p),"data.frame")
  expect_equal(nrow(res2p),exp_rows)
  expect_equal(ncol(res2p),length(exp_nms2))
  expect_equal(names(res2p),exp_nms2)

  # !!!!! did not re-test repetitive (non-calculated) or expected 0 values
  #       they should not have changed with the addition of lois

  # ----- tests of inverse/harvest slot without loi
  # ..... data types, sizes, and names
  expect_type(res1h,"list")
  expect_equal(class(res1h),"data.frame")
  expect_equal(nrow(res1h),exp_rows)
  expect_equal(ncol(res1h),length(exp_nms1))
  expect_equal(names(res1h),exp_nms1)

  # ..... test repetitive (non-calculated) values equal what was expected
  expect_equal(cm,unique(res1h$cm))
  expect_true(all(res1h$recruitmentTL==recruitmentTLh))
  expect_true(all(res1h$cfUnder==cfunder[["h"]]))
  expect_true(all(res1h$cfIn==cfin[["h"]]))
  expect_true(all(res1h$cfAbove==cfabove[["h"]]))
  expect_true(all(res1h$N0==LH$N0))
  expect_true(all(res1h$Linf==LH$Linf))
  expect_true(all(res1h$K==LH$K))
  expect_true(all(res1h$t0==LH$t0))
  expect_true(all(res1h$LWalpha==LH$LWalpha))
  expect_true(all(res1h$LWbeta==LH$LWbeta))
  expect_true(all(res1h$tmax==LH$tmax))

  # ..... values compute "under" and "above" the slot should all be 0
  expect_true(all(res1h$yieldUnder==0))
  expect_true(all(res1h$nHarvestUnder==0))
  expect_true(all(res1h$avglenUnder==0))
  expect_true(all(res1h$avgwtUnder==0))
  expect_true(all(res1h$FUnder==0))
  expect_true(all(res1h$cmUnder==0))
  expect_true(all(res1h$yieldabove==0))
  expect_true(all(res1h$nHarvestabove==0))
  expect_true(all(res1h$avglenabove==0))
  expect_true(all(res1h$avgwtabove==0))
  expect_true(all(res1h$Fabove==0))
  expect_true(all(res1h$cmabove==0))

  # ----- tests of protected slot with loi
  # ..... data types, sizes, and names
  expect_type(res2h,"list")
  expect_equal(class(res2h),"data.frame")
  expect_equal(nrow(res2h),exp_rows)
  expect_equal(ncol(res2h),length(exp_nms2))
  expect_equal(names(res2h),exp_nms2)

  # !!!!! did not re-test repetitive (non-calculated) or expected 0 values
  #       they should not have changed with the addition of lois
})



## ===== Test Results Accuracy =================================================
## !!!!!   related accuracty results in testing for yprBH_func
#test_that("yprBH_SlotLL() results",{

## !!!!! TO BE ADDED !!!!!
#})
