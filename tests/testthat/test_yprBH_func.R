## ===== Test Message Errors and Warnings ======================================
test_that("yprBH_func() messages",{
  LH <- makeLH(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10)
  yprBH_func(minLL=355,cf=0.45,cm=0.25,lhparms=LH) |>
    expect_no_error()

  # ----- test for missing arguments
  yprBH_func(cf=0.45,cm=0.25,lhparms=LH) |>
    expect_error("Need to specify a minimum length \\(mm\\) limit for harvest")
  yprBH_func(minLL=355,cm=0.25,lhparms=LH) |>
    expect_error("Need to specify a conditional fishing mortality in 'cf'")
  yprBH_func(minLL=355,cf=0.45,lhparms=LH) |>
    expect_error("Need to specify a conditional natural mortality in 'cm'")
  yprBH_func(minLL=355,cf=0.45,cm=0.25) |>
    expect_error("Need to specify a list or vector of life history parameters")

  # ----- test for bad values, more thorough testing is elsewhere;
  #       e.g., iCheckLinf(), iCheckN0()
  yprBH_func(minLL=-10,cf=0.45,cm=0.25,lhparms=LH) |>
    expect_error("'minLL' must be >=0")
  yprBH_func(minLL=35,cf=0.45,cm=0.25,lhparms=LH) |>
    expect_warning("A minimum length limit of harvest of 35 mm seems too small")
  yprBH_func(minLL=2235,cf=0.45,cm=0.25,lhparms=LH) |>
    expect_warning("A minimum length limit of harvest of 2235 mm seems too large") |>
    expect_warning("The set mininmum length limit of harvest")
  yprBH_func(minLL=355,cf=-0.45,cm=0.25,lhparms=LH) |>
    expect_error("'cf' must be >=0")
  yprBH_func(minLL=355,cf=1.45,cm=0.25,lhparms=LH) |>
    expect_error("'cf' must be <=1")
  yprBH_func(minLL=355,cf=0.45,cm=-0.25,lhparms=LH) |>
    expect_error("'cm' must be >=0")
  yprBH_func(minLL=355,cf=0.45,cm=1.25,lhparms=LH) |>
    expect_error("'cm' must be <=1")
  yprBH_func(minLL=355,cf=0.45,cm=0.25,lhparms=LH,loi=-100) |>
    expect_error("'loi' must be >=0")
  yprBH_func(minLL=355,cf=0.45,cm=0.25,lhparms=LH,loi=c(100,-200)) |>
    expect_error("All 'loi' must be >=0")

  # ----- spot tests for bad values in lhparms, more thorough testing is
  #       elsewhere; e.g., iCheckLinf(), iCheckN0()
  tmp <- list(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10)
  LH <- tmp
  LH["N0"] <- -100
  yprBH_func(minLL=355,cf=0.45,cm=0.25,lhparms=LH) |>
    expect_error("'N0' must be >=0")
  LH <- tmp
  LH["Linf"] <- "a"
  yprBH_func(minLL=355,cf=0.45,cm=0.25,lhparms=LH) |>
    expect_error("'Linf' must be a number")
  LH <- list(N0=100,tmax=15,Linf=2000,K=c(0.3,0.5),t0=-0.616,LWalpha=-5.453,LWbeta=3.10)
  yprBH_func(minLL=355,cf=0.45,cm=0.25,lhparms=LH) |>
    expect_error("Only use one value in 'K'")
  LH <- tmp
  LH["LWbeta"] <- 5
  yprBH_func(minLL=355,cf=0.45,cm=0.25,lhparms=LH) |>
    expect_warning("A weight-length beta coefficient of 5 seems too large")
})


## ===== Get Some Results for Use Below ========================================
## ----- lhparms as a list
LH <- makeLH(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10)
res1 <- yprBH_func(cf=0.45,cm=0.25,minLL=355,lhparms=LH,matchRicker=FALSE)

## ----- Same, but with lhparms as a vector
LH <- makeLH(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,
             restype="vector")
res2 <- yprBH_func(cf=0.45,cm=0.25,minLL=355,lhparms=LH,matchRicker=FALSE)


## ===== Test Output Types =====================================================
test_that("Two types of lhparams of yprBH_func() match",{
  expect_equal(res2,res1)
})

test_that("yprBH_func() output",{
  expect_type(res1,"list")
  expect_equal(class(res1),"data.frame")
  expect_equal(nrow(res1),1)
  expect_equal(ncol(res1),23)
  expect_equal(names(res1),c("yield","nharvest","ndie","nt","tr",
                             "avgwt","avglen","exploitation","F","M","Z","S",
                             "cf","cm","minLL","N0","Linf","K","t0",
                             "LWalpha","LWbeta","tmax","notes"))
  expect_equal(res1$notes,"Nt>N0")
})

## ===== Test Results Accuracy with Other Sources ==============================
test_that("yprBH_func() results",{
  ## Results from Jason's original yprBH_func(), assumed tested against FAMS
  ores <- data.frame(yield=663135.3,exploitation=0.3966366,nharvest=67.51261,
                     ndie=32.48739,nt=100,avgwt=9822.392,avglen=1113.895,
                     F=0.597837,M=0.2876821,Z=0.8855191,S=0.4125,
                     cf=0.45,cm=0.25,minLL=355,N0=100,Linf=2000,K=0.50,t0=-0.616,
                     LWalpha=-5.453,LWbeta=3.10,tmax=15)
  expect_equal(round(res1$exploitation,7),ores$exploitation)
  expect_equal(round(res1$yield,1),ores$yield)
  expect_equal(round(res1$nharvest,5),ores$nharvest)
  expect_equal(round(res1$ndie,5),ores$ndie)
  expect_equal(round(res1$nt,0),ores$nt)
  expect_equal(round(res1$avgwt,3),ores$avgwt)
  expect_equal(round(res1$avglen,3),ores$avglen)
  expect_equal(round(res1$F,6),ores$F)
  expect_equal(round(res1$M,7),ores$M)
  expect_equal(round(res1$Z,7),ores$Z)
  expect_equal(round(res1$S,4),ores$S)
  expect_equal(dplyr::select(res1,cf:tmax),dplyr::select(ores,cf:tmax))
})
