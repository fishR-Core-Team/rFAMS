## ===== Test Message Errors and Warnings ======================================
test_that("yprBH_SlotLL() messages",{
  LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
  cm <- c(0.3,0.4,0.5)

  # ----- no errors or warnings
  # ..... protected slot
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0.25,cfin=0,cfabove=0.15,cm=cm,
               lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
    expect_no_error()
  # ..... inverse/harvest slot
  yprBH_SlotLL(lowerSL=250,upperSL=325,
               cfunder=0,cfin=0.25,cfabove=0,cm=cm,
               lhparms=LH,loi=c(200,300)) |>
    expect_no_error()



    LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)

  # ----- test for missing arguments
  yprBH_MinLL(minLL=300,cf=0.3,cm=0.2,lhparms=LH) |>
    expect_no_error()
  yprBH_MinLL(cf=0.3,cm=0.2,lhparms=LH) |>
    expect_error("Need to specify a minimum length \\(mm\\) limit for harvest")
  yprBH_MinLL(minLL=300,cm=0.2,lhparms=LH) |>
    expect_error("Need to specify a conditional fishing mortality in 'cf'")
  yprBH_MinLL(minLL=300,cf=0.2,lhparms=LH) |>
    expect_error("Need to specify a conditional natural mortality in 'cm'")
  yprBH_MinLL(minLL=300,cf=0.3,cm=0.2) |>
    expect_error("Need to specify a list or vector of life history parameters in")

  # ----- test for bad values
  # ..... in minLL
  yprBH_MinLL(minLL=c(300,400),cf=0.3,cm=0.2,lhparms=LH) |>
    expect_no_error()
  yprBH_MinLL(minLL=-300,cf=0.3,cm=0.2,lhparms=LH) |>
    expect_error("'minLL' must be >=0")
  yprBH_MinLL(minLL=c(-300,400),cf=0.3,cm=0.2,lhparms=LH) |>
    expect_error("All 'minLL' must be >=0")
  yprBH_MinLL(minLL="a",cf=0.3,cm=0.2,lhparms=LH) |>
    expect_error("'minLL' must be a number")
  yprBH_MinLL(minLL=data.frame(minLL=300),cf=0.3,cm=0.2,lhparms=LH) |>
    expect_error("'minLL' must be a vector")
  yprBH_MinLL(minLL=2005,cf=0.3,cm=0.2,lhparms=LH) |>
    expect_error("A minimum length limit of harvest cannot be more than Linf")

  # ..... in cf
  yprBH_MinLL(minLL=300,cf=c(0.3,0.4),cm=0.2,lhparms=LH) |>
    expect_no_error()
  yprBH_MinLL(minLL=300,cf=-0.3,cm=0.2,lhparms=LH)|>
    expect_error("'cf' must be >=0")
  yprBH_MinLL(minLL=300,cf=c(-0.3,0.4),cm=0.2,lhparms=LH)|>
    expect_error("All 'cf' must be >=0")
  yprBH_MinLL(minLL=300,cf=1.3,cm=0.2,lhparms=LH)|>
    expect_error("'cf' must be <=1")
  yprBH_MinLL(minLL=300,cf=c(0.3,1.4),cm=0.2,lhparms=LH)|>
    expect_error("All 'cf' must be <=1")
  yprBH_MinLL(minLL=300,cf="a",cm=0.2,lhparms=LH)|>
    expect_error("'cf' must be a number")
  yprBH_MinLL(minLL=300,cf=data.frame(cf=0.3),cm=0.2,lhparms=LH)|>
    expect_error("'cf' must be a vector")

  # ..... in cm
  yprBH_MinLL(minLL=300,cf=0.2,cm=c(0.3,0.4),lhparms=LH) |>
    expect_no_error()
  yprBH_MinLL(minLL=300,cf=0.2,cm=-0.3,lhparms=LH)|>
    expect_error("'cm' must be >=0")
  yprBH_MinLL(minLL=300,cf=0.2,cm=c(-0.3,0.4),lhparms=LH)|>
    expect_error("All 'cm' must be >=0")
  yprBH_MinLL(minLL=300,cf=0.2,cm=1.3,lhparms=LH)|>
    expect_error("'cm' must be <=1")
  yprBH_MinLL(minLL=300,cf=0.2,cm=c(0.3,1.4),lhparms=LH)|>
    expect_error("All 'cm' must be <=1")
  yprBH_MinLL(minLL=300,cf=0.2,cm="a",lhparms=LH)|>
    expect_error("'cm' must be a number")
  yprBH_MinLL(minLL=300,cf=0.2,cm=data.frame(cm=0.3),lhparms=LH)|>
    expect_error("'cm' must be a vector")

  # ..... in loi
  lois <- c(300,400,500)
  yprBH_MinLL(minLL=300,cf=0.2,cm=c(0.3,0.4),lhparms=LH,loi=lois) |>
    expect_no_error()
  lois <- -300
  yprBH_MinLL(minLL=300,cf=0.2,cm=c(0.3,0.4),lhparms=LH,loi=lois) |>
    expect_error("'loi' must be >=0")
  lois <- c(-300,400,500)
  yprBH_MinLL(minLL=300,cf=0.2,cm=c(0.3,0.4),lhparms=LH,loi=lois) |>
    expect_error("All 'loi' must be >=0")
  lois <- "a"
  yprBH_MinLL(minLL=300,cf=0.2,cm=c(0.3,0.4),lhparms=LH,loi=lois) |>
    expect_error("'loi' must be a number")
  lois <- data.frame(loi=c(300,400,500))
  yprBH_MinLL(minLL=300,cf=0.2,cm=c(0.3,0.4),lhparms=LH,loi=lois) |>
    expect_error("'loi' must be a vector")

  # ..... spot tests for bad values in lhparms ... more thorough testing is
  #       elsewhere; e.g., iCheckLinf(), iCheckN0()
  tmp <- list(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
  LH <- tmp
  LH["N0"] <- -100
  yprBH_MinLL(minLL=300,cf=0.3,cm=0.2,lhparms=LH) |>
    expect_error("'N0' must be >=0")
  LH <- list(N0=100,tmax=15,Linf=592,K=c(0.3,0.5),t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
  yprBH_MinLL(minLL=300,cf=0.3,cm=0.2,lhparms=LH) |>
    expect_error("Only use one value in 'K'")
  LH <- tmp
  LH["LWbeta"] <- 5
  yprBH_MinLL(minLL=300,cf=0.3,cm=0.2,lhparms=LH) |>
    expect_warning("A weight-length beta coefficient of 5 seems too large") |>
    expect_warning("A weight-length beta coefficient of 5 seems too large")
  ## !!! May want to address this double warning
})


## ===== Get Some Results for Use Below ========================================
LH <- makeLH(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10)
minLL <- c(200,300)
cf <- seq(0.3,0.4,0.05)
cm <- c(0.2,0.3)
lois <- c(300,400)

res1 <- yprBH_MinLL(minLL=minLL,cf=cf,cm=cm,lhparms=LH,matchRicker=FALSE)
res2 <- yprBH_MinLL(minLL=minLL,cf=cf,cm=cm,lhparms=LH,loi=lois,matchRicker=FALSE)

exp_nms2 <- c("yield","nharvest","ndie","nt","tr","avgwt","avglen","nAt300","nAt400",
              "exploitation","F","M","Z","S","cf","cm","minLL",
              "N0","Linf","K","t0","LWalpha","LWbeta","tmax","notes")
exp_nms1 <- exp_nms2[!startsWith(exp_nms2,"nAt")]
exp_rows <- length(minLL)*length(cf)*length(cm)


## ===== Test Output Types =====================================================
test_that("yprBH_SlotLL() output",{
  # ----- tests without loi
  # ..... data types, sizes, and names
  expect_type(res1,"list")
  expect_equal(class(res1),"data.frame")
  expect_equal(nrow(res1),exp_rows)
  expect_equal(ncol(res1),length(exp_nms1))
  expect_equal(names(res1),exp_nms1)

  # ..... test repetitive (non-calculated) values equal what was expected
  expect_equal(minLL,unique(res1$minLL))
  expect_equal(cf,unique(res1$cf))
  expect_equal(cm,unique(res1$cm))
  expect_true(all(res1$N0==LH$N0))
  expect_true(all(res1$Linf==LH$Linf))
  expect_true(all(res1$K==LH$K))
  expect_true(all(res1$t0==LH$t0))
  expect_true(all(res1$LWalpha==LH$LWalpha))
  expect_true(all(res1$LWbeta==LH$LWbeta))

  # ----- tests with loi
  # ..... data types, sizes, and names
  expect_type(res2,"list")
  expect_equal(class(res2),"data.frame")
  expect_equal(nrow(res2),exp_rows)
  expect_equal(ncol(res2),length(exp_nms2))
  expect_equal(names(res2),exp_nms2)

  # ..... test repetitive (non-calculated) values equal what was expected
  expect_equal(minLL,unique(res2$minLL))
  expect_equal(cf,unique(res2$cf))
  expect_equal(cm,unique(res2$cm))
  expect_true(all(res2$N0==LH$N0))
  expect_true(all(res2$Linf==LH$Linf))
  expect_true(all(res2$K==LH$K))
  expect_true(all(res2$t0==LH$t0))
  expect_true(all(res2$LWalpha==LH$LWalpha))
  expect_true(all(res2$LWbeta==LH$LWbeta))
})



## ===== Test Results Accuracy =================================================
## !!!!!   related accuracty results in testing for yprBH_func
#test_that("yprBH_SlotLL() results",{

## !!!!! TO BE ADDED !!!!!
#})
