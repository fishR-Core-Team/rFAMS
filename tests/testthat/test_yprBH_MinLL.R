## ===== Test Message Errors and Warnings ======================================
test_that("yprBH_MinLL() messages",{
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
    expect_warning("A weight-length beta coefficient of 5 seems too large")
})


## ===== Get Some Results for Use Below ========================================
LH <- makeLH(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10)
lois <- c(300,400)

## ----- only one simulation ... lhparms as a list with lois
minll1 <- 355
cf1 <- 0.45
cm1 <- 0.25
minLL1 <- yprBH_MinLL(minLL=minll1,cf=cf1,cm=cm1,lhparms=LH,loi=lois)

## ----- multiple simulations ... lhparms as a list with lois
minll2 <- c(200,300)
cf2 <- seq(0.3,0.4,0.05)
cm2 <- c(0.2,0.3)
minLL2 <- yprBH_MinLL(minLL=minll2,cf=cf2,cm=cm2,lhparms=LH,loi=lois)

## expectations
exp_nms1 <- c("yield","nharvest","ndie","nt","tr","avgwt","avglen","nAt300",
              "nAt400","exploitation","F","M","Z","S","cf","cm","minLL","N0",
              "Linf","K","t0","LWalpha","LWbeta","tmax","notes")
exp_rows1 <- 1
exp_rows2 <- length(minll2)*length(cf2)*length(cm2)

## ===== Test Output Types =====================================================
test_that("yprBH_MinLL() output",{
  # ----- tests with single simulation
  # ..... data types, sizes, and names
  expect_type(minLL1,"list")
  expect_equal(class(minLL1),"data.frame")
  expect_equal(nrow(minLL1),exp_rows1)
  expect_equal(ncol(minLL1),length(exp_nms1))
  expect_equal(names(minLL1),exp_nms1)

  # ..... test repetitive (non-calculated) values equal what was expected
  expect_equal(unique(minLL1$minLL),minll1)
  expect_equal(unique(minLL1$cf),cf1)
  expect_equal(unique(minLL1$cm),cm1)
  expect_true(all(minLL1$N0==LH$N0))
  expect_true(all(minLL1$Linf==LH$Linf))
  expect_true(all(minLL1$K==LH$K))
  expect_true(all(minLL1$t0==LH$t0))
  expect_true(all(minLL1$LWalpha==LH$LWalpha))
  expect_true(all(minLL1$LWbeta==LH$LWbeta))

  # ----- tests with multiple simulations
  expect_type(minLL2,"list")
  expect_equal(class(minLL2),"data.frame")
  expect_equal(nrow(minLL2),exp_rows2)
  expect_equal(ncol(minLL2),length(exp_nms1))
  expect_equal(names(minLL2),exp_nms1)

  # ..... test repetitive (non-calculated) values equal what was expected
  expect_equal(unique(minLL2$minLL),minll2)
  expect_equal(unique(minLL2$cf),cf2)
  expect_equal(unique(minLL2$cm),cm2)
  expect_true(all(minLL2$N0==LH$N0))
  expect_true(all(minLL2$Linf==LH$Linf))
  expect_true(all(minLL2$K==LH$K))
  expect_true(all(minLL2$t0==LH$t0))
  expect_true(all(minLL2$LWalpha==LH$LWalpha))
  expect_true(all(minLL2$LWbeta==LH$LWbeta))
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
# saveRDS(minLL1,paste0(tmpdir,"ypr_minLL1_",dt,".rds"))
# saveRDS(minLL2,paste0(tmpdir,"ypr_minLL2_",dt,".rds"))

test_that("yprBH_MinLL() results",{
  # get list of files in datasnaps folder
  tmpdir <- paste0(testthat::test_path(),"/datasnaps/")
  tmpfns <- list.files(tmpdir)
  # Load snapshots of "old" (i.e., last stable) outputs
  minLL1_old <- readRDS(paste0(tmpdir,tmpfns[grepl("ypr_minLL1",tmpfns)]))
  minLL2_old <- readRDS(paste0(tmpdir,tmpfns[grepl("ypr_minLL2",tmpfns)]))

  # Compare new to "old" data.frames ... must have run "Get Results" code above
  expect_equal(minLL1,minLL1_old)
  expect_equal(minLL2,minLL2_old)
})
