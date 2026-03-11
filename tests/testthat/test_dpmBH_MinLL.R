## ===== Test Message Errors and Warnings ======================================
test_that("dpmBH_MinLL() messages",{
  lhparms <- makeLH(N0=100,tmax=30,Linf=1349.5,K=0.111,t0=0.065,
                    LWalpha=-5.2147,LWbeta=3.153)
  simyears <- 50
  minLL <- 400
  rec <- genRecruits(method="fixed",nR=100,simyears=simyears)
  cm <- matrix(rep(c(rep(0,1),rep(0.18,(lhparms$tmax))),simyears),
               nrow=simyears,byrow=TRUE)
  cf <- matrix(rep(c(rep(0,1),rep(0.33,(lhparms$tmax))),simyears),
               nrow=simyears,byrow=TRUE)

  cm2 <- cm
  cf2 <- cf
  rownames(cm2) <- rownames(cf2) <- paste0("year_",1:simyears)
  colnames(cm2) <- colnames(cf2) <- paste0("age_",0:lhparms$tmax)

  # ----- no errors
  dpmBH_MinLL(simyears=simyears,minLL=minLL,cf=cf,cm=cm,rec=rec,
              lhparms=lhparms,species="Striped Bass",group="landlocked") |>
    expect_no_error()
  # ..... confirming no errors even when cm/cf matrices are named
  dpmBH_MinLL(simyears=simyears,minLL=minLL,cf=cf2,cm=cm2,rec=rec,
              lhparms=lhparms,species="Striped Bass",group="landlocked") |>
    expect_no_error()

  # ----- test for missing arguments

  # ----- test for bad values
  # ..... in minLL

  # ..... in cf

  # ..... in cm

  # ..... in rec

  # ..... spot tests for bad values in lhparms ... more thorough testing is
  #       elsewhere; e.g., iCheckLinf(), iCheckN0()
  tmp <- list(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
  LH <- tmp
  LH["N0"] <- -100

  LH <- list(N0=100,tmax=15,Linf=592,K=c(0.3,0.5),t0=-0.3,LWalpha=-5.528,LWbeta=3.273)

  LH <- tmp
  LH["LWbeta"] <- 5

})


## ===== Get Some Results for Use Below ========================================
## Jason's original example
lhparms <- makeLH(N0=100,tmax=30,Linf=1349.5,K=0.111,t0=0.065,
            LWalpha=-5.2147,LWbeta=3.153)
simyears <- 50
minLL <- 400
rec <- genRecruits(method="fixed",nR=100,simyears=simyears)
cm <- matrix(rep(c(rep(0,1),rep(0.18,(lhparms$tmax))),simyears),
             nrow=simyears,byrow=TRUE)
cf <- matrix(rep(c(rep(0,1),rep(0.33,(lhparms$tmax))),simyears),
             nrow=simyears,byrow=TRUE)
minLL1 <- dpmBH_MinLL(simyears=simyears,minLL=minLL,cf=cf,cm=cm,rec=rec,
                      lhparms=lhparms,species="Striped Bass",group="landlocked")

## expectations

## ===== Test Output Types =====================================================
test_that("dpmBH_MinLL() output",{
  expect_type(minLL1,"list")
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
# saveRDS(minLL1,paste0(tmpdir,"dpm_minLL1_",dt,".rds"))

test_that("dmpBH_MinLL() results",{
  # get list of files in datasnaps folder
  tmpdir <- paste0(testthat::test_path(),"/datasnaps/")
  tmpfns <- list.files(tmpdir)
  # Load snapshots of "old" (i.e., last stable) outputs
  minLL1_old <- readRDS(paste0(tmpdir,tmpfns[grepl("dpm_minLL1",tmpfns)]))

  # Compare new to "old" data.frames ... must have run "Get Results" code above
  expect_equal(minLL1,minLL1_old)
})
