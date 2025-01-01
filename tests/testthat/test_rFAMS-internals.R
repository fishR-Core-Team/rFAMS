## Note must use rFAMS::: because internal functions are not exported

test_that("is.wholenumber() results",{
  expect_true(rFAMS:::is.wholenumber(17))
  expect_true(rFAMS:::is.wholenumber(-17))
  expect_true(rFAMS:::is.wholenumber(0))
  expect_false(rFAMS:::is.wholenumber(1.7))
  expect_false(rFAMS:::is.wholenumber(-1.7))
})

test_that("iCheckMLHinc() messages and values",{
  expect_error(rFAMS:::iCheckMLHinc(),
               "Need to specify an increment for minimum length")
  ## Set MLHinc to value outside function to test that name is extracted
  MLHmin <- 100; MLHmax <- 900
  MLHinc <- -100
  expect_error(rFAMS:::iCheckMLHinc(MLHinc,MLHmin,MLHmax),"must be >=0")
  MLHinc <- "a"
  expect_error(rFAMS:::iCheckMLHinc(MLHinc,MLHmin,MLHmax),"must be a number")
  MLHinc <- c(300,500)
  expect_error(rFAMS:::iCheckMLHinc(MLHinc,MLHmin,MLHmax),"Only use one value in")

  ## Problems with MLHmin and MLHmax
  MLHmin <- 900; MLHmax <- 100; MLHinc <- 100
  expect_error(rFAMS:::iCheckMLHinc(MLHinc,MLHmin,MLHmax),
               "'MLHmin' must be equal to or less than 'MLHmax'")
  MLHmin <- 100; MLHmax <- 900; MLHinc <- 1
  expect_warning(tmp <- rFAMS:::iCheckMLHinc(MLHinc,MLHmin,MLHmax),
                 "Choices of 'MLHmin', 'MLHmax', and 'MLHinc' resulted in")

  ## Values returned
  expect_equal(class(tmp),"numeric")
  expect_equal(length(tmp),801)
})

test_that("iCheckcf() messages",{
  expect_error(rFAMS:::iCheckcf(),"Need to specify a conditional fishing mortality in")
  ## Set cf to value outside function to test that name is extracted
  cf <- -1
  expect_error(rFAMS:::iCheckcf(cf),"'cf' must be >=0")
  cf <- 2
  expect_error(rFAMS:::iCheckcf(cf),"'cf' must be <=1")
  cf <- "a"
  expect_error(rFAMS:::iCheckcf(cf),"'cf' must be a number")
  cf <- c(0.3,0.5)
  expect_error(rFAMS:::iCheckcf(cf),"Only use one value in 'cf'")
  ## test function for work with cfmin and cfmax
  cfmin <- -1
  expect_error(rFAMS:::iCheckcf(cfmin),"'cfmin' must be >=0")
  cfmax <- "a"
  expect_error(rFAMS:::iCheckcf(cfmax),"'cfmax' must be a number")
})

test_that("iCheckcm() messages",{
  expect_error(rFAMS:::iCheckcm(),"Need to specify a conditional natural mortality in")
  ## Set cm to value outside function to test that name is extracted
  cm <- -1
  expect_error(rFAMS:::iCheckcf(cm),"'cm' must be >=0")
  cm <- 2
  expect_error(rFAMS:::iCheckcf(cm),"'cm' must be <=1")
  cm <- "a"
  expect_error(rFAMS:::iCheckcf(cm),"'cm' must be a number")
  cm <- c(0.3,0.5)
  expect_error(rFAMS:::iCheckcf(cm),"Only use one value in 'cm'")
  ## test function for work with cmmin and cmmax
  cmmin <- -1
  expect_error(rFAMS:::iCheckcf(cmmin),"'cmmin' must be >=0")
  cmmax <- "a"
  expect_error(rFAMS:::iCheckcf(cmmax),"'cmmax' must be a number")
})

test_that("iCheckcfminc() messages and values",{
  expect_error(rFAMS:::iCheckcfminc(),
               "Need to specify an increment for conditional natural mortality in")
  ## Set cfinc to value outside function to test that name is extracted
  cfmin <- 0.1; cfmax <- 0.9
  cfinc <- -0.1
  expect_error(rFAMS:::iCheckcfminc(cfinc,cfmin,cfmax),"must be >=0")
  cfinc <- 2
  expect_error(rFAMS:::iCheckcfminc(cfinc,cfmin,cfmax),"must be <=1")
  cfinc <- "a"
  expect_error(rFAMS:::iCheckcfminc(cfinc,cfmin,cfmax),"must be a number")
  cfinc <- c(0.3,0.5)
  expect_error(rFAMS:::iCheckcfminc(cfinc,cfmin,cfmax),"Only use one value in")

  ## Problems with cfmin and cfmax
  cfmin <- 0.9; cfmax <- 0.1; cfinc <- 0.1
  expect_error(rFAMS:::iCheckcfminc(cfinc,cfmin,cfmax),
               "'cfmin' must be equal to or less than 'cfmax'")
  cfmin <- 0.1; cfmax <- 0.9; cfinc <- 0.001
  expect_warning(tmp <- rFAMS:::iCheckcfminc(cfinc,cfmin,cfmax),
                        "Choices of 'cfmin', 'cfmax', and 'cfinc' resulted in")

  ## Values returned
  expect_equal(class(tmp),"numeric")
  expect_equal(length(tmp),801)
})

test_that("iCheckN0() messages and values",{
  ## Test with just one value
  expect_error(rFAMS:::iCheckN0(),
               "Need to specify an initial number of fish in the population in")
  ## Set MLHinc to value outside function to test that name is extracted
  N0 <- -100
  expect_error(rFAMS:::iCheckN0(N0),"must be >=0")
  N0 <- "a"
  expect_error(rFAMS:::iCheckN0(N0),"must be a number")
  N0 <- c(300,500)
  expect_error(rFAMS:::iCheckN0(N0),
               "'N0' must contain only one value for 'N0' or 7 named")

  ## Test as if using for life history parameters
  N0 <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,maxage=15)
  expect_no_error(iCheckN0(N0))
  N0 <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10)
  expect_error(iCheckN0(N0),
               "'N0' must contain only one value for 'N0' or 7 named")
  N0 <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,maxage=15,derek=7)
  expect_error(iCheckN0(N0),
               "'N0' must contain only one value for 'N0' or 7 named")
  N0 <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,MAXAGE=15)
  expect_error(iCheckN0(N0),
               "'N0' must have named values for all of")
  N0 <- c(100,2000,0.50,-0.616,-5.453,3.10,15)
  expect_error(iCheckN0(N0),
               "'N0' must have named values for")
})

## Continue with the rest of the internals ##
