## Note must use rFAMS::: because internal functions are not exported

test_that("is.wholenumber() results",{
  expect_true(rFAMS:::is.wholenumber(17))
  expect_true(rFAMS:::is.wholenumber(-17))
  expect_true(rFAMS:::is.wholenumber(0))
  expect_false(rFAMS:::is.wholenumber(1.7))
  expect_false(rFAMS:::is.wholenumber(-1.7))
})

test_that("iIbeta() messages and results",{
  # error messages
  expect_error(rFAMS:::iIbeta(x=-1,a=1,b=1),
               "'x' in incomplete beta function must be >=0")
  expect_error(rFAMS:::iIbeta(x=2,a=1,b=1),
               "'x' in incomplete beta function must be <=1")
  expect_error(rFAMS:::iIbeta(x=0.5,a=-1,b=1),
               "'a' in incomplete beta function must be >=0")
  expect_error(rFAMS:::iIbeta(x=0.5,a=0.5,b=-1),
               "'b' in incomplete beta function must be >=0")

  # comparison to other packages
  df <- expand.grid(x=seq(0.05,0.95,0.1),
                    a=seq(0.1,3.0,0.2),
                    b=seq(0.1,3.0,0.2)) |>
    dplyr::mutate(zipfR=zipfR::Ibeta(x=x,a=a,b=b),
                  spsh=spsh::Ibeta(z=x,a=a,b=b),
                  rFAMS=rFAMS:::iIbeta(x,a,b))
  expect_equal(df$rFAMS,df$zipfR)
  expect_equal(df$rFAMS,df$spsh)
  expect_equal(df$spsh,df$zipfR)
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

test_that("iCheckcfm() messages",{
  ## Fishing mortality
  expect_error(rFAMS:::iCheckcfm(type="fishing"),
               "Need to specify a conditional fishing mortality in")
  ## Set cf to value outside function to test that name is extracted
  cf <- -1
  expect_error(rFAMS:::iCheckcfm(cf,type="fishing"),"'cf' must be >=0")
  cf <- 2
  expect_error(rFAMS:::iCheckcfm(cf,type="fishing"),"'cf' must be <=1")
  cf <- "a"
  expect_error(rFAMS:::iCheckcfm(cf,type="fishing"),"'cf' must be a number")
  cf <- c(0.3,0.5)
  expect_error(rFAMS:::iCheckcfm(cf,type="fishing"),"Only use one value in 'cf'")
  ## test function for work with cfmin and cfmax
  cfmax <- "a"
  expect_error(rFAMS:::iCheckcfm(cfmax,type="fishing"),"'cfmax' must be a number")

  ## Natural mortality
  expect_error(rFAMS:::iCheckcfm(type="natural"),
               "Need to specify a conditional natural mortality in")
  ## Set cf to value outside function to test that name is extracted
  cm <- -1
  expect_error(rFAMS:::iCheckcfm(cm,type="natural"),"'cm' must be >=0")
  cm <- 2
  expect_error(rFAMS:::iCheckcfm(cm,type="natural"),"'cm' must be <=1")
  cm <- "a"
  expect_error(rFAMS:::iCheckcfm(cm,type="natural"),"'cm' must be a number")
  cm <- c(0.3,0.5)
  expect_error(rFAMS:::iCheckcfm(cm,type="natural"),"Only use one value in 'cm'")
  ## test function for work with cmmax
  cmmax <- "a"
  expect_error(rFAMS:::iCheckcfm(cmmax,type="natural"),"'cmmax' must be a number")
})

test_that("iCheckcfminc() messages and values",{
  expect_error(rFAMS:::iCheckcfminc(type="fishing"),
               "Need to specify an increment for conditional fishing mortality in")
  ## Set cfinc to value outside function to test that name is extracted
  cf <- 0.1; cfmax <- 0.9
  cfinc <- -0.1
  expect_error(rFAMS:::iCheckcfminc(cfinc,cf,cfmax,type="fishing"),"must be >=0")
  cfinc <- 2
  expect_error(rFAMS:::iCheckcfminc(cfinc,cf,cfmax,type="fishing"),"must be <=1")
  cfinc <- "a"
  expect_error(rFAMS:::iCheckcfminc(cfinc,cf,cfmax,type="fishing"),"must be a number")
  cfinc <- c(0.3,0.5)
  expect_error(rFAMS:::iCheckcfminc(cfinc,cf,cfmax,type="fishing"),"Only use one value in")

  ## Problems with cf and cfmax
  cf <- 0.9; cfmax <- 0.1; cfinc <- 0.1
  expect_error(rFAMS:::iCheckcfminc(cfinc,cf,cfmax,type="fishing"),
               "'cf' must be equal to or less than 'cfmax'")
  cf <- 0.1; cfmax <- 0.9; cfinc <- 0.001
  expect_warning(tmp <- rFAMS:::iCheckcfminc(cfinc,cf,cfmax,type="fishing"),
                        "Choices of 'cf', 'cfmax', and 'cfinc' resulted in")

  ## Values returned
  expect_equal(class(tmp),"numeric")
  expect_equal(length(tmp),801)
})

test_that("iCheckLHparms() messages and values",{
  ## Test as if using for life history parameters
  tmp <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,maxage=15)
  expect_no_error(iCheckLHparms(tmp))
  tmp <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10)
  expect_error(iCheckLHparms(tmp),
               "'tmp' does not contain a value for: maxage")
  tmp <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,maxage=15,derek=7)
  expect_error(iCheckLHparms(tmp),
               "'tmp' should have only 7")
  tmp <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,MAXAGE=15)
  expect_error(iCheckLHparms(tmp),
               "'tmp' does not contain a value for: maxage")
  tmp <- c(100,2000,0.50,-0.616,-5.453,3.10,15)
  expect_error(iCheckLHparms(tmp),
               "Life history parameters in 'tmp' must be NAMED")

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
               "Only use one value in 'N0'")
})

## Continue with the rest of the internals ##
