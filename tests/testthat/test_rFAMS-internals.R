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
    expect_error("junk must be <=1")
  rFAMS:::iErrGT(c(0.3,1,1.3),1,"junk") |>
    expect_error("All junk must be <=1")
  rFAMS:::iErrGT(0.3,1,"junk") |>
    expect_no_error()
  rFAMS:::iErrGT(1,1,"junk") |>
    expect_no_error()
  rFAMS:::iErrGT(c(0.3,0.1,0.9),1,"junk") |>
    expect_no_error()
  rFAMS:::iErrGT(c(0.3,0.1,1),1,"junk") |>
    expect_no_error()

  rFAMS:::iErrLT(-0.3,0,"junk") |>
    expect_error("junk must be >=0")
  rFAMS:::iErrLT(c(-0.3,1,1.3),0,"junk") |>
    expect_error("All junk must be >=0")
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
  # ----- test that something was sent
  rFAMS:::iCheckN0() |>
    expect_error("Need to specify an initial number of fish in the population")
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
  # ----- test that something was sent
  rFAMS:::iCheckMaxAge() |>
    expect_error("Need to specify a maximum age")
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


test_that("iCheckCondMort() messages",{
  # ----- test that something was sent
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
  cm <- c(-0.3,0.5)
  rFAMS:::iCheckCondMort(cm) |>
    expect_error("All 'cm' must be >=0")
  cm <- c(0.3,1.5)
  rFAMS:::iCheckCondMort(cm) |>
    expect_error("All 'cm' must be <=1")

  # ----- test wrong input types or values
  cfunder <- NULL
  rFAMS:::iCheckCondMort(cfunder) |>
    expect_error("Need to specify a conditional fishing mortality in 'cfunder'")

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
  cfunder <- c(-0.3,0.5)
  rFAMS:::iCheckCondMort(cfunder) |>
    expect_error("All 'cfunder' must be >=0")
  cfunder <- c(0.3,1.5)
  rFAMS:::iCheckCondMort(cfunder) |>
    expect_error("All 'cfunder' must be <=1")
})

test_that("iCheckcf() messages",{
  rFAMS:::iCheckcf() |>
    expect_error("Need to specify a conditional fishing mortality in")
  ## Set cf to value outside function to test that name is extracted
  cf <- -1
  rFAMS:::iCheckcf(cf) |>
    expect_error("'cf' must be >=0")
  cf <- 2
  rFAMS:::iCheckcf(cf) |>
    expect_error("'cf' must be <=1")
  cf <- "a"
  rFAMS:::iCheckcf(cf) |>
    expect_error("'cf' must be a number")
  cf <- c(0.3,0.5)
  rFAMS:::iCheckcf(cf) |>
    expect_error("Only use one value in 'cf'")
  ## test function for work with cfmin and cfmax
  cfmin <- -1
  rFAMS:::iCheckcf(cfmin) |>
    expect_error("'cfmin' must be >=0")
  cfmax <- "a"
  rFAMS:::iCheckcf(cfmax) |>
    expect_error("'cfmax' must be a number")
})

test_that("iCheckcm() messages",{
  rFAMS:::iCheckcm() |>
    expect_error("Need to specify a conditional natural mortality in")
  ## Set cm to value outside function to test that name is extracted
  cm <- -1
  rFAMS:::iCheckcm(cm) |>
    expect_error("'cm' must be >=0")
  cm <- 2
  rFAMS:::iCheckcm(cm) |>
    expect_error("'cm' must be <=1")
  cm <- "a"
  rFAMS:::iCheckcm(cm) |>
    expect_error("'cm' must be a number")
  cm <- c(0.3,0.5)
  rFAMS:::iCheckcm(cm) |>
    expect_error("Only use one value in 'cm'")
  ## test function for work with cmmin and cmmax
  cmmin <- -1
  rFAMS:::iCheckcm(cmmin) |>
    expect_error("'cmmin' must be >=0")
  cmmax <- "a"
  rFAMS:::iCheckcm(cmmax) |>
    expect_error("'cmmax' must be a number")
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

