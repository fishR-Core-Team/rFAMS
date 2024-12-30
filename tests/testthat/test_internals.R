## Note must use rFAMS::: because internal functions are not exported

test_that("is.wholenumber() results",{
  expect_true(rFAMS:::is.wholenumber(17))
  expect_true(rFAMS:::is.wholenumber(-17))
  expect_true(rFAMS:::is.wholenumber(0))
  expect_false(rFAMS:::is.wholenumber(1.7))
  expect_false(rFAMS:::is.wholenumber(-1.7))
})

test_that("iCheckcf() messages",{
  expect_error(rFAMS:::iCheckcf(),"Need to specify a conditional fishing mortality in")
  ## Set cf to value outside function to test that name is extracted
  cf <- -1
  expect_error(rFAMS:::iCheckcf(cf),"must be >=0")
  expect_error(rFAMS:::iCheckcf(cf),"'cf'")
  cf <- 2
  expect_error(rFAMS:::iCheckcf(cf),"must be <=1")
  expect_error(rFAMS:::iCheckcf(cf),"'cf'")
  cf <- "a"
  expect_error(rFAMS:::iCheckcf(cf),"must be a number")
  expect_error(rFAMS:::iCheckcf(cf),"'cf'")
  cf <- c(0.3,0.5)
  expect_error(rFAMS:::iCheckcf(cf),"Only use one value in")
  expect_error(rFAMS:::iCheckcf(cf),"'cf'")
})

test_that("iCheckcm() messages",{
  expect_error(rFAMS:::iCheckcm(),"Need to specify a conditional natural mortality in")
  ## Set cf to value outside function to test that name is extracted
  cm <- -1
  expect_error(rFAMS:::iCheckcm(cm),"must be >=0")
  expect_error(rFAMS:::iCheckcm(cm),"'cm'")
  cm <- 2
  expect_error(rFAMS:::iCheckcm(cm),"must be <=1")
  expect_error(rFAMS:::iCheckcm(cm),"'cm'")
  cm <- "a"
  expect_error(rFAMS:::iCheckcm(cm),"must be a number")
  expect_error(rFAMS:::iCheckcm(cm),"'cm'")
  cm <- c(0.3,0.5)
  expect_error(rFAMS:::iCheckcm(cm),"Only use one value in")
  expect_error(rFAMS:::iCheckcm(cm),"'cm'")
})

## Continue with the rest of the internals ##
