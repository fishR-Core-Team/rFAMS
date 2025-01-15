

test_that("makeMort() messages",{
  expect_error(makeMort(M=-0.5,Z=1),"'M' must be >=0")
  expect_error(makeMort(cm=0.5),"Must include 'cf' with 'cm'")
  expect_error(makeMort(cm=-0.5,cf=0.5),"'cm' must be >=0")
  expect_error(makeMort(cm=-0.5,cf=1),"'cm' must be >=0")
  expect_error(makeMort(cm=0.5,cf=2),"'cf' must be <=1")
  expect_error(makeMort(cm=0.5,cf=0.2,cfmax=2,cfinc=0.25),"'cfmax' must be <=1")
  expect_error(makeMort(cm=0.5,cf=0.2,cfmax=0.9,cfinc=-0.25),"'cfinc' must be >=0")
  expect_error(makeMort(cm=0.5,cf=0.2,cfmax=0.9,cfinc=1.5),"'cfinc' must be <=1")
  expect_error(makeMort(cf=0.1,M=0.3),"'M', 'Mmax', and 'Minc' not used with 'cf'")
  expect_error(makeMort(cf=0.1,F=0.3),"'F', 'Fmax', and 'Finc' not used with 'cf'")
  expect_error(makeMort(cf=0.1,Z=0.3),"'Z', 'Zmax', and 'Zinc' not used with 'cf'")
  expect_error(makeMort(cm=0.1,M=0.3),"'M', 'Mmax', and 'Minc' not used with 'cm'")
  expect_error(makeMort(cm=0.1,F=0.3),"'F', 'Fmax', and 'Finc' not used with 'cm'")
  expect_error(makeMort(cm=0.1,Z=0.3),"'Z', 'Zmax', and 'Zinc' not used with 'cm'")
  expect_error(makeMort(F=0.1,M=0.2,Z=0.3),"Only two of 'F', 'M', and 'Z' can be given")
})

test_that("makeMort() results",{
})
