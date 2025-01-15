

test_that("makeMort() messages",{
  expect_error(makeMort("MZ",M=-0.5,Z=1),"'M' must be >=0")
  expect_error(makeMort("cfcm",cm=0.5),
    "Need to specify a conditional fishing mortality in 'cf'")
  expect_error(makeMort("cfcm",cm=-0.5,cf=0.5),"'cm' must be >=0")
  expect_error(makeMort("cfcm",cm=-0.5,cf=1),"'cm' must be >=0")
  expect_error(makeMort("cfcm",cm=0.5,cf=2),"'cf' must be <=1")
  expect_error(makeMort("cfcm",cm=0.5,cf=0.2,cfmax=2,cfinc=0.25),"'cfmax' must be <=1")
  expect_error(makeMort("cfcm",cm=0.5,cf=0.2,cfmax=0.9,cfinc=-0.25),"'cfinc' must be >=0")
  expect_error(makeMort("cfcm",cm=0.5,cf=0.2,cfmax=0.9,cfinc=1.5),"'cfinc' must be <=1")
  expect_error(makeMort("cfcm",cf=0.1,M=0.3),
               "'M', 'Mmax', and 'Minc' not used with 'input' of 'cfcm'")
  expect_error(makeMort("MF",cf=0.1,M=0.3),
               "'cf', 'cfmax', and 'cfinc' not used with 'input' of 'FM'")
  expect_error(makeMort("MZ",cf=0.1,M=0.3),
               "'cf', 'cfmax', and 'cfinc' not used with 'input' of 'MZ'")
  expect_error(makeMort("MZ",F=0.1,Z=0.3),
               "'F', 'Fmax', and 'Finc' not used with 'input' of 'MZ'")
  expect_error(makeMort("MZ",F=0.1,M=0.3),
               "'F', 'Fmax', and 'Finc' not used with 'input' of 'MZ'")
})

test_that("makeMort() results",{
})
