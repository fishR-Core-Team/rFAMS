test_that("genRecruits() messages",{
  ## ===== No errors
  genRecruits(simyears=10,method="fixed",nR=50) |>
    expect_no_error()
  genRecruits(simyears=10,method="uniform",minR=25,maxR=75) |>
    expect_no_error()
  genRecruits(simyears=10,method="normal",minR=25,maxR=75,meanR=50,sdR=10) |>
    expect_no_error()
  genRecruits(simyears=10,method="StrYC_Nth",nR=50,sizeStr=2,nStr=5) |>
    expect_no_error()
  genRecruits(simyears=10,method="StrYC_randInt",nR=50,sizeStr=2,avgFreq=5) |>
    expect_no_error()

  ## ===== Bad simyears or method
  genRecruits(simyears=-10,method="fixed",nR=50) |>
    expect_error("'simyears' must be >=1")
  genRecruits(simyears=10.5,method="fixed",nR=50) |>
    expect_error("'simyears' must be a whole number of years")
  genRecruits(simyears=10,method="Jason",nR=50) |>
    expect_error("'arg' should be one of")

  ## ===== Check arguments provided (or not) depends on method
  genRecruits(simyears=10,method="fixed") |>
    expect_error("Must provide 'nR' when 'method=\"fixed\"'")
  genRecruits(simyears=10,method="fixed",nR=50,minR=10) |>
    expect_warning("Only use 'nr' when 'method' is 'fixed'")
  genRecruits(simyears=10,method="fixed",nR=50,minR=10,sdR=3) |>
    expect_warning("Only use 'nr' when 'method' is 'fixed'")

  genRecruits(simyears=10,method="uniform") |>
    expect_error("Must provide 'minR' when 'method=\"uniform\"'")
  genRecruits(simyears=10,method="uniform",minR=10) |>
    expect_error("Must provide 'maxR' when 'method=\"uniform\"'")
  genRecruits(simyears=10,method="uniform",minR=10,maxR=20,sdR=3) |>
    expect_warning("Only use 'minR' and 'maxR' when 'method' is 'uniform'")
  genRecruits(simyears=10,method="uniform",nR=50,minR=10,maxR=20,sdR=3) |>
    expect_warning("Only use 'minR' and 'maxR' when 'method' is 'uniform'")

  genRecruits(simyears=10,method="normal") |>
    expect_error("Must provide 'minR' when 'method=\"normal\"'")
  genRecruits(simyears=10,method="normal",minR=10) |>
    expect_error("Must provide 'maxR' when 'method=\"normal\"'")
  genRecruits(simyears=10,method="normal",minR=10,maxR=20) |>
    expect_error("Must provide 'meanR' when 'method=\"normal\"'")
  genRecruits(simyears=10,method="normal",minR=10,maxR=20,meanR=15) |>
    expect_error("Must provide 'sdR' when 'method=\"normal\"'")
  genRecruits(simyears=10,method="normal",nR=50,minR=10,maxR=20,meanR=15,sdR=3) |>
    expect_warning("Only use 'minnr', 'maxR', 'meanR', and 'sdR' when")
  genRecruits(simyears=10,method="normal",nR=50,minR=10,maxR=20,meanR=15,sdR=3,avgFreq=3) |>
    expect_warning("Only use 'minnr', 'maxR', 'meanR', and 'sdR' when")
  genRecruits(simyears=10,method="normal",minR=10,maxR=20,meanR=5,sdR=3) |>
    expect_error("'meanR' cannot be less than 'minR'")
  genRecruits(simyears=10,method="normal",minR=10,maxR=20,meanR=25,sdR=3) |>
    expect_error("'meanR' cannot be greater than 'maxR'")

  genRecruits(simyears=10,method="StrYC_Nth") |>
    expect_error("Must provide 'nR' when 'method=\"StrYC_Nth\"'")
  genRecruits(simyears=10,method="StrYC_Nth",nR=50) |>
    expect_error("Must provide 'nStr' when 'method=\"StrYC_Nth\"'")
  genRecruits(simyears=10,method="StrYC_Nth",nR=50,nStr=5) |>
    expect_error("Must provide 'sizeStr' when 'method=\"StrYC_Nth\"'")
  genRecruits(simyears=10,method="StrYC_Nth",nR=50,meanR=50,nStr=5,sizeStr=2) |>
    expect_warning("Only use 'nR', 'nStr', and 'sizeStr' when")
  genRecruits(simyears=10,method="StrYC_Nth",nR=50,meanR=50,nStr=5,sizeStr=2,avgFreq=3) |>
    expect_warning("Only use 'nR', 'nStr', and 'sizeStr' when")

  genRecruits(simyears=10,method="StrYC_randInt") |>
    expect_error("Must provide 'nR' when 'method=\"StrYC_randInt\"'")
  genRecruits(simyears=10,method="StrYC_randInt",nR=50) |>
    expect_error("Must provide 'sizeStr' when 'method=\"StrYC_randInt\"'")
  genRecruits(simyears=10,method="StrYC_randInt",nR=50,sizeStr=2) |>
    expect_error("Must provide 'avgFreq' when 'method=\"StrYC_randInt\"'")
  genRecruits(simyears=10,method="StrYC_randInt",nR=50,meanR=50,sizeStr=2,avgFreq=5) |>
    expect_warning("Only use 'nR', 'avgFreq', and 'sizeStr' when")
  genRecruits(simyears=10,method="StrYC_randInt",nR=50,meanR=50,sizeStr=2,
              avgFreq=5,maxR=20) |>
    expect_warning("Only use 'nR', 'avgFreq', and 'sizeStr' when")

  # ===== Spot check values with each of the arguments
  genRecruits(simyears=10,method="fixed",nR=-50) |>
    expect_error("'nR' must be >=0")
  genRecruits(simyears=10,method="fixed",nR="a") |>
    expect_error("'nR' must be a number")
  genRecruits(simyears=10,method="fixed",nR=c(40,50)) |>
    expect_error("Only use one value in 'nR'")
  genRecruits(simyears=10,method="fixed",nR=50.5) |>
    expect_warning("The value in 'nR' is not a whole number")

  genRecruits(simyears=10,method="normal",minR=-10,maxR=20,meanR=15,sdR=3) |>
    expect_error("'minR' must be >=0")
  genRecruits(simyears=10,method="normal",minR="a",maxR=20,meanR=15,sdR=3) |>
    expect_error("'minR' must be a number")
  genRecruits(simyears=10,method="normal",minR=c(10,20),maxR=20,meanR=15,sdR=3) |>
    expect_error("Only use one value in 'minR'")
  genRecruits(simyears=10,method="normal",minR=10.5,maxR=20,meanR=15,sdR=3) |>
    expect_warning("The value in 'minR' is not a whole number")

  genRecruits(simyears=10,method="normal",minR=10,maxR=-20,meanR=15,sdR=3) |>
    expect_error("'maxR' must be >=0")
  genRecruits(simyears=10,method="normal",minR=10,maxR="a",meanR=15,sdR=3) |>
    expect_error("'maxR' must be a number")
  genRecruits(simyears=10,method="normal",minR=10,maxR=c(10,20),meanR=15,sdR=3) |>
    expect_error("Only use one value in 'maxR'")
  genRecruits(simyears=10,method="normal",minR=10,maxR=20.5,meanR=15,sdR=3) |>
    expect_warning("The value in 'maxR' is not a whole number")

  genRecruits(simyears=10,method="normal",minR=10,maxR=20,meanR=-15,sdR=3) |>
    expect_error("'meanR' must be >=0")
  genRecruits(simyears=10,method="normal",minR=10,maxR=20,meanR="a",sdR=3) |>
    expect_error("'meanR' must be a number")
  genRecruits(simyears=10,method="normal",minR=10,maxR=20,meanR=c(15,17),sdR=3) |>
    expect_error("Only use one value in 'meanR'")
  genRecruits(simyears=10,method="normal",minR=10,maxR=20,meanR=15.5,sdR=3) |>
    expect_no_error()

  genRecruits(simyears=10,method="normal",minR=10,maxR=20,meanR=15,sdR=-3) |>
    expect_error("'sdR' must be >=0")
  genRecruits(simyears=10,method="normal",minR=10,maxR=20,meanR=15,sdR="a") |>
    expect_error("'sdR' must be a number")
  genRecruits(simyears=10,method="normal",minR=10,maxR=20,meanR=15,sdR=c(3,5)) |>
    expect_error("Only use one value in 'sdR'")
  genRecruits(simyears=10,method="normal",minR=10,maxR=20,meanR=15,sdR=3.5) |>
    expect_no_error()

  genRecruits(simyears=10,method="StrYC_Nth",nR=50,nStr=-5,sizeStr=2) |>
    expect_error("'nStr' must be >=0")
  genRecruits(simyears=10,method="StrYC_Nth",nR=50,nStr="a",sizeStr=2) |>
    expect_error("'nStr' must be a number")
  genRecruits(simyears=10,method="StrYC_Nth",nR=50,nStr=5:6,sizeStr=2) |>
    expect_error("Only use one value in 'nStr'")
  genRecruits(simyears=10,method="StrYC_Nth",nR=50,nStr=5.5,sizeStr=2) |>
    expect_error("The value in 'nStr' is not a whole number")
  genRecruits(simyears=10,method="StrYC_Nth",nR=50,nStr=15,sizeStr=2) |>
    expect_error("'nStr' \\(=15\\) is greater than 'simyears' \\(=10\\)")

  genRecruits(simyears=10,method="StrYC_Nth",nR=50,nStr=5,sizeStr=-2) |>
    expect_error("'sizeStr' must be >=0")
  genRecruits(simyears=10,method="StrYC_Nth",nR=50,nStr=5,sizeStr="a") |>
    expect_error("'sizeStr' must be a number")
  genRecruits(simyears=10,method="StrYC_Nth",nR=50,nStr=5,sizeStr=2:3) |>
    expect_error("Only use one value in 'sizeStr'")
  genRecruits(simyears=10,method="StrYC_Nth",nR=50,nStr=5,sizeStr=2.5) |>
    expect_no_error()

  genRecruits(simyears=10,method="StrYC_randInt",nR=50,sizeStr=2,avgFreq=-5) |>
    expect_error("'avgFreq' must be >=0")
  genRecruits(simyears=10,method="StrYC_randInt",nR=50,sizeStr=2,avgFreq="a") |>
    expect_error("'avgFreq' must be a number")
  genRecruits(simyears=10,method="StrYC_randInt",nR=50,sizeStr=2,avgFreq=4:5) |>
    expect_error("Only use one value in 'avgFreq'")
  genRecruits(simyears=10,method="StrYC_randInt",nR=50,sizeStr=2,avgFreq=5.5) |>
    expect_no_error()
  genRecruits(simyears=10,method="StrYC_randInt",nR=50,sizeStr=2,avgFreq=15) |>
    expect_warning("'avgFreq' \\(=15\\) is greater than 'simyears' \\(=10\\)")
})

test_that("genRecruits() output",{
  set.seed(534234)   # for consistency as some tests use randomization
  sy <- 10
  nR <- 50
  res1 <- genRecruits(simyears=sy,method="fixed",nR=nR)
  expect_equal(class(res1),c("GENREC","numeric"))
  expect_true(is.numeric(res1))
  expect_equal(length(res1),sy)
  expect_true(all(res1==nR))

  minR <- 25; maxR <- 75
  res1 <- genRecruits(simyears=sy,method="uniform",minR=minR,maxR=maxR)
  expect_equal(class(res1),c("GENREC","numeric"))
  expect_true(is.numeric(res1))
  expect_equal(length(res1),sy)
  expect_true(min(res1)>=minR)
  expect_true(max(res1)<=maxR)

  res1 <- genRecruits(simyears=sy,method="normal",minR=minR,maxR=maxR,meanR=50,sdR=10)
  expect_equal(class(res1),c("GENREC","numeric"))
  expect_true(is.numeric(res1))
  expect_equal(length(res1),sy)
  expect_true(min(res1)>=minR)
  expect_true(max(res1)<=maxR)

  sizeStr <- 2
  nStr <- 5
  res1 <- genRecruits(simyears=sy,method="StrYC_Nth",nR=nR,sizeStr=sizeStr,nStr=nStr)
  expect_equal(class(res1),c("GENREC","numeric"))
  expect_true(is.numeric(res1))
  expect_equal(length(res1),sy)
  expect_equal(unique(res1),c(nR,nR*sizeStr))
  expect_equal(sum(res1==nR),sy-sy/nStr)
  expect_equal(sum(res1==nR*sizeStr),sy/nStr)

  res1 <- genRecruits(simyears=sy,method="StrYC_randInt",
                      nR=nR,sizeStr=sizeStr,avgFreq=2)
  expect_equal(class(res1),c("GENREC","numeric"))
  expect_true(is.numeric(res1))
  expect_equal(length(res1),sy)
  expect_equal(unique(res1),c(nR,nR*sizeStr))
})
