# ====== Get some simple results
# ------ LVB results (as an example)
data(SpotVA1,package="FSA")
SpotVA1 <- SpotVA1 |> dplyr::mutate(tl=tl*25.4)
vb1 <- FSA::makeGrowthFun()
fit1 <- nls(tl~vb1(age,Linf,K,t0),data=SpotVA1,
            start=FSA::findGrowthStarts(tl~age,data=SpotVA1))
vb2 <- FSA::makeGrowthFun(pname="Original")
fit2 <- nls(tl~vb2(age,Linf,K,L0),data=SpotVA1,start=list(Linf=337,K=0.4,L0=10))

# ----- LW results (as an example)
data(BluegillLM,package="FSAdata")
BluegillLM <- BluegillLM |>
  dplyr::mutate(logW=log10(wght),logL=log10(tl))
fit3 <- lm(logW~logL,data=BluegillLM)

test_that("makeLH() messages",{
  # ----- test for missing arguments
  makeLH(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10) |>
    expect_no_error()
  makeLH(Linf=2000,tmax=15,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10) |>
    expect_error("Need to specify an initial number of fish in the population")
  makeLH(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10) |>
    expect_error("Need to specify a maximum age")
  makeLH(N0=100,tmax=15,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10) |>
    expect_error("Need to specify a mean asymptotic length \\(mm\\)")
  makeLH(N0=100,tmax=15,Linf=2000,t0=-0.616,LWalpha=-5.453,LWbeta=3.10) |>
    expect_error("Need to specify a Brody growth coefficient")
  makeLH(N0=100,tmax=15,Linf=2000,K=0.50,LWalpha=-5.453,LWbeta=3.10) |>
    expect_error("Need to specify a time when the mean length is 0")
  makeLH(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWbeta=3.10) |>
    expect_error("Need to specify a weight-length alpha coefficient.")
  makeLH(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453) |>
    expect_error("Need to specify a weight-length beta coefficient.")

  # ----- spot test for reasonableness of values, more specific tests
  #       elsewhere; e.g., iCheckLinf, iCheckN0
  makeLH(N0=-100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10) |>
    expect_error("'N0' must be >=0")
  makeLH(N0=100,tmax=15,Linf=-2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10) |>
    expect_error("'Linf' must be >=0")
  makeLH(N0=100,tmax=c(15,20),Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10) |>
    expect_error("Only use one value in 'tmax'")
  makeLH(N0=100,tmax=15,Linf=2000,K="a",t0=-0.616,LWalpha=-5.453,LWbeta=3.10) |>
    expect_error("'K' must be a number")
  makeLH(N0=100,tmax=15,Linf=20000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10) |>
    expect_warning("A mean asymptotic length of 20000 mm seems too large")

  # ----- using results from model fittings (from above)
  # ..... gets items correctly from correct model fits
  makeLH(N0=100,tmax=15,Linf=fit1,LWalpha=fit3) |>
    expect_no_error()
  makeLH(N0=100,tmax=15,Linf=fit1,LWalpha=-5.453,LWbeta=3.10) |>
    expect_no_error()
  makeLH(N0=100,tmax=15,Linf=2000,K=0.5,t0=-0.616,LWalpha=fit3) |>
    expect_no_error()

  # ..... giving bad model fits to arguments
  makeLH(N0=100,tmax=15,Linf=fit2,LWalpha=fit3) |>
    expect_error("Names of parameters in 'nls' object are not 'Linf', 'K', and 't0'")
  makeLH(N0=100,tmax=15,Linf=fit3,LWalpha=fit3) |>
    expect_error("'Linf' given object from 'lm")
  makeLH(N0=100,tmax=15,Linf=fit1,LWalpha=fit2) |>
    expect_error("'LWalpha' given object from 'nls")
})

test_that("makeLH() output",{
  # ----- as a list
  tmp <- makeLH(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10)
  expect_true(isa(tmp,c("MAKELH","list")))
  expect_equal(length(names(tmp)),7)
  expect_equal(names(tmp),c("N0","tmax","Linf","K","t0","LWalpha","LWbeta"))

  # ----- as a vector
  tmp <- makeLH(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,
                restype="vector")
  expect_true(isa(tmp,c("MAKELH","numeric")))
  expect_true(is.numeric(tmp))
  expect_equal(length(names(tmp)),7)
  expect_equal(names(tmp),c("N0","tmax","Linf","K","t0","LWalpha","LWbeta"))

  # ----- as a list but using model results
  tmp <- makeLH(N0=100,tmax=15,Linf=fit1,LWalpha=fit3)
  expect_true(isa(tmp,c("MAKELH","list")))
  expect_equal(length(names(tmp)),7)
  expect_equal(names(tmp),c("N0","tmax","Linf","K","t0","LWalpha","LWbeta"))
  # ----- make sure model results were extracted correctly
  expect_equal(unlist(tmp[c("Linf","K","t0")]),coef(fit1))
  expect_equal(unlist(tmp[c("LWalpha","LWbeta")]),coef(fit3),ignore_attr=TRUE)
})
