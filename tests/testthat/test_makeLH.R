## Get some simple results
## get some LVB results (as an example)
data(SpotVA1,package="FSA")
SpotVA1 <- SpotVA1 |> dplyr::mutate(tl=tl*25.4)
vb1 <- FSA::vbFuns()
fit1 <- nls(tl~vb1(age,Linf,K,t0),data=SpotVA1,start=FSA::vbStarts(tl~age,data=SpotVA1))
vb2 <- FSA::vbFuns(param="Original")
fit2 <- nls(tl~vb1(age,Linf,K,L0),data=SpotVA1,start=list(Linf=337,K=0.4,L0=10))


## get some LW results (as an example)
data(BluegillLM,package="FSAdata")
BluegillLM <- BluegillLM |>
  dplyr::mutate(logW=log10(wght),logL=log10(tl))
fit3 <- lm(logW~logL,data=BluegillLM)


test_that("makeLH() messages",{
  expect_no_error(makeLH(N0=100,maxage=15,Linf=2000,K=0.50,t0=-0.616,
                         LWalpha=-5.453,LWbeta=3.10))
  expect_error(makeLH(Linf=2000,maxage=15,K=0.50,t0=-0.616,
                      LWalpha=-5.453,LWbeta=3.10),
               "Need to specify an initial number of fish")
  expect_error(makeLH(N0=100,maxage=15,K=0.50,t0=-0.616,
                      LWalpha=-5.453,LWbeta=3.10),
               "Need to specify a mean asymptotic length")

  ## A few checks on reasonableness of values, there are more tests in internals
  expect_error(makeLH(N0=-100,maxage=15,Linf=2000,K=0.50,t0=-0.616,
                      LWalpha=-5.453,LWbeta=3.10),
               "'N0' must be >=0")
  expect_error(makeLH(N0=100,maxage=15,Linf=-2000,K=0.50,t0=-0.616,
                      LWalpha=-5.453,LWbeta=3.10),
               "'Linf' must be >=0")
  expect_error(makeLH(N0=c(100,200),maxage=15,Linf=2000,K=0.50,t0=-0.616,
                      LWalpha=-5.453,LWbeta=3.10),
               "Only use one value in 'N0'")
  expect_error(makeLH(N0=100,maxage=15,Linf="a",K=0.50,t0=-0.616,
                      LWalpha=-5.453,LWbeta=3.10),
               "'Linf' must be a number")
  expect_warning(makeLH(N0=100,maxage=15,Linf=20000,K=0.50,t0=-0.616,
                        LWalpha=-5.453,LWbeta=3.10),
               "A mean asymptotic length of 20000 mm seems too large")
  expect_error(makeLH(N0=100,maxage=15,Linf=fit2,LWalpha=-5.453,LWbeta=3.10),
               "Names of parameters in 'nls' object are not 'Linf', 'K', and 't0'")
  expect_error(makeLH(N0=100,maxage=15,Linf=fit3,LWalpha=-5.453,LWbeta=3.10),
               "'Linf' given object from 'lm")
  expect_error(makeLH(N0=100,maxage=15,Linf=fit1,LWalpha=fit1),
               "'LWalpha' given object from 'nls")
})

test_that("makeLH() results",{
  ## as a list
  tmp <- makeLH(N0=100,maxage=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10)
  expect_true(isa(tmp,"list"))
  expect_equal(length(names(tmp)),7)
  expect_equal(names(tmp),c("N0","maxage","Linf","K","t0","LWalpha","LWbeta"))

  ## as a vector
  tmp <- makeLH(N0=100,maxage=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,
                restype="vector")
  expect_true(is.numeric(tmp))
  expect_equal(length(names(tmp)),7)
  expect_equal(names(tmp),c("N0","maxage","Linf","K","t0","LWalpha","LWbeta"))

  ## as a list but using model results
  tmp <- makeLH(N0=100,maxage=15,Linf=fit1,LWalpha=fit3)
  expect_true(isa(tmp,"list"))
  expect_equal(length(names(tmp)),7)
  expect_equal(names(tmp),c("N0","maxage","Linf","K","t0","LWalpha","LWbeta"))
})
