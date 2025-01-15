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
                      LWalpha=-5.453,LWbeta=3.10,interactive=FALSE),
               "Must give a value for 'N0' or set 'interactive=TRUE'")
  expect_error(makeLH(N0=100,maxage=15,K=0.50,t0=-0.616,
                      LWalpha=-5.453,LWbeta=3.10,interactive=FALSE),
               "Must give a value for 'Linf' or set 'interactive=TRUE'")

  ## A few checks on reasonableness of vaules, there are more tests in internals
  expect_error(makeLH(N0=-100,maxage=15,Linf=2000,K=0.50,t0=-0.616,
                      LWalpha=-5.453,LWbeta=3.10,interactive=FALSE),
               "'N0' must be >=0")
  expect_error(makeLH(N0=100,maxage=15,Linf=-2000,K=0.50,t0=-0.616,
                      LWalpha=-5.453,LWbeta=3.10,interactive=FALSE),
               "'Linf' must be >=0")
  expect_error(makeLH(N0=c(100,200),maxage=15,Linf=2000,K=0.50,t0=-0.616,
                      LWalpha=-5.453,LWbeta=3.10,interactive=FALSE),
               "Only use one value in 'N0'")
  expect_error(makeLH(N0=100,maxage=15,Linf="a",K=0.50,t0=-0.616,
                      LWalpha=-5.453,LWbeta=3.10,interactive=FALSE),
               "'Linf' must be a number")
  expect_warning(makeLH(N0=100,maxage=15,Linf=20000,K=0.50,t0=-0.616,
                        LWalpha=-5.453,LWbeta=3.10,interactive=FALSE),
               "A mean asymptotic length of 20000 mm seems too large")
  expect_error(makeLH(N0=100,Linf=fit2,
                      LWalpha=-5.453,LWbeta=3.10,maxage=15,interactive=FALSE),
               "Names of parameters in 'nls' object are not 'Linf', 'K', and 't0'")
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
