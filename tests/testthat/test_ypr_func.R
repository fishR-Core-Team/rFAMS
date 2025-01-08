## Get some simple results
res <- ypr_func(minLL=355,cf=0.45,cm=0.25,N0=100,
                Linf=2000,K=0.50,t0=-0.616,
                LWalpha=-5.453,LWbeta=3.10,maxage=15,matchRicker=FALSE)

# Same, but with named vector in N0
parms <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,maxage=15)
res2 <- ypr_func(cf=0.45,cm=0.25,minLL=355,N0=parms,matchRicker=FALSE)

# Same, but with named list in N0
parms <- list(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,maxage=15)
res3 <- ypr_func(cf=0.45,cm=0.25,minLL=355,N0=parms,matchRicker=FALSE)


test_that("Three params of ypr_func() match",{
  expect_equal(res2,res)
  expect_equal(res3,res)
  expect_equal(res3,res2)
})

test_that("ypr_func() messages",{
  ## Tests are not exhaustive, see test_internals for more
  ## check for some missing values
  expect_error(ypr_func(),"Need to specify a minimum length")
  expect_error(ypr_func(cm=0.25,minLL=355,N0=100,
                        Linf=2000,K=0.50,t0=-0.616,
                        LWalpha=-5.453,LWbeta=3.10,maxage=15),
               "Need to specify a conditional fishing mortality in 'cf'.")
  expect_error(ypr_func(cf=0.45,cm=0.25,minLL=355,N0=100,
                        Linf=2000,t0=-0.616,
                        LWalpha=-5.453,LWbeta=3.10,maxage=15),
               "Need to specify a Brody growth coefficient in 'K'")
  expect_warning(ypr_func(cf=0.45,cm=0.25,minLL=35,N0=100,
                        Linf=2000,K=0.50,t0=-0.616,
                        LWalpha=-5.453,LWbeta=3.10,maxage=15),
               "A minimum length limit of harvest of 35 mm seems too small")
  ypr_func(cf=0.45,cm=0.25,minLL=2235,N0=100,
           Linf=2000,K=0.50,t0=-0.616,
           LWalpha=-5.453,LWbeta=3.10,maxage=15) |>
    expect_warning("A minimum length limit of harvest of 2235 mm seems too large") |>
    expect_warning("The set mininmum length limit of harvest")

  ## Errors when using the named vector/list
  tmp <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,maxage=15)
  expect_error(ypr_func(cf=0.45,cm=0.25,minLL=355,N0=100,Linf=tmp),
               "Only use one value in 'Linf'")
  tmp <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10)
  expect_error(ypr_func(cf=0.45,cm=0.25,minLL=355,N0=tmp),
               "'N0' must contain only one value for 'N0' or 7 named")
  tmp <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,maxage=15,derek=7)
  expect_error(ypr_func(cf=0.45,cm=0.25,minLL=355,N0=tmp),
               "'N0' must contain only one value for 'N0' or 7 named")
  tmp <- c(N0=100,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,MAXAGE=15)
  expect_error(ypr_func(cf=0.45,cm=0.25,minLL=355,N0=tmp),
               "'N0' must have named values for all of")
  tmp <- c(100,2000,0.50,-0.616,-5.453,3.10,15)
  expect_error(ypr_func(cf=0.45,cm=0.25,minLL=355,N0=tmp),
               "'N0' must have named values for")
})

test_that("ypr_func() output",{
  expect_type(res,"list")
  expect_equal(class(res),"data.frame")
  expect_equal(nrow(res),1)
  expect_equal(ncol(res),22)
  expect_equal(names(res),c("yield","exploitation","Nharvest","Ndie","Nt",
                            "avgwt","avglen","tr","Fmort","Mmort","Zmort","S",
                            "cf","cm","minLL","N0","Linf","K","t0",
                            "LWalpha","LWbeta","maxage"))
})

test_that("ypr_func() results",{
  ## Results from Jason's original ypr_func(), assumed tested against FAMS
  ores <- data.frame(yield=663135.3,exploitation=0.3966366,Nharvest=67.51261,
                     Ndie=32.48739,Nt=100,avgwt=9822.392,avglen=1113.895,
                     Fmort=0.597837,Mmort=0.2876821,Zmort=0.8855191,S=0.4125,
                     cf=0.45,cm=0.25,minLL=355,N0=100,Linf=2000,K=0.50,t0=-0.616,
                     LWalpha=-5.453,LWbeta=3.10,maxage=15)
  expect_equal(round(res$exploitation,7),ores$exploitation)
  expect_equal(round(res$yield,1),ores$yield)
  expect_equal(round(res$Nharvest,5),ores$Nharvest)
  expect_equal(round(res$Ndie,5),ores$Ndie)
  expect_equal(round(res$Nt,0),ores$Nt)
  expect_equal(round(res$avgwt,3),ores$avgwt)
  expect_equal(round(res$avglen,3),ores$avglen)
  expect_equal(round(res$Fmort,6),ores$Fmort)
  expect_equal(round(res$Mmort,7),ores$Mmort)
  expect_equal(round(res$Zmort,7),ores$Zmort)
  expect_equal(round(res$S,4),ores$S)
  expect_equal(dplyr::select(res,cf:maxage),dplyr::select(ores,cf:maxage))
})
