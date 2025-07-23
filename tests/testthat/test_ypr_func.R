## Get some simple results
LH <- makeLH(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10)
res1 <- yprBH_func(cf=0.45,cm=0.25,minLL=355,lhparms=LH,matchRicker=FALSE)

# Same, but with named vector in N0
LH <- makeLH(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,
             restype="vector")
res2 <- yprBH_func(cf=0.45,cm=0.25,minLL=355,lhparms=LH,matchRicker=FALSE)


test_that("Two types of lhparams of yprBH_func() match",{
  expect_equal(res2,res1)
})

test_that("yprBH_func() messages",{
  ## Tests are not exhaustive, see test_internals for more
  ## check for some missing values
  expect_error(yprBH_func(),"Need to specify a minimum length")
  # expect_error(yprBH_func(minLL=355,cm=0.25,lhparms=LH),
  #              "Need to specify a conditional fishing mortality in 'cf'.")
  # expect_error(yprBH_func(,minLL=355,cf=0.45,lhparms=LH),
  #              "Need to specify a conditional natural mortality in 'cm'.")
  expect_warning(yprBH_func(minLL=35,cf=0.45,cm=0.25,lhparms=LH),
                 "A minimum length limit of harvest of 35 mm seems too small")
  # yprBH_func(minLL=2235,cf=0.45,cm=0.25,lhparms=LH) |>
  #   expect_warning("A minimum length limit of harvest of 2235 mm seems too large") |>
  #   expect_warning("The set mininmum length limit of harvest")

  ## Errors when using the named vector/list
  # tmp <- c(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10,derek=7)
  # expect_error(yprBH_func(cf=0.45,cm=0.25,minLL=355,lhparms=tmp),
  #              "'lhparms' should have only 7")
  # tmp <- c(N0=100,tmax=15,Linf=2000,K=0.50,t0=-0.616,LWalpha=-5.453,LWbeta=3.10)
  # expect_error(yprBH_func(cf=0.45,cm=0.25,minLL=355,lhparms=tmp),
  #              "'lhparms' does not contain a value for: tmax")
  # tmp <- c(100,15,2000,0.50,-0.616,-5.453,3.10)
  # expect_error(yprBH_func(cf=0.45,cm=0.25,minLL=355,lhparms=tmp),
  #              "Life history parameters in 'lhparms' must be NAMED.")
})

test_that("yprBH_func() output",{
  expect_type(res1,"list")
  expect_equal(class(res1),"data.frame")
  expect_equal(nrow(res1),1)
  expect_equal(ncol(res1),23)
  expect_equal(names(res1),c("yield","u","Nharvest","Ndie",
                             "avgwt","avglen","Nt","tr","F","M","Z","S",
                             "cf","cm","minLL","N0","Linf","K","t0",
                             "LWalpha","LWbeta","tmax","notes"))
  expect_equal(res1$notes,"Nt>N0")
})

test_that("yprBH_func() results",{
  ## Results from Jason's original yprBH_func(), assumed tested against FAMS
  ores <- data.frame(yield=663135.3,exploitation=0.3966366,Nharvest=67.51261,
                     Ndie=32.48739,Nt=100,avgwt=9822.392,avglen=1113.895,
                     F=0.597837,M=0.2876821,Z=0.8855191,S=0.4125,
                     cf=0.45,cm=0.25,minLL=355,N0=100,Linf=2000,K=0.50,t0=-0.616,
                     LWalpha=-5.453,LWbeta=3.10,tmax=15)
  expect_equal(round(res1$u,7),ores$exploitation)
  expect_equal(round(res1$yield,1),ores$yield)
  expect_equal(round(res1$Nharvest,5),ores$Nharvest)
  expect_equal(round(res1$Ndie,5),ores$Ndie)
  expect_equal(round(res1$Nt,0),ores$Nt)
  expect_equal(round(res1$avgwt,3),ores$avgwt)
  expect_equal(round(res1$avglen,3),ores$avglen)
  expect_equal(round(res1$F,6),ores$F)
  expect_equal(round(res1$M,7),ores$M)
  expect_equal(round(res1$Z,7),ores$Z)
  expect_equal(round(res1$S,4),ores$S)
  expect_equal(dplyr::select(res1,cf:tmax),dplyr::select(ores,cf:tmax))
})
