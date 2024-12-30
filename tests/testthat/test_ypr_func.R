## Get some simple results
res <- ypr_func(minlength=355,cf=0.45,cm=0.25,N0=100,
                linf=2000,K=0.50,t0=-0.616,
                LWalpha=-5.453,LWbeta=3.10,maxage=15)

test_that("ypr_func() messages",{
  ## Tests are not exhaustive, see test_internals for more
  ## check for some missing values
  expect_error(ypr_func(),"Need to specify a conditional fishing mortality")
  expect_error(ypr_func(cm=0.25,minlength=355,N0=100,
                        linf=2000,K=0.50,t0=-0.616,
                        LWalpha=-5.453,LWbeta=3.10,maxage=15),
               "Need to specify a conditional fishing mortality in 'cf'.")
  expect_error(ypr_func(cf=0.45,cm=0.25,minlength=355,N0=100,
                        linf=2000,t0=-0.616,
                        LWalpha=-5.453,LWbeta=3.10,maxage=15),
               "Need to specify a Brody growth coefficient in 'K'")
  expect_warning(ypr_func(cf=0.45,cm=0.25,minlength=35,N0=100,
                        linf=2000,K=0.50,t0=-0.616,
                        LWalpha=-5.453,LWbeta=3.10,maxage=15),
               "A minimum length limit of harvest of 35 mm seems too small")

})

test_that("ypr_func() output",{
  expect_type(res,"list")
  expect_equal(class(res),"data.frame")
  expect_equal(nrow(res),1)
  expect_equal(ncol(res),22)
  expect_equal(names(res),c("exploitation","yield","Nharvest","Ndie","wt",
                            "avgl","Nt","tr","Fmort","Mmort","Zmort","S",
                            "cf","cm","minlength","N0","linf","K","t0",
                            "LWalpha","LWbeta","maxage"))
})

test_that("ypr_func() results",{
  ## Results from Jason's original ypr_func(), assumed tested against FAMS
  ores <- data.frame(exploitation=0.3966366,yield=663135.3,Nharvest=67.51261,
                     Ndie=32.48739,wt=9822.392,avgl=1113.895,Nt=100,
                     Fmort=0.597837,Mmort=0.2876821,Zmort=0.8855191,S=0.4125,
                     cf=0.45,cm=0.25,minlength=355,N0=100,linf=2000,K=0.50,t0=-0.616,
                     LWalpha=-5.453,LWbeta=3.10,maxage=15)
  expect_equal(round(res$exploitation,7),ores$exploitation)
  expect_equal(round(res$yield,1),ores$yield)
  expect_equal(round(res$Nharvest,5),ores$Nharvest)
  expect_equal(round(res$Ndie,5),ores$Ndie)
  expect_equal(round(res$wt,3),ores$wt)
  expect_equal(round(res$avgl,3),ores$avgl)
  expect_equal(round(res$Nt,0),ores$Nt)
  expect_equal(round(res$Fmort,6),ores$Fmort)
  expect_equal(round(res$Mmort,7),ores$Mmort)
  expect_equal(round(res$Zmort,7),ores$Zmort)
  expect_equal(round(res$S,4),ores$S)
  expect_equal(dplyr::select(res,cf:maxage),dplyr::select(ores,cf:maxage))
})
