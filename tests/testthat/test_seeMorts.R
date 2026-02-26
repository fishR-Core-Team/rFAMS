## Get results for the output and results tests
res1 <- seeMorts(cf=0.3,cm=0.2,verbose=FALSE)
sum1 <- summary(res1,verbose=FALSE)
res2 <- seeMorts(cf=c(0.1,0.3),cm=c(0.2,0.4),verbose=FALSE)
sum2 <- summary(res2,verbose=FALSE)

res3 <- seeMorts(cf=0.3,cm=0.2,type=1,verbose=FALSE)
sum3 <- summary(res1,verbose=FALSE)
res4 <- seeMorts(cf=c(0.1,0.3),cm=c(0.2,0.4),type=1,verbose=FALSE)
sum4 <- summary(res2,verbose=FALSE)

test_that("seeMorts() messages",{
  expect_error(seeMorts(),
               "Need to specify a conditional fishing mortality in 'cf'")
  expect_error(seeMorts(cm=0.3),
               "Need to specify a conditional fishing mortality in 'cf'")
  expect_error(seeMorts(cf=0.2),
               "Need to specify a conditional natural mortality in 'cm'")
  expect_error(seeMorts(cf=0.2,cm=0.1,type=0),
               "'type' must be 1 or 2 to choose a")
  expect_error(seeMorts(cf=0.2,cm=0.1,type="I"),
               "'type' must be 1 or 2 to choose a")

  expect_error(seeMorts(cm="a",cf=0.4),"'cm' must be a number")
  expect_error(seeMorts(cf="a",cm=0.4),"'cf' must be a number")
  expect_error(seeMorts(cm=0.3,cf=data.frame(cf=0.3)),"'cf' must be a vector")
  expect_error(seeMorts(cf=0.3,cm=data.frame(cm=0.3)),"'cm' must be a vector")

  expect_error(seeMorts(cm=-0.3,cf=0.4),"'cm' must be >=0")
  expect_error(seeMorts(cm=0.3,cf=-0.4),"'cf' must be >=0")
  expect_error(seeMorts(cm=1.3,cf=0.4),"'cm' must be <=1")
  expect_error(seeMorts(cm=0.3,cf=1.4),"'cf' must be <=1")

  expect_error(seeMorts(cm=c(-0.3,0.1,0.5),cf=0.5),"All 'cm' must be >=0")
  expect_error(seeMorts(cm=0.5,cf=c(-0.3,0.1,0.5)),"All 'cf' must be >=0")
  expect_error(seeMorts(cm=c(0.3,0.1,1.5),cf=0.5),"All 'cm' must be <=1")
  expect_error(seeMorts(cm=0.5,cf=c(0.3,1.1,0.5)),"All 'cf' must be <=1")

  expect_error(seeMorts(cm=c(0.3,0.1,0.5),cf=-0.5),"'cf' must be >=0")
  expect_error(seeMorts(cm=c(0.3,-0.1,0.5),cf=0.5),"All 'cm' must be >=0")

  expect_warning(seeMorts(cm=c(0.3,0.3,0.5,0.6),cf=0.2,verbose=FALSE),
                 "Duplicated values in 'cm' were dropped")
  expect_warning(seeMorts(cf=c(0.3,0.3,0.5,0.6),cm=0.2,verbose=FALSE),
                 "Duplicated values in 'cf' were dropped")
})

test_that("seeMorts() output",{
  expect_equal(class(res1),c("SEEMORTS","data.frame"))
  expect_equal(names(res1),c("cm","cf","M","F","Z","A","u","v"))
  expect_true(all(unlist(lapply(res1,is.numeric))))
  expect_equal(dim(res1),c(1,8))

  expect_equal(class(res2),c("SEEMORTS","data.frame"))
  expect_equal(names(res2),c("cm","cf","M","F","Z","A","u","v"))
  expect_true(all(unlist(lapply(res2,is.numeric))))
  expect_equal(dim(res2),c(4,8))

  expect_equal(class(sum1),"data.frame")
  expect_equal(names(sum1),c("type","unique","min","max"))
  expect_equal(sum1$type,c("cm","cf","M","F","Z","A","u","v"))
  expect_equal(dim(sum1),c(8,4))

  expect_equal(class(sum2),"data.frame")
  expect_equal(names(sum2),c("type","unique","min","max"))
  expect_equal(sum2$type,c("cm","cf","M","F","Z","A","u","v"))
  expect_equal(dim(sum2),c(8,4))
})

test_that("seeMorts() results",{
  ## Type-2 fishery results
  exp <- data.frame(cm=0.2,cf=0.3)
  exp$M <- -log(1-exp$cm)
  exp$F <- -log(1-exp$cf)
  exp$Z <- exp$M+exp$F
  exp$A <- 1-exp(-exp$Z)
  exp$u <- exp$A*exp$F/exp$Z
  exp$v <- exp$A*exp$M/exp$Z
  expect_equal(res1,exp,ignore_attr=TRUE)

  exp <- as.data.frame(expand.grid(cm=c(0.2,0.4),cf=c(0.1,0.3)))
  exp <- exp[order(exp$cm,exp$cf),]
  exp$M <- -log(1-exp$cm)
  exp$F <- -log(1-exp$cf)
  exp$Z <- exp$M+exp$F
  exp$A <- 1-exp(-exp$Z)
  exp$u <- exp$A*exp$F/exp$Z
  exp$v <- exp$A*exp$M/exp$Z
  expect_equal(res2,exp,ignore_attr=TRUE)

  expect_equal(sum1$unique,rep(1,8))
  expect_equal(sum2$unique,c(2,2,2,2,4,4,4,4))

  ## Type-1 fishery results
  exp <- data.frame(cm=0.2,cf=0.3)
  exp$M <- -log(1-exp$cm)
  exp$F <- -log(1-exp$cf)
  exp$Z <- exp$M+exp$F
  exp$A <- 1-exp(-exp$Z)
  exp$u <- exp$cf
  exp$v <- exp$cm*(1-exp$u)
  expect_equal(res3,exp,ignore_attr=TRUE)

  exp <- as.data.frame(expand.grid(cm=c(0.2,0.4),cf=c(0.1,0.3)))
  exp <- exp[order(exp$cm,exp$cf),]
  exp$M <- -log(1-exp$cm)
  exp$F <- -log(1-exp$cf)
  exp$Z <- exp$M+exp$F
  exp$A <- 1-exp(-exp$Z)
  exp$u <- exp$cf
  exp$v <- exp$cm*(1-exp$u)
  expect_equal(res4,exp,ignore_attr=TRUE)

  expect_equal(sum3$unique,rep(1,8))
  expect_equal(sum4$unique,c(2,2,2,2,4,4,4,4))
})
