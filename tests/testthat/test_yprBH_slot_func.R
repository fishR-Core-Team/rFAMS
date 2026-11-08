## ===== Get Some Results for Use Below ========================================
## ----- lhparms as a list
## ..... protected slot with and without loi
LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
pslot1a <- yprBH_slot_func(lowerSL=250,upperSL=325,
                           cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
                           lhparms=LH,recruitmentTL=200,loi=NULL,matchRicker=FALSE)
pslot1b <- yprBH_slot_func(lowerSL=250,upperSL=325,
                           cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
                           lhparms=LH,recruitmentTL=200,loi=c(200,300),matchRicker=FALSE)
## ..... harvest slot with and without loi
hslot1a <- yprBH_slot_func(lowerSL=250,upperSL=325,
                           cfBelow=0,cfIn=0.25,cfAbove=0,cm=0.4,
                           lhparms=LH,recruitmentTL=NULL,loi=NULL,matchRicker=FALSE)
hslot1b <- yprBH_slot_func(lowerSL=250,upperSL=325,
                           cfBelow=0,cfIn=0.25,cfAbove=0,cm=0.4,
                           lhparms=LH,recruitmentTL=NULL,loi=c(200,300),matchRicker=FALSE)

## ----- lhparms as a vector
LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273,
             restype="vector")
pslot2a <- yprBH_slot_func(lowerSL=250,upperSL=325,
                           cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
                           lhparms=LH,recruitmentTL=200,loi=NULL,matchRicker=FALSE)
pslot2b <- yprBH_slot_func(lowerSL=250,upperSL=325,
                           cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
                           lhparms=LH,recruitmentTL=200,loi=c(200,300),matchRicker=FALSE)
## ..... harvest slot with and without loi
hslot2a <- yprBH_slot_func(lowerSL=250,upperSL=325,
                           cfBelow=0,cfIn=0.25,cfAbove=0,cm=0.4,
                           lhparms=LH,recruitmentTL=NULL,loi=NULL,matchRicker=FALSE)
hslot2b <- yprBH_slot_func(lowerSL=250,upperSL=325,
                           cfBelow=0,cfIn=0.25,cfAbove=0,cm=0.4,
                           lhparms=LH,recruitmentTL=NULL,loi=c(200,300),matchRicker=FALSE)

## expectations
exp_nms2 <- c("yieldTotal","yieldBelow","yieldIn","yieldAbove","nharvestTotal",
              "ndieTotal","nharvestBelow","nharvestIn","nharvestAbove","n0die",
              "ndieBelow","ndieIn","ndieAbove","nrBelow","nrIn","nrAbove",
              "trBelow","trIn","trAbove","avglenBelow","avglenIn","avglenAbove",
              "avgwtBelow","avgwtIn","avgwtAbove","nAt200","nAt300","cm",
              "expBelow","expIn","expAbove","FBelow","FIn","FAbove","MBelow",
              "MIn","MAbove","ZBelow","ZIn","ZAbove","SBelow","SIn","SAbove",
              "cfBelow","cfIn","cfAbove","recruitmentTL","lowerSL","upperSL","N0",
              "Linf","K","t0","LWalpha","LWbeta","tmax")
exp_nms1 <- exp_nms2[!startsWith(exp_nms2,"nAt")]

## ===== Test Output Types =====================================================
test_that("Two types of lhparams of yprBH_slot_func() match",{
  expect_equal(pslot1a,pslot2a)
  expect_equal(hslot1a,hslot2a)
  expect_equal(pslot1b,pslot2b)
  expect_equal(hslot1b,hslot2b)
})

test_that("yprBH_slot_func() output",{
  # ----- data types, sizes, and names
  # ..... tests without loi and protected slot
  expect_type(pslot1a,"list")
  expect_equal(class(pslot1a),"data.frame")
  expect_equal(nrow(pslot1a),1)
  expect_equal(ncol(pslot1a),length(exp_nms1))
  expect_equal(names(pslot1a),exp_nms1)

  # ..... tests with loi and protected slot
  expect_type(pslot1b,"list")
  expect_equal(class(pslot1b),"data.frame")
  expect_equal(nrow(pslot1b),1)
  expect_equal(ncol(pslot1b),length(exp_nms2))
  expect_equal(names(pslot1b),exp_nms2)

  # ..... tests without loi and protected slot
  expect_type(hslot1a,"list")
  expect_equal(class(hslot1a),"data.frame")
  expect_equal(nrow(hslot1a),1)
  expect_equal(ncol(hslot1a),length(exp_nms1))
  expect_equal(names(hslot1a),exp_nms1)

  # ..... tests with loi and protected slot
  expect_type(hslot1b,"list")
  expect_equal(class(hslot1b),"data.frame")
  expect_equal(nrow(hslot1b),1)
  expect_equal(ncol(hslot1b),length(exp_nms2))
  expect_equal(names(hslot1b),exp_nms2)
})



# ## ===== Test Message Errors and Warnings ======================================
# test_that("yprBH_slot_func() messages",{
#   # ----- no errors or warnings
#   # ..... protected slot
#   LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_no_error()
#   # ..... inverse/harvest slot
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0,cfIn=0.3,cfAbove=0,cm=0.4,
#                   lhparms=LH,loi=c(200,300)) |>
#     expect_no_error()
#
#   # ----- test for missing arguments
#   yprBH_slot_func(upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200) |>
#     expect_error("Need to specify a lower slot limit total length")
#
#   yprBH_slot_func(lowerSL=250,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200) |>
#     expect_error("Need to specify a upper slot limit total length")
#
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200) |>
#     expect_error("Need to specify a conditional fishing mortality under the slot")
#
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200) |>
#     expect_error("Need to specify a conditional fishing mortality in the slot")
#
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cm=0.4,
#                   lhparms=LH,recruitmentTL=200) |>
#     expect_error("Need to specify a conditional fishing mortality above the slot")
#
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,
#                   lhparms=LH,recruitmentTL=200) |>
#     expect_error("Need to specify a conditional natural mortality in 'cm'")
#
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   recruitmentTL=200) |>
#     expect_error("Need to specify a list or vector of life history parameters")
#
#   # ----- test for bad values, more thorough testing is elsewhere;
#   #       e.g., iCheckLinf(), iCheckN0()
#   # ..... protected slot
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=-200,loi=c(200,300)) |>
#     expect_error("'recruitmentTL' must be >=0")
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=275,loi=c(200,300)) |>
#     expect_error("'recruitmentTL' cannot be greater than 'lowerSL'")
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=600,loi=c(200,300)) |>
#     expect_error("'recruitmentTL' cannot be greater than 'Linf'")
#
#   yprBH_slot_func(lowerSL=-250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("'lowerSL' must be >=0")
#   yprBH_slot_func(lowerSL=450,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("'lowerSL' must be less than 'upperSL'")
#   yprBH_slot_func(lowerSL=650,upperSL=725,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("The lower slot limit total length \\(=650\\) mm cannot be greater")
#   yprBH_slot_func(lowerSL=c(250,275),upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("Only use one value in 'lowerSL'")
#
#   yprBH_slot_func(lowerSL=250,upperSL=-325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("'upperSL' must be >=0")
#   yprBH_slot_func(lowerSL=250,upperSL=625,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("The upper slot limit total length \\(=625\\) mm cannot be greater")
#   yprBH_slot_func(lowerSL=250,upperSL=c(300,325),
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("Only use one value in 'upperSL'")
#
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=-0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("'cfBelow' must be >=0")
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=1.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("'cfBelow' must be <=1")
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=c(0.25,0.35),cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("Only use one value in 'cfBelow'")
#
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=-0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("'cfAbove' must be >=0")
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=1.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("'cfAbove' must be <=1")
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=c(0.15,0.25),cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("Only use one value in 'cfAbove'")
#
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=-0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("'cm' must be >=0")
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=1.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("'cm' must be <=1")
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=c(0.4,0.5),
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("Only use one value in 'cm'")
#
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=-200) |>
#     expect_error("'loi' must be >=0")
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(-200,300)) |>
#     expect_error("All 'loi' must be >=0")
#
#   # ..... spot tests for bad values in lhparms, more thorough testing is
#   #       elsewhere; e.g., iCheckLinf(), iCheckN0()
#   tmp <- list(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
#   LH <- tmp
#   LH["N0"] <- -100
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("'N0' must be >=0")
#   LH <- tmp
#   LH["Linf"] <- "a"
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("'Linf' must be a number")
#   LH <- list(N0=100,tmax=15,Linf=592,K=c(0.3,0.5),t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_error("Only use one value in 'K'")
#   LH <- tmp
#   LH["LWbeta"] <- 5
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0.25,cfIn=0,cfAbove=0.15,cm=0.4,
#                   lhparms=LH,recruitmentTL=200,loi=c(200,300)) |>
#     expect_warning("A weight-length beta coefficient of 5 seems too large")
#
#   # ..... inverse/harvest slot
#   LH <- makeLH(N0=100,tmax=15,Linf=592,K=0.20,t0=-0.3,LWalpha=-5.528,LWbeta=3.273)
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0,cfIn=-0.3,cfAbove=0,cm=0.4,
#                   lhparms=LH,loi=c(200,300)) |>
#     expect_error("'cfIn' must be >=0")
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0,cfIn=1.3,cfAbove=0,cm=0.4,
#                   lhparms=LH,loi=c(200,300)) |>
#     expect_error("'cfIn' must be <=1")
#   yprBH_slot_func(lowerSL=250,upperSL=325,
#                   cfBelow=0,cfIn=c(0.3,0.4),cfAbove=0,cm=0.4,
#                   lhparms=LH,loi=c(200,300)) |>
#     expect_error("Only use one value in 'cfIn'")
# })
