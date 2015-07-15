library(faoswsFlag)
library(truncnorm)
## Create new flag table
exampleFlagTable =
    data.frame(flagObservationStatus = c("", "E", "I"),
               flagObservationWeights = c(1, 0.2, 0.6))
## Create artificial example for Supply and Utilisation Account
exampleSUA.df =
    data.frame(product = c("wheat", "wheat flour"),
               productionValue = c(100, 96),
               productionFlag = c("", "E"),
               importValue = c(10, 0),
               importFlag = c("", ""),
               exportValue = c(50, 0),
               exportFlag = c("", ""),
               foodValue = c(0, 80),
               foodFlag = c("E", "E"),
               seedValue = c(20, 0),
               seedFlag = c("", "E"),
               wasteValue = c(20, 16),
               wasteFlag = c("I", "E"),
               foodManuValue = c(100, 0),
               foodManuFlag = c("E", ""))
## Create artificial example for the unbalanced Food Balance Sheet
exampleFBS.df =
	data.frame(product = "wheat and products",
           productionValue = 220,
           importValue = 10,
           exportValue = 50,
           foodValue = 100, seedValue = 20,
           wasteValue = 40,
           foodManuValue = 100)




parameterise(obsValue = 22000, selfInformation = -log(0.8), distribution = "normal")
## $mean
## [1] 22000
##
## $sd
## [1] 0.3024634

parameterise(obsValue = 22000, selfInformation = -log(0.8), distribution = "cauchy")
## $location
## [1] 22000
##
## $scale
## [1] 0.09947184


parameterise(obsValue = 22000,
             selfInformation = -log(0.8),
             distribution = "truncNorm")
## $mean
## [1] 22000
##
## $sd
## [1] 0.3024757

## Select all the flag columns
flagColumns = grep("Flag", colnames(exampleSUA.df), value = TRUE)
## First we convert the flags to weights or confidence
(weightsSUA.df = data.frame(lapply(exampleSUA.df[, flagColumns], flag2weight)))

## Then we compute the self-information
(selfInfoSUA.df = data.frame(lapply(weightsSUA.df, selfInformation)))

## Then we sum up the self-information for each element to obtain
## the uncertainty of each element.
totalInfo.df = data.frame(lapply(selfInfoSUA.df, sum))

## Parameterise the production element with a Normal distribution
distributionise(obsValue = exampleFBS.df$productionValue,
                selfInformation = totalInfo.df$productionFlag,
                distribution = "normal")


## Parameterise the production element with a Truncated Normal distribution
distributionise(obsValue = exampleFBS.df$productionValue,
                selfInformation = totalInfo.df$productionFlag,
                distribution = "truncNorm")
                
productionDist =
    distributionise(obsValue = exampleFBS.df$productionValue,
                    selfInformation = totalInfo.df$productionFlag,
                    distribution = "truncNorm")
importDist =
    distributionise(obsValue = exampleFBS.df$importValue,
                    selfInformation = totalInfo.df$importFlag,
                    distribution = "truncNorm")

exportDist =
    distributionise(obsValue = exampleFBS.df$exportValue,
                    selfInformation = totalInfo.df$exportFlag,
                    distribution = "truncNorm")
foodDist =
    distributionise(obsValue = exampleFBS.df$foodValue,
                    selfInformation = totalInfo.df$foodFlag,
                    distribution = "truncNorm")
seedDist =
    distributionise(obsValue = exampleFBS.df$seedValue,
                    selfInformation = totalInfo.df$seedFlag,
                    distribution = "truncNorm")
wasteDist =
    distributionise(obsValue = exampleFBS.df$wasteValue,
                    selfInformation = totalInfo.df$wasteFlag,
                    distribution = "truncNorm")
foodManuDist =
    distributionise(obsValue = exampleFBS.df$foodManuValue,
                    selfInformation = totalInfo.df$foodManuFlag,
                    distribution = "truncNorm")


ll = function(x){
    -log(productionDist$pdf(x[1])) -
	log(importDist$pdf(x[2])) -
    log(exportDist$pdf(x[3])) -
    log(foodDist$pdf(x[4])) -
    log(seedDist$pdf(x[5])) -
    log(wasteDist$pdf(x[6])) -
    log(foodManuDist$pdf(x[7]))
}
constraint = function(x){
    x[1] + x[2] + x[3] - x[4] - x[5] - x[6] - x[7]
}


library(Rsolnp) ## Double check this solution, the resulting likelihood is infinite
balancedFBS =
    solnp(pars = as.numeric(exampleFBS.df[, -1]),
          fun = ll,
          eqfun = constraint,
	      eqB = 0,
          control = list(tol =1e-10))

balancedFBS$par






