#### NEW STRATEGY ####
## 1. Another otpimization algorithm
## 2. Give bounds from 0 to 0 for structural zeros


## Legend elements
### 51 Production
### 61 Imports
### 71 Stock Changes
### 91 Exports
### 101 Feed
### 111 Seed
### 121 Losses
### 141 Food Availability
### 151 Industrial Uses

## Additional element not in the equation
### 171 Food Consumption
### 181 Statistical Discrepancy

### Balancing equation
## - Production - Imports + Exports - Stock + 
## + Feed + Seed + Food Availability + 
## + Industrial Uses + Losses = 0


suppressMessages({
#	library(faosws)
#	library(faoswsUtil)
	library(data.table)
#	library(magrittr)
#	library(reshape2)
	library(dplyr)
	library(tidyr)
})






# Just temporary to check 
setwd("~/Documents/Balancing_ML/")

tab = tbl_df(fread("sample_balance_table.csv",header=T))

## Set to 0 the NAs (this should be structural zeros)
## NAs has to be removed and then back in the balancing
NAs = filter(tab,is.na(Value))

tab = filter(tab,!is.na(Value))

## Remove additional value not necessary in the balacing
tab_fl = filter(tab, !(measuredElement %in% c(171,181)))

## Elements of the equation, this is important for the sign
elements =  c(51,61,91,71,101,111,141,151,121)
names(elements) = c("Production","Imports","Exports","StockChanges",
			"Feed","Seed","Food","Industrial","Losses")

## For each itemCPC all values per line
ttab = tab_fl %>%
	group_by(geographicAreaM49, timePointYears, 
		measuredElement,measuredItemCPC) %>%
	summarize(
		Dist = paste(fbsDistribParam, collapse=","),
		Values = paste(Value, collapse = ",")
	)
		
## Get information of of the distribution
## and save it as a member of the equation
ttab = ttab %>% 
	mutate(
		## Add distribution information
		Distr = ifelse(Dist=="mu,sigma", "dnorm", 
		ifelse(Dist == "logmu,logsigma","dlnorm", 
		ifelse(Dist == "alpha,beta","dbeta","dexp"))),
		Params = paste0("params[",match(measuredElement,elements),"]"),
		Member = paste0("- ",Distr,"(",Params, ",",Values,",log=T)")
	) %>% rowwise() %>%
		mutate(
			InitialValue = unlist(strsplit(Values,","))[1]
		)
	
## Remove columns not necessary
ttab = ttab %>%  
	select(
		geographicAreaM49, timePointYears, 
		measuredElement, measuredItemCPC, Params, Member, InitialValue
	)

## Table per commodity with the balancing equation directly	
elements

ttab_like = ttab %>%
	group_by(geographicAreaM49, timePointYears, measuredItemCPC) %>%
	summarize(
		presents = paste((elements %in% measuredElement)*1,collapse=","),
		signs = "-,-,+,-,+,+,+,+,+",
		params = paste(Params,collapse=","),
		nelements = n(),
		initialValues = paste(InitialValue,collapse=","),
		Likelihood = paste(Member, collapse = " ")
	)
	

ttab_like2 = ttab_like %>%
	mutate(
		parsig = paste0(unlist(strsplit(signs,","))[as.numeric(unlist(strsplit(presents,",")))],collapse=",")
	)



example = filter(ttab_like,nelements==8)




#likelihood = function(params){
    prod = params[1]
    imp = params[2]
    #stock = params[3]
    #food = prod + imp + stock
    food = params[3]
    stock = food - prod - imp
    -dnorm(prod, mean = Pmean, sd = Psigma, log = TRUE) -
        dnorm(food, mean = Fmean, sd = Fsigma, log = TRUE) -
        dnorm(imp, mean = Imean, sd = Isigma, log = TRUE) -
        dnorm(stock, mean = Smean, sd = Ssigma, log = TRUE)
}




## Little example

#Pmean = 10
#Psigma = 0.001
#Imean = 2
#Isigma = .5
#Fmean = 3
#Fsigma = 0.1
#Smean = 0
#Ssigma = 10

#likelihood = function(params){
    prod = params[1]
    imp = params[2]
    #stock = params[3]
    #food = prod + imp + stock
    food = params[3]
    stock = food - prod - imp
    -dnorm(prod, mean = Pmean, sd = Psigma, log = TRUE) -
        dnorm(food, mean = Fmean, sd = Fsigma, log = TRUE) -
        dnorm(imp, mean = Imean, sd = Isigma, log = TRUE) -
        dnorm(stock, mean = Smean, sd = Ssigma, log = TRUE)
}

#likelihood(c(10, 2,3))
#likelihood(c(100, 2,3))

#fit = optim(par = c(10, 2, 3), fn = likelihood, lower = rep(0, 3),
            method = "L-BFGS-B")

#stock = fit$par[3] - fit$par[1] - fit$par[2]
###food = sum(fit$par)
#c(fit$par, stock)

plot( -10:100, sapply(-10:100, function(x){likelihood(c(10, x))}))





### Marco: need to work on that as soon as I understand the previous problem

#optim = tab_like %>%
#	rowwise() %>%
#	mutate(
#		params = optim(par = rep(0,9), 
#			fn = function(params){eval(parse(text = Likelihood))},
#			lower = rep(0,9),
#			method = "L-BFGS-B")$par
#	)



	
### ANOTHER APPROACH, but not finished ###
idvar = c("geographicAreaM49","measuredElement","measuredItemCPC","timePointYears")
timevar = c("fbsDistribParam","Value")
tab1 = spread(tab, fbsDistribParam, Value)
## Remove Food Consumption 171 and Statistical Discrepancy 181
tab2 = filter(tab1, !(measuredElement %in% c(171,181)))







###########################################################################
###########################################################################
###########################################################################
###########################################################################

parameterise2 = function (Values, distribution) 
{
    switch(distribution, dnorm = {
        mean = unlist(strsplit(x = Values, split = ","))[1]
        sd = unlist(strsplit(x = Values, split = ","))[2]
        list(mean = mean, sd = sd)
    }, dbeta = {
        alpha = unlist(strsplit(x = Values, split = ","))[1]
        scale = unlist(strsplit(x = Values, split = ","))[2]
        list(alpha = alpha, beta = beta)
    }, dlnorm = {
        meanlog = unlist(strsplit(x = Values, split = ","))[1]
        sdlog = unlist(strsplit(x = Values, split = ","))[2]
        list(meanlog = meanlog, sdlog = sdlog)
    }, exponential = {
        lambda = unlist(strsplit(x = Values, split = ","))[1]
        #warning("Exponential distribution has mode at zero")
        list(rate = lambda)
    })
}

distributionise = function (Values, distribution) 
{
    parameters = parameterise2(Values = Values, distribution = distribution)
    switch(distribution, dnorm = {
        list(pdf = with(parameters, function(x) dnorm(x, mean = mean, 
            sd = sd)), parameters = parameters)
    }, dbeta = {
        list(pdf = with(parameters, function(x) dbeta(x, alpha = alpha, 
            beta = beta)), parameters = parameters)
    }, logNorm = {
        list(pdf = with(parameters, function(x) dlnorm(x, meanlog = meanlog, 
            sdlog = sdlog)), parameters = parameters)
    }, dexp = {
        list(pdf = with(parameters, function(x) dexp(x, 
            rate = rate)), parameters = parameters)
    })
}


productionDist =
    distributionise(obsValue = exampleFBS.df$productionValue,
                    selfInformation = totalInfo.df$productionFlag,
                    distribution = "truncNorm")
                    
                    
                    

                    
                    
                    
                   