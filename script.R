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
## + Industrial Uses + Losses 


suppressMessages({
	library(faosws)
	library(faoswsUtil)
	library(data.table)
	library(magrittr)
	library(reshape2)
	library(dplyr)
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
		Member = paste0("+ ",Distr,"(", Params, ",",Values,", log = T)")
	)
	
## Remove columns not necessary
ttab = ttab %>%  
	select(
		geographicAreaM49, timePointYears, 
		measuredElement, measuredItemCPC, Member
	)


## Table per commodity with the balancing equation directly	
tab_like = ttab %>%
	group_by(geographicAreaM49, timePointYears, measuredItemCPC) %>%
	summarize(
		Likelihood = paste(Member, collapse = " ")
	)





### SUM OF ALL PROBABILITIES THE LIKELIHOOD
### ONE ELEMENT CALCULATED AS DIFFERENCE, tricky





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
