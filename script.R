## LEGEND

## 51 Production
## 61 Imports
## 71 Stock Changes
## 91 Exports
## 101 Feed
## 111 Seed
## 121 Losses
## 141 Food Availability
## 151 Industrial Uses

## Additional but not used for the balancing. To be filtered out
## 171 Food Consumption
## 181 Statistical Discrepancy

### BALANCING EQUATION
## Production + Imports - Exports + Stock = Feed + Seed + Food Availability + Industrial Uses + Losses
## - Production - Imports + Exports - Stock + Feed + Seed + Food Availability + Industrial Uses + Losses 


suppressMessages({
	library(faosws)
	library(faoswsUtil)
	library(data.table)
	library(magrittr)
	library(reshape2)
	library(dplyr)
})


# Just temporary to check 
setwd("~/Desktop/FAO/AA_new_balancing/")

tab = tbl_df(fread("sample_balance_table.csv",header=T))

## Set to 0 the NAs (this should be structural zeros)
tab = mutate(tab,Value = ifelse(is.na(Value),0,Value))

## Remove additional value not necessary in the balacing
tab_fl = filter(tab, !(measuredElement %in% c(171,181)))

ttab = tab_fl %>%
	group_by(geographicAreaM49,timePointYears,measuredElement,measuredItemCPC) %>%
	summarize(
		Dist = paste(fbsDistribParam, collapse=","),
		Values = paste(Value, collapse = ",")) %>% 
	mutate(
		## Add distribution information
		Distr = ifelse(Dist=="mu,sigma", "dnorm", 
		ifelse(Dist == "logmu,logsigma","dlnorm", 
		ifelse(Dist == "alpha,beta","dbeta","dgamma")))
	)
	

tts = ttab %>%
	group_by(measuredItemCPC) %>%
	summarize(
		all = n()
	) %>% filter(all>7)

ab = filter(ttab,measuredItemCPC== 116)

likelihood = function(data,params){
	## Element of the equation (the order is important for the equation)
	###### EQUATION ######
	## - Production - Imports + Exports - Stock + Feed 
	## + Seed + Food Availability + Industrial Uses + Losses
	## - 51 - 61 + 91 - 71 + 101 + 111 + 141 + 151 + 121
	#####
	elements =  c(51,61,91,71,101,111,141,151,121)
	names(elements) = c("Production","Imports","Exports","StockChanges",
			"Feed","Seed","Food","Industrial","Losses")
	signs = c(-1,-1,+1,-1,+1,+1,+1,+1,+1)
	signs = presents*signs
	## Check if the element is present in the commodity we are selecting
	presents = sapply(elements, function(x) NROW(filter(data, measuredElement == x)))
	## Parameters to zero if element not present
	params = params*presents	
	# - Production - Imports + Exports - Stock + Feed + 
	# Seed + Food Availability + Industrial Uses + Losses
	## The equation is data dependent, otherwise it does not work cause it cannot find elements
	parse(text = )
	
	
}


fit = optim(par = c(), fn = likelihood, lower = rep(0,9), method = "L-BFGS-B")
f = sum(fit$par)
c(fit$par,f)
# plot(-10:20, sapply(-10:20, function(x){likelihood(c(x,2))}))

ttab2 %>% 
	group_by(measuredItemCPC) %>% 
	
	

ttab2



	

### ANOTHER APPROACH, but not finished ###


idvar = c("geographicAreaM49","measuredElement","measuredItemCPC","timePointYears")
timevar = c("fbsDistribParam","Value")

tab1 = spread(tab, fbsDistribParam, Value)
## Remove Food Consumption 171 and Statistical Discrepancy 181
tab2 = filter(tab1, !(measuredElement %in% c(171,181)))
