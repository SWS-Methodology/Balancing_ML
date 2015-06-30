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

# tab = mutate(tab,Value = ifelse(is.na(Value),0,Value)) 
# We cannot do that, since we have different distributions

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
		ifelse(Dist == "alpha,beta","dbeta","dgamma"))),
		Sign = ifelse(measuredElement %in% c(51,61,71),"-","+"),
		Params = paste0("params[",match(measuredElement,elements),"]"),
		Member = ifelse(Distr!="dgamma",
			paste0(Sign," ",Distr,"(",Params,",",Values,")"),
			paste0(Sign," ",Distr,"(",Params,", rate = ",Values,")"))
	)
	
## Remove columns not necessary
ttab = ttab %>%  
	select(
		geographicAreaM49, timePointYears, 
		measuredElement, measuredItemCPC, Member
	)
	
tab_like = ttab %>%
	group_by(geographicAreaM49, timePointYears, measuredItemCPC) %>%
	summarize(
		Likelihood = paste(Member, collapse = " ")
	)
	

optimization = function(data,params){
	fit = optim
}


	
optim = tab_like %>%
	
	

fit = optim(par = c(10,2), fn = likelihood, lower = rep(0,9), method = "L-BFGS-B")
f = sum(fit$par)
c(fit$par,f)
# plot(-10:20, sapply(-10:20, function(x){likelihood(c(x,2))}))

ttab2 %>% 
	group_by(measuredItemCPC) %>% 
	
	

ttab2



likelihood = function(params){
    p = params[1]
    i = params[2]
    f = p + i
    s = 0
    -dnorm(p, mean = Pmean, sd = Psigma, log = TRUE) -
        dnorm(f, mean = Fmean, sd = Fsigma, log = TRUE) -
        dnorm(i, mean = Imean, sd = Isigma, log = TRUE)# -
#        dnorm(s, mean = Smean, sd = Ssigma, log = TRUE)
}
	

### ANOTHER APPROACH, but not finished ###


idvar = c("geographicAreaM49","measuredElement","measuredItemCPC","timePointYears")
timevar = c("fbsDistribParam","Value")

tab1 = spread(tab, fbsDistribParam, Value)
## Remove Food Consumption 171 and Statistical Discrepancy 181
tab2 = filter(tab1, !(measuredElement %in% c(171,181)))
