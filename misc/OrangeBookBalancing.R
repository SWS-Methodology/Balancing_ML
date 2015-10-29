load("example.Rdata")
source("balancing.R")
source("defaultStandardizationParametersBalancing.R")

parameters = defaultStandardizationParametersBalancing()
parameters.df = plyr::ldply(parameters$elements, data.frame,.id = "element")

tableToBalance = merge(example,parameters.df,by="element")

tableToBalance[,balancedValue := balancing(param1 = Value,
                                           param2 = standardDeviation,
                                           dist = Dist,
                                           sign = Sign,
                                           lbounds = LB,
                                           ubounds = UB)]
tableToBalance

## Example for all not fixed elements
test1 = tableToBalance[!(standardDeviation == 0),]
test1[,balancedValue := balancing(param1 = Value,
                        param2 = standardDeviation,
                        dist = Dist,
                        sign = Sign,
                        lbounds = LB,
                        ubounds = UB)]
test1

## Example all fixed elements
# Example not balanced
test2 = tableToBalance[standardDeviation == 0,]
test2[,balancedValue := balancing(param1 = Value,
                                 param2 = standardDeviation,
                                 dist = Dist,
                                 sign = Sign,
                                 lbounds = LB,
                                 ubounds = UB)]
# Example balanced
test2[3,Value := -32789894]
test2[,balancedValue := balancing(param1 = Value,
                                  param2 = standardDeviation,
                                  dist = Dist,
                                  sign = Sign,
                                  lbounds = LB,
                                  ubounds = UB)]
test2

## Example one not fixed element
test3 = tableToBalance[2:6,]
test3[,balancedValue := balancing(param1 = Value,
                                  param2 = standardDeviation,
                                  dist = Dist,
                                  sign = Sign,
                                  lbounds = LB,
                                  ubounds = UB)]
test3
