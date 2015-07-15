load("example.Rdata")

parameters = defaultStandardizationParametersBalancing()
parameters.df = plyr::ldply(parameters$elements, data.frame,.id = "element")

## Just for myself to understand
#fbsElements = as.character(c(5113, 5025, 5312, 5510, 5327, 5421, 5520, 5525,
#                5120, 5023, 5141, 71, 5600, 5900))
#legend = data.frame(element = fbsElements,
#                    names = c("OpeningStock", "AreaSown", "AreaHarv", "Production",
#                              "Input", "Yield", "Feed", "Seed", "Waste", "Processed",
#                              "Food","StockChange", "Imports", "Exports"))

## Name of the elements
#merge(example,legend,by="element")

tableToBalance = merge(example,parameters.df,by="element")
#tableToBalance[Value == 0 & standardDeviation == 0, LB := 0]
#tableToBalance[Value == 0 & standardDeviation == 0, UB := 0]

tableToBalance[,balancedValue := balancing(param1 = Value,
                                           param2 = standardDeviation,
                                           dist = Dist,
                                           sign = Sign,
                                           lbounds = LB,
                                           ubounds = UB,
                                           optim = "L-BFGS-B")]


## Testing just for the rows with no 0 standard deviation
test = tableToBalance[!(standardDeviation == 0),]
test[,balancedValue := balancing(param1 = Value,
                        param2 = standardDeviation,
                        dist = Dist,
                        sign = Sign,
                        lbounds = LB,
                        ubounds = UB,
                        optimize =  "L-BFGS-B")]

