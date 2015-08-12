##' Force Balance
##' 
##' This function takes values and their signs, as well as whether or not 
##' certain elements are fixed, and it forces the equation to be balanced (if 
##' possible).  It does not consider any errors in the variables, as this result
##' is simply meant to be an initial parameter in the feasability space for 
##' future optimization.
##' 
##' @param value A vector of the values in the balancing equation.
##' @param sign A vector of negative 1's and positive 1's indicating whether an 
##'   element is negative or positive in the balance (or, on the left vs. on the
##'   right side of the balance).
##' @param fixed A vector of logicals indicating whether values are fixed or
##'   adjustable.
##' @param lowerBound A vector of lower bounds for each element.
##' @param standardError A vector of the same length as value, used to
##'   prioritize where deltas should be allocated.  Differences will first be
##'   allocated to the variable with the largest variability down to the
##'   variable with the smallest variability.  Defaults to all 1's, as the
##'   relative magnitude is all that matters for the sorting.
##'   
##' @example
##' ## Example of positive adjusted down
##' forceBalance(value = c(100, 10, 30, 60), sign = c(1, 1, -1, -1),
##'              fixed = c(F, T, T, T), lowerBound = c(0,0,0,0))
##' ## Example of negative adjusted down
##' forceBalance(value = c(100, 10, 50, 80), sign = c(1, 1, -1, -1),
##'              fixed = c(T, T, F, T), lowerBound = c(0,0,0,0))
##' ## Example of multiple adjustments
##' forceBalance(value = c(10, 10, 30, 50), sign = c(1, 1, -1, -1),
##'              fixed = c(T, T, F, F), lowerBound = c(0,0,0,0))
##' ## Example of non-zero lower bound
##' forceBalance(value = c(10, 10, 30, 50), sign = c(1, 1, -1, -1),
##'              fixed = c(T, T, T, F), lowerBound = c(0,0,0,-Inf))
##' 
##' @return A vector of the same length as value, with values updated to satisfy
##'   the balance.
##'   

forceBalance = function(value, sign, fixed, lowerBound = rep(0, length(value)),
                        upperBound = rep(Inf, length(value)),
                        standardError = rep(1, length(value))){
    
    ## Input checks
    N = length(value)
    stopifnot(length(sign) == N)
    stopifnot(length(fixed) == N)
    stopifnot(length(lowerBound) == N)
    stopifnot(length(standardError) == N)
    stopifnot(sign %in% c(-1, 1))
    stopifnot(is(fixed, "logical"))
    
    posSum = sum(value[sign == 1])
    negSum = sum(value[sign == -1])
    delta = posSum - negSum
    ## Start with the passed value as the output, and adjust from here
    output = value
    if(delta == 0){
        return(output)
    ## The next two conditions are trickier: we have to adjust elements either 
    ## up or down, but we must also ensure they don't go outside their bounds.
    } else if(delta != 0){
        ## How much can we adjust each element by?
        posAdjByElement = ifelse(fixed, 0,
                          ifelse(sign == 1, (upperBound - value),
                          ifelse(sign == -1, (value - lowerBound), NA)))
        negAdjByElement = ifelse(fixed, 0,
                          ifelse(sign == 1, (value - lowerBound),
                          ifelse(sign == -1, (upperBound - value), NA)))
        ## Iteratively allocate the difference to adjustable elements
        adjustOrder = order(-standardError)
        i = 1
        if(delta > 0){
            while(delta > 0 & i <= N){
                currentAdj = min(delta, negAdjByElement[adjustOrder[i]])
                output[adjustOrder[i]] = output[adjustOrder[i]] -
                    currentAdj * sign[adjustOrder[i]]
                delta = delta - currentAdj
                i = i + 1
            }
        } else {
            while(delta < 0 & i <= N){
                currentAdj = min(abs(delta), posAdjByElement[adjustOrder[i]])
                output[adjustOrder[i]] = output[adjustOrder[i]] +
                    currentAdj * sign[adjustOrder[i]]
                delta = delta + currentAdj
                i = i + 1
            }
        }
        if(abs(delta) > 0){
            ## Must raise an error here, as not satisfying the constraints will 
            ## kill our optimizer.  We must have not been able to allocate
            ## adjustments in the previous steps.
            stop("Cannot force initial constraint to be satisfied.")
        }
    }
    
    output
}