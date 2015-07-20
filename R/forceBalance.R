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

forceBalance = function(value, sign, fixed, lowerBound = rep(0, length(value))){
    
    ## Input checks
    N = length(value)
    stopifnot(length(sign) == N)
    stopifnot(length(fixed) == N)
    stopifnot(length(lowerBound) == N)
    stopifnot(sign %in% c(-1, 1))
    stopifnot(is(fixed, "logical"))
    
    posSum = sum(value[sign == 1])
    negSum = sum(value[sign == -1])
    ## How much is adjustable (downwards)?
    posAdj = sum(value[sign == 1 & !fixed] - lowerBound[sign == 1 & !fixed])
    negAdj = sum(value[sign == -1 & !fixed] - lowerBound[sign == -1 & !fixed])
    delta = posSum - negSum
    ## Start with the passed value as the output, and adjust from here
    output = value
    ## First two conditions will adjust up the negative/positive elements if
    ## that is possible.  This will always be ok as long as there is a
    ## negative/positive element that is not fixed.
    if(delta == 0){
        return(output)
    } else if(delta > 0 & any(sign == -1 & !fixed)){
        output[sign == -1 & !fixed][1] = delta +
            output[sign == -1 & !fixed][1]
    } else if(delta < 0 & any(sign == 1 & !fixed)){
        output[sign == 1 & !fixed][1] = -delta +
            output[sign == 1 & !fixed][1]
    ## The next two conditions are trickier: we have to adjust elements
    ## down, but we must also ensure they don't go below their lower bounds.
    } else if(delta > 0 & posAdj > delta){
        ## How much can we adjust each element by?
        posAdjByElement = (value - lowerBound) * (sign == 1 & !fixed)
        ## Iteratively allocate the difference to adjustable elements
        i = 1
        while(delta > 0){
            currentAdj = min(delta, posAdjByElement[i])
            output[i] = output[i] - currentAdj
            delta = delta - currentAdj
            i = i + 1
        }
    } else if(delta < 0 & negAdj > delta){
        ## How much can we adjust each element by?
        negAdjByElement = (value - lowerBound) * (sign == -1 & !fixed)
        ## Iteratively allocate the difference to adjustable elements
        i = 1
        while(delta < 0){
            currentAdj = min(abs(delta), negAdjByElement[i])
            output[i] = output[i] - currentAdj
            delta = delta + currentAdj
            i = i + 1
        }
    } else {
        ## Must raise an error here, as not satisfying the constraints will
        ## kill our optimizer.
        stop("Cannot force initial constraint to be satisfied.")
    }
    
    output
}