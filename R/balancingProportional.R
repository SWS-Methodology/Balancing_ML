##' Balancing algorithm via Maximum Likelihood with all Normal Distributions
##' 
##' This function has more or less the same inputs as balancing(), with the 
##' exception of a handful of the tuning options that don't apply.  If all 
##' distributions are normal and unbounded, there is a simple analytical 
##' solution to the balancing equation.  Even if we have bounds, we can 
##' iteratively find the optimal solution by 1. optimizing analytically and 
##' ignoring bounds, 2. adjust elements that fall outside of their bounds, 3. 
##' repeat.  We will have at most as many iterations as elements and thus it 
##' will be very fast, and we are also guaranteed to find the true optimum.
##' 
##' Minimizing the negative log-likelihood of the sum of the normal densities is
##' equivalent to minimizing the weighted sums of squares (where the weights are
##' 1/sigma_i^2).  Thus, the analytical solution is to just proportion the
##' difference according to those weights (sigma_i^2).
##' 
##' @param param1 A vector of the first parameter for each of the elements.  For
##'   a normal distribution, this is the mean.
##' @param param2 A vector of the second parameter for each of the elements. For
##'   a normal distribution, this is the standard deviation.
##' @param sign A vector of the sign of each element.  These values should all 
##'   be +1 or -1, and they indicate if Delta_1, Delta_2, ... should be 
##'   pre-multiplied by a negative or not.  Usually, these will all be +1.
##' @param lbounds A vector of the lower bounds for each element. These values 
##'   should all be numeric
##' @param ubounds A vector of the upper bounds for each element. These values 
##'   should all be numeric
##' @param failed Logic value indicating if the balancing occurs after the failure of some cases
##' when imbalance>0
##' @return A vector of the final balanced values
##'   
##' @examples
##' 
##' # Production, Imports, Exports, Stock, Food, Processing, Feed, Waste,
##' # Seed, Industrial, Tourist
##' param1 = c(54418808, 1999076, 32789894, -230630, 0, 26331060, 4898000,
##'            560306, 1904246, 0, 67)
##' param2 = c(544188, 0, 0, 89854, 0, 1749, 244900, 56031, 1129, 0, 7)
##' sign = c(1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1)
##' lbounds = c(0, 0, 0, -Inf, 0, 0, 0, 0, 0, 0, 0)
##' ubounds = rep(Inf, 11)
##' 

balancingProportional = function(param1, param2, sign,
                           lbounds = rep(0, length(param1)),
                           ubounds = rep(Inf, length(param1)),
                           failed = FALSE){
  ## Data quality checks
  ## (None implemented as this function is a helper function and should not be
  ## called directly (but rather called from balancing()).
  
  imbalance = param1 %*% sign
  
  signSupply= sign
  signSupply[signSupply<0]=0
  
  supply= param1 %*% signSupply
  
  maxResidual=supply*0.05

  solution = param1
  
  if(imbalance>0&!failed)
  {

    signResidual=sign
    signResidual[signResidual==0]=100
    signResidual[signResidual<100]=0
    signResidual[signResidual==100]=1
    
    if(imbalance<maxResidual){
      solution= solution + imbalance *(signResidual)
      imbalance=0
      
    }else{
    
      solution= solution+ maxResidual *(signResidual) 
      imbalance= imbalance-maxResidual
    }
}
  
  if(all(param2 == 0) & imbalance != 0){
      stop("Balance was not possible because of constraints.")
    }
  
  if(any(is.infinite(param1)) | any(is.infinite(param2))){
    stop("Balance was not possible because presence of infinite values in means or param1 or param2")
  }
  ## Initialize solution

  
  weight =  abs(param2)
  if(all(weight==0)){
    
    weight=weight
  } else {weight = weight / sum(weight)}
  
  solution = solution + imbalance * weight * (-sign)
  failedCases = solution < lbounds | solution > ubounds
  if(any(failedCases)){
    ## Assign to boundary if it crossed boundary
    solution[solution < lbounds] = lbounds[solution < lbounds]
    solution[solution > ubounds] = ubounds[solution > ubounds]
    ## Resolve (i.e. reallocate new residual to elements that did not
    ## violate bounds).
    solution = balancingProportional(param1 = solution,
                               param2 = ifelse(failedCases, 0, param2),
                               sign = sign, lbounds = lbounds,
                               ubounds = ubounds,failed = TRUE)
  }
  return(solution)
}
