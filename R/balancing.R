##' Balancing algorithm via Maximum Likelihood
##' 
##' Taken mainly from allocateDelta.R in Aupus
##' 
##' @param param1 A vector of the first parameter for each of the elements.  For
##'   a normal distribution, this is the mean.
##' @param param2 A vector of the second parameter for each of the elements. For
##'   a normal distribution, this is the standard deviation.
##' @param dist A vector of the name of the distribution for each distribution. 
##'   Currently, only "Normal" is implemented. Working in progress for different
##'   distribution
##' @param sign A vector of the sign of each element.  These values should all 
##'   be +1 or -1, and they indicate if Delta_1, Delta_2, ... should be 
##'   pre-multiplied by a negative or not.  Usually, these will all be +1.
##' @param optimize A string with the method of optimization of the Maximum 
##'   Likelihood, default solnp (Rsolnp dependency)
##' @param lbounds A Vector of the lower bounds for each element. These values 
##'   should all be numeric
##' @param ubounds A Vector of the upper bounds for each element. These values 
##'   should all be numeric
##' @param forceInitialConstraint Should the initial parameter vector be forced 
##'   to satisfy the constraints?  If TRUE, the initial value for one element 
##'   will be adjusted in an attempt to satisfy the constraints, but this may 
##'   not be possible.  If not, a warning is given and optimization proceeds
##'   with the provided starting value.
##'   
##' @return A vector of the final balanced values
##'   

balancing = function(param1, param2, sign, dist = rep("Normal", length(param1)),
                     optimize = "solnp", lbounds = rep(0, length(param1)),
                     ubounds = rep(Inf, length(param1)),
                     forceInitialConstraint = TRUE){
  ## Input Checks
  N = length(param1)
  #stopifnot(length(param1) == 1)
  stopifnot(length(param2) == N)
  stopifnot(length(dist) == N)
  stopifnot(length(sign) == N)
  # This has to removed as soon as other distribution are available
  stopifnot(dist %in% "Normal")
  stopifnot(sign %in% c(-1, 1))
  stopifnot(optimize %in% c("solnp", "L-BFGS-B"))
  #if(any(dist == "Normal" & param2 < 1)){
  #  param2[dist == "Normal" & param2 < 1] = 1
  #  warning("Some standard deviations (for a normal distribution) were ",
  #          "small (<1) and could cause a problem for the optimization.  ",
  #          "These were adjusted up to 1.")
  #}
  
  ## value should be of length = length(param1)-1 so that the final element
  ## can be computed.
  
  
  
  switch(optimize, "L-BFGS-B" = {
    functionToOptimize = function(value){
      ## We must have sum(value * sign) = 0 if all elements are included. 
      ## Thus, the difference in the first N-1 is sum(value[-last] * sign[-last])
      residual = sum(value * sign[-N])
      value = c(value, -residual*sign[N])
      densities = ifelse(dist == "Normal",
                         dnorm(value * sign, mean = param1,
                               sd = param2, log = TRUE),
                         NA)
      # Negative log-likelihood
      return(-sum(densities[!is.infinite(densities)]))
    }
    meanVec = param1[-N]
    optimizedResult = optim(par = meanVec, fn = functionToOptimize,
                            method = "L-BFGS-B", lower = lbounds)
    finalValues = optimizedResult$par
    residual = -sum(finalValues * sign[-N])
    finalValues = c(finalValues, residual * sign[N])
    return(finalValues)
  }, "solnp" = {
    fixedIndex = (dist == "Normal" & param2 == 0) # Normal: sd=0 => Fixed
    ##' Function to Optimize
    ##' 
    ##' @param value A vector of the non-fixed values to optimize.
    functionToOptimize = function(value){
      densities = ifelse(dist[!fixedIndex] == "Normal",
                         dnorm(value,
                               mean = param1[!fixedIndex],
                               sd = param2[!fixedIndex],
                               log = TRUE),
                         NA)
      #return(-sum(densities[!is.infinite(densities)]))
      return(-sum(densities))
    }
    constraint = function(value){
      sum(value * sign[!fixedIndex]) + sum(param1[fixedIndex] * sign[fixedIndex])
    }

    ## Scale parameters    
    scaleFactor = max(param1)
    param1 = param1 / scaleFactor
    param2 = param2 / scaleFactor

    initial = param1
    if(forceInitialConstraint){
      posSum = sum(param1[sign == 1])
      negSum = sum(param1[sign == -1])
      if(posSum > negSum & any(sign == -1 & !fixedIndex)){
        initial[sign == -1 & !fixedIndex][1] = posSum - negSum +
            initial[sign == -1 & !fixedIndex][1]
      } else if(negSum > posSum & any(sign == 1 & !fixedIndex)){
        initial[sign == 1 & !fixedIndex][1] = negSum - posSum +
            initial[sign == 1 & !fixedIndex][1]
      } else {
        warning("Cannot easily force initial constraint to be satisfied, so ",
                "initializing with default parameters.")
      }
    }
    
    optimizedResult = Rsolnp::solnp(pars = initial[!fixedIndex],
                  fun = functionToOptimize,
                  eqfun = constraint,
                  eqB = 0,
                  #LB = rep(0,length(param1))
                  control = list(tol = 1e-8),
                  LB = lbounds[!fixedIndex],
                  UB = ubounds[!fixedIndex]
                  )
    output = param1 * scaleFactor
    output[!fixedIndex] = optimizedResult$pars * scaleFactor
    return(output)
  })
}

