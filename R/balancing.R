##' Balancing algorithm via Maximum Likelihood
##'
##' This function balances the data using maximum likelihood.
##'
##' @param param1 A vector of the first parameter for each of the elements. For
##'   a normal distribution, this is the mean.
##' @param param2 A vector of the second parameter for each of the elements. For
##'   a normal distribution, this is the standard deviation.
##' @param dist A vector of the name of the distribution for each distribution.
##'   Currently, only "Normal" is implemented. Working in progress for different
##'   distribution
##' @param sign A vector of the sign of each element. These values should all be
##'   +1 or -1, and they indicate if Delta_1, Delta_2, ... should be
##'   pre-multiplied by a negative or not. Usually, these will all be +1.
##' @param optimize A string with the method of optimization of the Maximum
##'   Likelihood, default solnp (Rsolnp dependency). Must be one of "solnp",
##'   "L-BFGS-B" (uses optim and is NOT recommended, as it hasn't been tested
##'   thoroughly and doesn't enforce all constraints), or "constrOptim" (using
##'   constrOptim from base package).
##' @param lbounds A Vector of the lower bounds for each element. These values
##'   should all be numeric
##' @param ubounds A Vector of the upper bounds for each element. These values
##'   should all be numeric
##' @param forceInitialConstraint Should the initial parameter vector be forced
##'   to satisfy the constraints?  If TRUE, the initial value for one element
##'   will be adjusted in an attempt to satisfy the constraints, but this may
##'   not be possible. If not, a warning is given and optimization proceeds with
##'   the provided starting value.
##' @param tol The level of tolerance of the balancing (numeric). By default is
##'   set up to 1e-5. Currently only used if all elements are fixed, and in this
##'   case it provides the numerical tolerance requried to check if the balance
##'   is satisfied.
##' @param constrTol The tolerance to pass to the constrOptim algorithm. This
##'   algorithm does not force that the equation is balanced but rather forces
##'   that the equation is within +/- constrTol % of being balanced. For
##'   example, if constrTol = 0.0001, then a tolerable error of .01% is allowed
##'   in the balance. This should be set very small as later any lack of balance
##'   is assigned to food, and such assignment could create negative values.
##' @param plot Logical. Should a plot be generated which shows the result of
##'   the balancing?
##' @param plotSigma A plotting parameter. See ?plotBalancing.
##'
##' @return A vector of the final balanced values
##'
##' @export
##' 

balancing = function(param1, param2, sign, dist = rep("Normal", length(param1)),
                     optimize = "solnp", lbounds = rep(0, length(param1)),
                     ubounds = rep(Inf, length(param1)),
                     forceInitialConstraint = TRUE, tol = 1e-5,
                     constrTol = 1e-6, plot = FALSE,
                     plotSigma = 3){
  ## Input Checks
  N = length(param1)
  #stopifnot(length(param1) == 1)
  stopifnot(length(param2) == N)
  stopifnot(length(dist) == N)
  stopifnot(length(sign) == N) 
  # This has to removed as soon as other distribution are available
  stopifnot(dist %in% "Normal")
  stopifnot(sign %in% c(-1, 1,0))
  stopifnot(optimize %in% c("solnp", "L-BFGS-B", "constrOptim"))
  #if(any(dist == "Normal" & param2 < 1)){
  #  param2[dist == "Normal" & param2 < 1] = 1
  #  warning("Some standard deviations (for a normal distribution) were ",
  #          "small (<1) and could cause a problem for the optimization. ",
  #          "These were adjusted up to 1.")
  #}
  
  ## value should be of length = length(param1)-1 so that the final element
  ## can be computed.
  fixedIndex = (dist == "Normal" & param2 == 0) # Normal: sd=0 => Fixed
  howManyFixed = length(which(fixedIndex))
  ## We have 3 different condition
  cond = 3 ## Two or more not fixed elements
  if(howManyFixed == N) cond = 1 ## All fixed elements
  if(howManyFixed == N-1) cond = 2 ## One not fixed element
  switch(cond, `1` = {
    ## I had to round it, otherwise it didn't work, the sum was 0.469 for test1
    if (abs(sum(round(param1*sign))) < tol){
    #if (abs(sum(param1*sign)) < tol){
      output = param1
      #return(output) 
    } else {
      stop(paste0("All elements of the balancing cannot be fixed and not balanced, 
            the sum is ", sum(round(param1*sign))))
    }
  }, `2` = {
    output = param1
    output[fixedIndex] = param1[fixedIndex]
    output[!fixedIndex] = sum(output[fixedIndex]*sign[fixedIndex])
  }, `3` = {
    ## If all distributions are normal, use the balancingNormal function which
    ## analytically finds the solution.
    if(all(dist == "Normal")){
      output = balancingProportional(param1 = param1, param2 = param2, sign = sign,
                               lbounds = lbounds, ubounds = ubounds)
    } else {
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
        output = optimizedResult$par
        residual = -sum(output * sign[-N])
        output = c(output, residual * sign[N])
      }, "solnp" = {
        ## Function to Optimize
        ## 
        ## @param value A vector of the non-fixed values to optimize.
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
        scaleFactor = max(abs(param1))
        param1 = param1 / scaleFactor
        param2 = param2 / scaleFactor
        lbounds = lbounds / scaleFactor
        ubounds = ubounds / scaleFactor
      
        initial = param1
        if(forceInitialConstraint){
          initial = forceBalance(value = initial, sign = sign,
                                 fixed = fixedIndex, lowerBound = lbounds)
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
      },
      "constrOptim" = {
        ## Function to Optimize
        ## 
        ## @param value A vector of the non-fixed values to optimize.
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
        
#         ## Gradient of Optimization
#         ## 
#         ## @param value A vector of the non-fixed values to optimize.
#         ## @return This function returns the gradient of the optimization
#         ##   function with respect to the input parameters.
#         Gradient = function(value){
#               ## Scale/center the value
#               value = (value - param1[!fixedIndex])/param2[!fixedIndex]
#               ## Use the fact that if phi(x) = standard normal distribution, then
#               ## phi'(x) = x phi(x)
#               gradDensity = ifelse(dist[!fixedIndex] == "Normal",
#                                  value * dnorm(value,
#                                        mean = 0, sd = 1), # scaled already
#                                  NA)
#               #return(-sum(densities[!is.infinite(densities)]))
#               return(gradDensity)
#         }
      
        ## Scale parameters
        scaleFactor = max(abs(param1))
        param1 = param1 / scaleFactor
        param2 = param2 / scaleFactor
        lbounds = lbounds / scaleFactor
        ## We need finite lower bounds. Since the largest value is currently 1, a
        ## lower bound of -10000 should always be large enough for all
        ## optimizations.
        lbounds[lbounds == -Inf] = -10000
        if(any(ubounds < Inf)){
          warning("Current optimization ignores upper bound!")
        }
      
        initial = param1
        if(forceInitialConstraint){
          initial = forceBalance(value = initial, sign = sign,
                                 fixed = fixedIndex, lowerBound = lbounds,
                                 standardError = param2)
          ## Force values with a lower bound of 0 to be slightly above 0
          initial[!fixedIndex & lbounds == 0] =
            pmax(.Machine$double.eps, initial[!fixedIndex & lbounds == 0])
        }
      
        ## According to constrOptim documentation, the constraints must be given 
        ## by a matrix ui and ci. It will then be enforced that ui %*% theta - ci
        ## >= 0. We must ensure the equation balances, i.e. the sum of the fixed 
        ## and non-fixed elements, multiplied by their sign, is 0. To impose 
        ## this, we use two constraints:
        ## 
        ## 1. The sum of the fixed elements (with their signs) must be >= the sum 
        ## of the non-fixed elements (with their signs) minus a small number 
        ## (arbitrarily choosen to be 0.0001).
        ## 
        ## 2. The negative sum of the fixed elements (with their signs) must be >=
        ## the negative sum of the non-fixed elements (with their signs) minus a 
        ## small number (arbitrarily choosen to be 0.0001). With 1 and 2, we 
        ## enforce that the imbalance will be very small.
        ## 
        ## 3. Lastly, we also constrain every element individually by it's lower
        ## bound. This amounts to using an identity matrix to require each
        ## element be greater than lbounds.
        ##
        optimizedResult = constrOptim(theta = initial[!fixedIndex],
                                      f = functionToOptimize,
                                      grad = NULL,
                                      ui = rbind(sign[!fixedIndex], -sign[!fixedIndex],
                                                 diag(1, sum(!fixedIndex))),
                                      ci = c(sum(-sign[fixedIndex] * initial[fixedIndex]) - constrTol,
                                             sum(sign[fixedIndex] * initial[fixedIndex]) - constrTol,
                                             lbounds[!fixedIndex]),
                                      control = list(maxit = 10000)
        )
      
        output = param1 * scaleFactor
        output[!fixedIndex] = optimizedResult$par * scaleFactor
        #prob = rep(1,N)
        #prob[!fixedIndex] = getProbability(output[!fixedIndex],
        #                                   param1[!fixedIndex] * scaleFactor,
        #                                   param2[!fixedIndex] * scaleFactor) 
        #final = list(output,prob)
      })
    }
  })
  if(plot){
    plotBalancing(param1 = param1, param2 = param2, final = output,
                  plotSigma = plotSigma)
  }
  
  return(output)
}