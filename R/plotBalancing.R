##' Plot Balancing
##' 
##' This function takes the results from the balancing code and plots the 
##' distributions.  Currently, it assumes all distributions are normal for 
##' simplicity.
##' 
##' @param param1 A vector of the "first parameter" (which, in the case of the 
##'   normal distribution, is the mean).
##' @param param2 A vector of the "second parameter" (in the case of the normal 
##'   distribution, the standard deviation).
##' @param final A vector of the final/optimized values.
##' @param plotSigma All plots have the same width (although presumably 
##'   different centers) so as to show differences in widths of distributions. 
##'   The width choosen for each plot is 2*plotSigma*max(param2).  Thus,
##'   plotSigma = 3 implies the width of the plotting window will be wide enough
##'   to show 3 standard deviations for the widest distribution.
##'   
##' @return No value is returned, but a plot is generated.
##'   

plotBalancing = function(param1, param2, final, plotSigma = 3){
    
    ## Generate the points in the distributions
    range = seq(-plotSigma, plotSigma, .01) * max(param2)
    computeDist = function(mu, sd){
        out = data.frame(x = mu + range)
        out$y = dnorm(out$x, mean = mu, sd = sd)
        return(out)
    }
    grid = mapply(computeDist, mu = param1, sd = param2, SIMPLIFY = FALSE)
    grid = lapply(1:length(grid), function(i){
        out = grid[[i]]
        out$index = i
        return(out)
    })
    grid = do.call("rbind", grid)
    
    ## Generate the data for the lines
    linesExpected = data.frame(x = param1, index = 1:length(param1))
    linesExpected = merge(linesExpected, data.frame(y = c(0, Inf)))
    linesFinal = data.frame(x = final, index = 1:length(final))
    linesFinal = merge(linesFinal, data.frame(y = c(0, Inf)))
    
    print(ggplot(grid, aes(x = x, y = y, color = factor(index))) +
        geom_line() + facet_wrap( ~ index, scale = "free_x") +
        guides(color = FALSE) +
        geom_line(data = linesExpected, aes(linetype = "Expected Value")) +
        geom_line(data = linesFinal, aes(linetype = "Adjusted Value")) +
        scale_linetype_manual(breaks = c("Adjusted Value", "Expected Value"),
                              values = c(1, 5)) +
        labs(x = "", y = "", linetype = ""))
}