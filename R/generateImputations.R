##' Generate imputations
##' 
##' This function takes a data.table containing the historical FBS data and 
##' generates imputed values for each observation.  These imputed values can be 
##' treated in a cross-validation sense: the MSE for values can be considered a 
##' measure of the variability of a particular observation, and we could model 
##' this variable as a function of flag, year, commodity, country, and element.
##' 
##' @param d The data.table containing the FBS data.  This should be generated 
##'   by a call to pullHistoricalData().
##' 
##' @return No value is returned, but new column(s) is/are added to d with the
##'   imputed values.
##' 
##' @export
##' 
##' @import faoswsImputation
##' @import faoswsUtil
##' 

generateImputations = function(d){
    ## Data Quality Checks
    stopifnot(is.data.table(d))
    stopifnot(colnames(d) == c("measuredElement", "geographicAreaM49",
                               "measuredItemCPC", "timePointYears", "Value",
                               "flagObservationStatus", "flagMethod"))
    
    ## Set up the initial imputation parameters
    impParams = defaultImputationParameters(variable = 5421)
    impParams$newImputationColumn = "modelEstimate"
    impParams$imputationValueColumn = "Value"
    impParams$imputationFlagColumn = "flagObservationStatus"
    impParams$imputationMethodColumn = "flagMethod"
    impParams$byKey = c("geographicAreaM49", "measuredItemCPC", "measuredElement")

    ## Update models to use
    impParams$ensembleModels = impParams$ensembleModels[
        ! names(impParams$ensembleModels) %in%
            c("defaultMixedModel", "defaultExp", "defaultLogistic")]
    
    ## Generate the imputations
    cvGroup = makeCvGroup(data = d, imputationParameters = impParams)
    modelFits = computeLoocvFits(data = d, cvGroup = cvGroup,
                                 imputationParameters = impParams)
    for(i in 1:length(modelFits)){
        newColumn = names(modelFits)[i]
        d[, c(newColumn) := modelFits[[i]]]
    }
}
