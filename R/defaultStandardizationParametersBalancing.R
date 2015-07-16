##' Default Standardization Parameters
##' 
##' Provides an object which contains the standardization parameters.  This
##' allows for easy passing into functions.
##' 
##' @return A list with the standardization parameters.
##' 

defaultStandardizationParametersBalancing = function(){
  geoVar = "geographicAreaM49"
  yearVar = "timePointYears"
  itemVar = "measuredItemCPC"
  list(
    geoVar = geoVar,
    yearVar = yearVar,
    itemVar = itemVar,
    mergeKey = c(geoVar, yearVar, itemVar), # For merging with the main data
    groupID = "groupID",
    elementPrefix = "Value_measuredElement_",
    childVar = "childID",
    parentVar = "parentID",
    extractVar = "extractionRate",
    shareVar = "share",
    elements = list(
      "5510" = list(
        Who = "Production",
        Sign = +1,
        Dist = "Normal",
        LB = 0,
        UB = Inf
      ),
      "5421" = list(
        Who = "Imports",
        Sign = +1,
        Dist = "Normal",
        LB = 0,
        UB = Inf
      ),
      "5900" = list(
        Who = "Exports",
        Sign = -1,
        Dist = "Normal",
        LB = 0,
        UB = Inf
      ),
      "71" = list(
        Who = "StockChange",
        Sign = -1,
        Dist = "Normal",
        LB = -Inf,
        UB = Inf
      ),
      "5141" = list(
        Who = "Food",
        Sign = -1,
        Dist = "Normal",
        LB = 0,
        UB = Inf
      ),
      "5520" = list(
        Who = "Feed",
        Sign = -1,
        Dist = "Normal",
        LB = 0,
        UB = Inf
      ),
      "5120" = list(
        Who = "Waste",
        Sign = -1,
        Dist = "Normal",
        LB = 0,
        UB = Inf
      ),
      "5525" = list(
        Who = "Seed",
        Sign = -1,
        Dist = "Normal",
        LB = 0,
        UB = Inf
      ),
      "I???" = list(
        Who = "Industrial",
        Sign = -1,
        Dist = "Normal",
        LB = 0,
        UB = Inf
      ),
      "T???" = list(
        Who = "Tourist",
        Sign = -1,
        Dist = "Normal",
        LB = 0,
        UB = Inf
      ),
      "R???" = list(
        Who = "Residual",
        Sign = -1,
        Dist = "Normal",
        LB = -Inf,
        UB = Inf
      )
    )
  )
}