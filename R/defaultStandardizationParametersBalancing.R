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
    production = list(
      Code = "5510",
      Sign = "+",
      Dist = "Normal",
      LB = 0,
      UB = Inf
    ),
    import = list(
      Code = "5421",
      Sign = "+",
      Dist = "Normal",
      LB = 0,
      UB = Inf
    ),
    export = list(
      Code = "5900",
      Sign = "-",
      Dist = "Normal",
      LB = 0,
      UB = Inf
    ),
    stock = list(
      Code = "71",
      Sign = "+",
      Dist = "Normal",
      LB = -Inf,
      UB = Inf
    ),
    food = list(
      Code = "5141",
      Sign = "-",
      Dist = "Normal",
      LB = 0,
      UB = Inf
    ),
    feed = list(
      Code = "5520",
      Sign = "-",
      Dist = "Normal",
      LB = 0,
      UB = Inf
    ),
    waste = list(
      Code = "5120",
      Sign = "-",
      Dist = "Normal",
      LB = 0,
      UB = Inf
    ),
    seed = list(
      Code = "5525",
      Sign = "-",
      Dist = "Normal",
      LB = 0,
      UB = Inf
    ),
    industrial = list(
      Code = "",
      Sign = "-",
      Dist = "Normal",
      LB = 0,
      UB = Inf
    ),
    tourist = list(
      Code = "",
      Sign = "-",
      Dist = "Normal",
      LB = 0,
      UB = Inf
    ),
    residual = list(
      Code = "",
      Sign = "-",
      Dist = "Normal",
      LB = -Inf,
      UB = Inf
    )
  )
}