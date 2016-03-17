##' Pull Historical FBS Data
##' 
##' This function extracts historical data from the SWS for the specified years.
##' It converts all the specific variable codes (i.e. production can have 5510, 
##' 5511, 5512, etc. based on units, and it converts these to 51).  The dataset
##' is then ready for analysis to compute flag variabilities.
##' 
##' @param years A numeric vector containing the years for the analysis.
##'   
##' @return A data.table containing the full dataset.
##' 
##' @export
##' 
##' @import faoswsUtil
##' 

pullHistoricalData = function(years = 1991:2011){
    ## Data Quality Checks
    stopifnot(is.numeric(years))
    stopifnot(min(years) >= 1960)
    stopifnot(max(years) <= 2050)
    
    ## Set up the dimensions for the key
    elementKeys = GetCodeTree("agriculture", "aproduction", "measuredElement")
    elementKeys = unique(elementKeys) # bug in code tree, get two 151 parents
    ## Add in elements without parents: food (5141), tourist (5164), industrial
    ## (5165), residual (5166)
    elementKeys = rbind(elementKeys, data.table(parent = c("5141", "5164", "5165", "5166"),
                                                children = c("5141", "5164", "5165", "5166")))
    elementKeys = elementKeys[parent %in% c(51, 71, 101, 111, 121, 131, 151,
                                            5141, 5164, 5165, 5166), ]
    fbsKeys = sapply(elementKeys[, children],
                     function(x){
                         strsplit(x, split = ", ")
                     })
    fbsKeys = do.call("c", fbsKeys)
    fbsKeys = unique(fbsKeys)
    
    ## Create the dataset key and pull the data
    key = DatasetKey(domain = "agriculture", dataset = "aproduction", dimensions = list(
        geographicAreaM49 = Dimension(name = "geographicAreaM49",
            key = GetCodeList("agriculture", "aproduction", "geographicAreaM49")[, code]),
        measuredElement = Dimension(name = "measuredElement", key = fbsKeys),
        measuredItemCPC = Dimension(name = "measuredItemCPC", 
            key = GetCodeList("agriculture", "aproduction", "measuredItemCPC")[, code]),
        timePointYears = Dimension(name = "timePointYears", key = as.character(years))
    ))
    d = GetData(key)
    
    ## Correct indigenous and biological figures
    d[nchar(measuredElement) > 4, c("measuredElement", "measuredItemCPC") :=
          list(substr(measuredElement, 1, 4),
               ifelse(substr(measuredElement, 5, 5) == 0,
                      paste0(measuredItemCPC, "i"),
                      paste0(measuredItemCPC, "b")))]
    
    ## Map all keys to main groups (5510, 5511, 5512, ... to 51, etc.)
    map = faoswsUtil::adjacent2edge(elementKeys)
    setnames(map, "children", "measuredElement")
    d = merge(d, map, by = "measuredElement", all.x = TRUE)
    d[, measuredElement := parent]
    d[, parent := NULL]
    
    ## Remove missing values, as they're useless for this analysis
    d = d[flagObservationStatus != "M", ]
    
    ## If we have multiple records for unique country/element/item/year, then
    ## (arbitrarily) de-duplicate by first choosing the one with the best flag
    ## and otherwise the first.
    keyCols = c("measuredElement", "geographicAreaM49",
                "measuredItemCPC", "timePointYears")
    uniquePts = d[, .N, by = keyCols]
    if(any(uniquePts$N > 1)){
        warning("Multiple records for one unique country/commodity/variable/",
                "year!  Arbitrarily picking one observation.")
        uniquePts = uniquePts[N > 1, ]
        # Default order of characters is "", "-", "E", "I", "T" and this is
        # conveniently exactly what we want!
        d = d[order(flagObservationStatus), ]
        d[, uniqueIndex := 1:nrow(.SD), by = keyCols]
        d = d[uniqueIndex == 1, ]
        d[, uniqueIndex := NULL]
    }
    
    ## Remove bad flags:
    d = d[flagObservationStatus != "-", ]
    
    d
}