library(faosws)
library(data.table)

DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    cat("Not on server, so setting up environment...\n")
    
    ## Define directories
    if(Sys.info()[7] == "josh")
        apiDirectory = "~/Documents/Github/faoswsBalancing/R"

    ## Get SWS Parameters
    GetTestEnvironment(
        ## baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        ## token = "e77abee7-9b0d-4557-8c6f-8968872ba7ca"
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "c585c410-fb9e-44ea-ba36-ef940d32185d"
    )

    ## Source local scripts for this local test
    for(file in dir(apiDirectory, full.names = T))
        source(file)
}

d = pullHistoricalData(1991:2011)
save(d, file = "~/Documents/Github/faoswsBalancing/Data/flagEstimation.RData")
generateImputations(d)
save(d, file = "~/Documents/Github/faoswsBalancing/Data/flagEstimation.RData")