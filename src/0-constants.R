## stsimcbmcfs3 - constants
## ApexRMS, January 2022

# Run with R-4.1.1
# This script is used to define constants across scripts

# crosswalk function ---

# Crosswalk function
crossSF <- function(CBMStock){ as.character(crosswalkStock$StockTypeID[crosswalkStock$CBMStock==CBMStock])}

# Constants ----------

## CBM simulation stocks ## - names taken directly from column headers of CBM simulation outputs

CBM_Stocks_HW <- c("Aboveground Very Fast DOM", "Aboveground Fast DOM", "Medium DOM", "Aboveground Slow DOM",
                      "Belowground Very Fast DOM", "Belowground Fast DOM", "Belowground Slow DOM",
                      "Hardwood Branch Snag", "Hardwood Stem Snag",
                      "Hardwood Merchantable", "Hardwood Foliage", "Hardwood Other", "Hardwood Coarse Roots", "Hardwood Fine Roots")

CBM_Stocks_SW <- c("Aboveground Very Fast DOM", "Aboveground Fast DOM", "Medium DOM", "Aboveground Slow DOM",
                      "Belowground Very Fast DOM", "Belowground Fast DOM", "Belowground Slow DOM",
                      "Softwood Branch Snag", "Softwood Stem Snag",
                      "Softwood Merchantable", "Softwood Foliage", "Softwood Other", "Softwood Coarse Roots", "Softwood Fine Roots")


## Biomass stocks ----

### simplified version of CBM biomass stocks

biomassStocks <- c("Merchantable", 
                   "Foliage", 
                   "Other", 
                   "Coarse Roots", 
                   "Fine Roots")

numBiomassStocks <- length(biomassStocks)

### ST-Sim biomass stocks

biomassStockTypes <- c("Biomass: Merchantable", 
                       "Biomass: Foliage", 
                       "Biomass: Other Wood",
                       "Biomass: Coarse Root",
                       "Biomass: Fine Root")

### biomass state attributes

biomassStateAtts <- c("Merchantable", "Foliage", "Other Wood", "Coarse Roots", "Fine Roots")
biomassStateAtts <- as.list(str_c("Carbon Initial Conditions: ", biomassStateAtts))


## DOM stocks ----

### simplified version from CBM 

DOMStocks <- c("Aboveground Very Fast DOM", 
               "Aboveground Fast DOM", 
               "Medium DOM", 
               "Aboveground Slow DOM",
               "Belowground Very Fast DOM", 
               "Belowground Fast DOM", 
               "Belowground Slow DOM",
               "Branch Snag", 
               "Stem Snag")

numDOMStocks <- length(DOMStocks)

DOMStocks_hw <- c(DOMStocks[1:7], "Hardwood Branch Snag", "Hardwood Stem Snag")
DOMStocks_sw <- c(DOMStocks[1:7],"Softwood Branch Snag", "Softwood Stem Snag")

### ST-Sim DOM stocks

DOMStockTypes <- c("DOM: Aboveground Very Fast", 
                   "DOM: Aboveground Fast", 
                   "DOM: Aboveground Medium",
                   "DOM: Aboveground Slow", 
                   "DOM: Belowground Very Fast",
                   "DOM: Belowground Fast",
                   "DOM: Belowground Slow",
                   "DOM: Snag Branch",
                   "DOM: Snag Stem")
                   # "DOM: Black Carbon")

### DOM state attributes

DOMStateAtts <- c("Aboveground Very Fast", "Aboveground Fast", "Aboveground Medium", "Aboveground Slow",
                  "Belowground Very Fast", "Belowground Fast", "Belowground Slow", "Snag Branch", "Snag Stem")
DOMStateAtts <- as.list(str_c("Carbon Initial Conditions: ", DOMStateAtts))





