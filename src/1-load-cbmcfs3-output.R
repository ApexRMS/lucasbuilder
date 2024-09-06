## stsimcbmcfs3 - load cbmcfs3 output
## ApexRMS, August 2024

# Run with R-4.1.1
# This script loads in crosswalks for CBM-CFS3 to ST-Sim species type and 
# carbon stock, and populates the state attributes datafeed. 
# CBM-CFS3 validation outputs are also generated. 

# source constants 
pkg_dir <- (Sys.getenv("ssim_package_directory"))
source(file.path(pkg_dir, "0-dependencies.R"))
source(file.path(pkg_dir, "0-constants.R"))

options(stringsAsFactors=FALSE)

# Get ST-Sim library, project and scenario ----
myLibrary <- ssimLibrary()
myProject <- project()
myScenario <- scenario() 

# load run control data - to get maxTimestep
sheetName <- "stsim_RunControl"
maxTimestep <- datasheet(myScenario, name = sheetName)$MaximumTimestep 

# Pull in crosswalk table
sheetName <- "lucasbuilder_CrosswalkSpecies"
crosswalkSUSTFull <- datasheet(myScenario, name = sheetName, optional = T)

# State Attribute Values ----

# Connect to CBM-CFS3 "ArchiveIndex_Beta_Install.mdb" to assess if the species is Softwood or Hardwood
CBMDatabasePath <- datasheet(myLibrary, name = "lucasbuilder_Database")
CBMdatabase <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", CBMDatabasePath))
speciesTypeTable <- sqlFetch(CBMdatabase, "tblSpeciesTypeDefault")
forestTypeTable <- sqlFetch(CBMdatabase, "tblForestTypeDefault")
close(CBMdatabase)


## State Attributes for Living Biomass

sheetName <- "stsim_StateAttributeValue"
stateAttributeInitialCarbonBiomass <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
stateAttributeInitialCarbonBiomassFull = data.frame()

for(i in seq(1:nrow(crosswalkSUSTFull))) {
  
  crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
  CBMSimulationData <- read.csv(crosswalkSUST$CBMOutputFile, header=TRUE, check.names = F)
  CBMSimulationData <- CBMSimulationData[,1:(ncol(CBMSimulationData)-1)]
  
  # Remove time steps above run control maxTimestep
  CBMSimulationData <- CBMSimulationData[CBMSimulationData$`Time Step` <= maxTimestep,]
  
  # Get Species and Forest Type Ids
  speciesTypeId <- speciesTypeTable$SpeciesTypeId[speciesTypeTable$SpeciesTypeName == as.character(crosswalkSUST$SpeciesTypeId)]
  forestTypeId <- speciesTypeTable$ForestTypeId[speciesTypeTable$SpeciesTypeId == speciesTypeId]
  forestType <- as.character(forestTypeTable$ForestTypeName[forestTypeTable$ForestTypeId == forestTypeId])
  
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "StratumId"] <- crosswalkSUST$StratumId            
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "SecondaryStratumId"] <- crosswalkSUST$SecondaryStratumId
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "StateClassId"] <- crosswalkSUST$StateClassId
  stateAtts <- NULL
  for(r in biomassStateAtts){ stateAtts <- c(stateAtts, rep(r,nrow(CBMSimulationData)))}
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "StateAttributeTypeId"] <- stateAtts
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "AgeMin"] <- rep(CBMSimulationData[,"Time Step"], numBiomassStocks)
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "AgeMax"] <- rep(CBMSimulationData[,"Time Step"], numBiomassStocks)
  stateAttributeInitialCarbonBiomass[stateAttributeInitialCarbonBiomass$AgeMin == (nrow(CBMSimulationData)-1), "AgeMax"] <- NA
  value <- NULL
  for(r in biomassStocks){ value <- c(value, CBMSimulationData[,paste(forestType, r)])}
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "Value"] <- value
  stateAttributeInitialCarbonBiomassFull = rbind(stateAttributeInitialCarbonBiomass, stateAttributeInitialCarbonBiomassFull)
}

## State Attributes for DOM
stateAttributeInitialCarbonDOM <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
stateAttributeInitialCarbonDOMFull = data.frame()

for(i in seq(1:nrow(crosswalkSUSTFull))) {
  
  crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
  CBMSimulationData <- read.csv(crosswalkSUST$CBMOutputFile, header=TRUE, check.names = F)
  CBMSimulationData <- CBMSimulationData[,1:(ncol(CBMSimulationData)-1)]
  # Remove time steps above run control maxTimestep
  CBMSimulationData <- CBMSimulationData[CBMSimulationData$`Time Step` <= maxTimestep,]
  
  # Get Species and Forest Type Ids
  speciesTypeId <- speciesTypeTable$SpeciesTypeId[speciesTypeTable$SpeciesTypeName == as.character(crosswalkSUST$SpeciesTypeId)]
  forestTypeId <- speciesTypeTable$ForestTypeId[speciesTypeTable$SpeciesTypeId == speciesTypeId]
  forestType <- as.character(forestTypeTable$ForestTypeName[forestTypeTable$ForestTypeId == forestTypeId])
  
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "StratumId"] <- crosswalkSUST$StratumId           
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "SecondaryStratumId"] <- crosswalkSUST$SecondaryStratumId
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "StateClassId"] <- crosswalkSUST$StateClassId
  stateAtts <- NULL
  for(r in DOMStateAtts){ stateAtts <- c(stateAtts, rep(r,nrow(CBMSimulationData)))}
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "StateAttributeTypeId"] <-  stateAtts
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "AgeMin"] <- rep(CBMSimulationData[, "Time Step"], numDOMStocks)
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "AgeMax"] <- rep(CBMSimulationData[, "Time Step"], numDOMStocks)
  stateAttributeInitialCarbonDOM[stateAttributeInitialCarbonDOM$AgeMin == (nrow(CBMSimulationData)-1), "AgeMax"] <- NA
  value <- NULL
  if(forestType == "Hardwood"){ for(r in DOMStocks_hw){ value <- c(value, CBMSimulationData[,r])} }
  if(forestType == "Softwood"){ for(r in DOMStocks_sw){ value <- c(value, CBMSimulationData[,r])} }
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "Value"] <- value
  stateAttributeInitialCarbonDOMFull = rbind(stateAttributeInitialCarbonDOM, stateAttributeInitialCarbonDOMFull)
}

## Combine the state attribute datasheets
stateAttributesMerged <- rbind(stateAttributeInitialCarbonBiomassFull, stateAttributeInitialCarbonDOMFull)
saveDatasheet(myScenario, stateAttributesMerged, sheetName)

#########################################################
# Generate Validation Scenario Stock Outputs (STSim SF) #
#########################################################

# load CBM carbon stock crosswalk
sheetName <- "lucasbuilder_CrosswalkStock"
crosswalkStock  <- datasheet(myScenario, name = sheetName, optional = T)


## loop over the rows in the crosswalkSUSTFull
Validation_OutputStock <- data.frame()
for (row in 1:nrow(crosswalkSUSTFull)) { # row = 1
 
  # Read in CBM-CFS3 Crosswalk for Spatial Unit and Species Type
  crosswalkSUST <- crosswalkSUSTFull %>% slice(row)
  
  # Get Species and Forest Type Ids
  speciesTypeId <- speciesTypeTable$SpeciesTypeId[speciesTypeTable$SpeciesTypeName == as.character(crosswalkSUST[1, "SpeciesTypeId"])]
  forestTypeId <- speciesTypeTable$ForestTypeId[speciesTypeTable$SpeciesTypeId == speciesTypeId]
   
  # Get Forest Type Name
  forestType <- as.character(forestTypeTable$ForestTypeName[forestTypeTable$ForestTypeId == forestTypeId])
  
  CBMSimulationData <- read.csv(crosswalkSUST$CBMOutputFile, header=TRUE, check.names = F)
  
  # Remove blank column that CBM-CFS exports by default
  validationDataWide <- CBMSimulationData[,1:(ncol(CBMSimulationData)-1)]
  
  # Remove time steps above run control maxTimestep
  validationDataWide <- validationDataWide[validationDataWide$`Time Step` <= maxTimestep,]
  
  # convert validation carbon data from wide to long format
  if(forestType == "Softwood"){ validationStocks <- CBM_Stocks_SW }
  if(forestType == "Hardwood"){ validationStocks <- CBM_Stocks_HW }
  validationDataWide <- cbind("Timestep"=validationDataWide[, "Time Step"], validationDataWide[, names(validationDataWide) %in% validationStocks])
  validationCarbon <- gather(validationDataWide, Name, Amount, names(validationDataWide)[2:ncol(validationDataWide)], factor_key = TRUE)
  validationCarbon$Name <- as.character(validationCarbon$Name)
  validationCarbon$Name <- unlist(lapply(validationCarbon$Name, crossSF))
  validationCarbon$Name <- paste(validationCarbon$Name, "[Type]")
  
  validationCarbon$Iteration <- 1
  if(is.na(crosswalkSUST$StratumId)){validationCarbon$StratumId <- "[Unspecified]" }else{ validationCarbon$StratumId <- crosswalkSUST$StratumId }
  validationCarbon$SecondaryStratumId <- crosswalkSUST$SecondaryStratumId
  validationCarbon$TertiaryStratumId <- crosswalkSUST$TertiaryStratumId
  validationCarbon$StateClassId <- crosswalkSUST$StateClassId
  names(validationCarbon)[which(names(validationCarbon) == "Name")] <- "StockGroupId"
  Validation_OutputStock <- rbind(Validation_OutputStock, validationCarbon)
  
}

# output validation carbon stocks to result scenario
SF_OutputStock <- datasheet(myScenario, name = "stsim_OutputStock") 
SF_OutputStock1 <- add_row(SF_OutputStock, Validation_OutputStock)

saveDatasheet(myScenario, SF_OutputStock1, name = "stsim_OutputStock") 

