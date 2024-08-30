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
  
  # Get Species and Forest Type IDs
  speciesTypeID <- speciesTypeTable$SpeciesTypeID[speciesTypeTable$SpeciesTypeName == as.character(crosswalkSUST$SpeciesTypeID)]
  forestTypeID <- speciesTypeTable$ForestTypeID[speciesTypeTable$SpeciesTypeID == speciesTypeID]
  forestType <- as.character(forestTypeTable$ForestTypeName[forestTypeTable$ForestTypeID == forestTypeID])
  
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "StratumID"] <- crosswalkSUST$StratumID            
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "SecondaryStratumID"] <- crosswalkSUST$SecondaryStratumID
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "StateClassID"] <- crosswalkSUST$StateClassID
  stateAtts <- NULL
  for(r in biomassStateAtts){ stateAtts <- c(stateAtts, rep(r,nrow(CBMSimulationData)))}
  stateAttributeInitialCarbonBiomass[1:(nrow(CBMSimulationData)*numBiomassStocks), "StateAttributeTypeID"] <- stateAtts
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
  
  # Get Species and Forest Type IDs
  speciesTypeID <- speciesTypeTable$SpeciesTypeID[speciesTypeTable$SpeciesTypeName == as.character(crosswalkSUST$SpeciesTypeID)]
  forestTypeID <- speciesTypeTable$ForestTypeID[speciesTypeTable$SpeciesTypeID == speciesTypeID]
  forestType <- as.character(forestTypeTable$ForestTypeName[forestTypeTable$ForestTypeID == forestTypeID])
  
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "StratumID"] <- crosswalkSUST$StratumID           
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "SecondaryStratumID"] <- crosswalkSUST$SecondaryStratumID
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "StateClassID"] <- crosswalkSUST$StateClassID
  stateAtts <- NULL
  for(r in DOMStateAtts){ stateAtts <- c(stateAtts, rep(r,nrow(CBMSimulationData)))}
  stateAttributeInitialCarbonDOM[1:(nrow(CBMSimulationData)*numDOMStocks), "StateAttributeTypeID"] <-  stateAtts
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
  
  # Get Species and Forest Type IDs
  speciesTypeID <- speciesTypeTable$SpeciesTypeID[speciesTypeTable$SpeciesTypeName == as.character(crosswalkSUST[1, "SpeciesTypeID"])]
  forestTypeID <- speciesTypeTable$ForestTypeID[speciesTypeTable$SpeciesTypeID == speciesTypeID]
   
  # Get Forest Type Name
  forestType <- as.character(forestTypeTable$ForestTypeName[forestTypeTable$ForestTypeID == forestTypeID])
  
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
  if(is.na(crosswalkSUST$StratumID)){validationCarbon$StratumID <- "[Unspecified]" }else{ validationCarbon$StratumID <- crosswalkSUST$StratumID }
  validationCarbon$SecondaryStratumID <- crosswalkSUST$SecondaryStratumID
  validationCarbon$TertiaryStratumID <- crosswalkSUST$TertiaryStratumID
  validationCarbon$StateClassID <- crosswalkSUST$StateClassID
  names(validationCarbon)[which(names(validationCarbon) == "Name")] <- "StockGroupID"
  Validation_OutputStock <- rbind(Validation_OutputStock, validationCarbon)
  
}

# output validation carbon stocks to result scenario
SF_OutputStock <- datasheet(myScenario, name = "stsim_OutputStock") 
SF_OutputStock1 <- add_row(SF_OutputStock, Validation_OutputStock)

saveDatasheet(myScenario, SF_OutputStock1, name = "stsim_OutputStock") 

