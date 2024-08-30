## stsimcbmcfs3 - flow pathways           
## ApexRMS, August 2024

# Run with R-4.1.1       
# Before running script make sure that Microsoft Access Database Engine is 
# installed so that you can connect to a MS Access db from R x64bit
# On Sept 27 2018: AccessDatabaseEngine_X64.exe installed from 
# https://www.microsoft.com/en-us/download/details.aspx?id=13255

# source constants and helper functions
pkg_dir <- (Sys.getenv("ssim_package_directory"))
source(file.path(pkg_dir, "0-dependencies.R"))
source(file.path(pkg_dir, "0-constants.R"))
source(file.path(pkg_dir, "0-helper-functions.R"))

# Get ST-Sim library, project and scenario
myLibrary <- ssimLibrary()
myProject <- project(myLibrary, 1)
myScenario <- scenario() # myScenario <- scenario(myLibrary,scenario = 15)

# Get disturbance flow pathways - needs further testing
doDisturbances = T

# Use CBM output to derive expansion factors?
useCBMAgeVsCarbonCurves=T
# useCBMAgeVsCarbonCurves=F


###################################
# Get CBM database and crosswalks #
###################################
CBMDatabase <- datasheet(myLibrary, "lucasbuilder_Database")[1,"Path"]
crosswalkStratumState <- datasheet(myScenario, "lucasbuilder_CrosswalkSpecies", 
                                   optional = T) # crosswalkStratumState <- datasheet(myLibrary, scenario = 11, "lucasbuilder_CrosswalkSpecies", optional = T)

# load flow type crosswalk for transitions
crossTFT <- read.csv(file.path(pkg_dir,"data/flow-type-crosswalk.csv"))

crosswalkStock <- datasheet(myScenario, "lucasbuilder_CrosswalkStock") # crosswalkStock <- datasheet(myLibrary, scenario = 8, "lucasbuilder_CrosswalkStock")

# SF Flow Pathways
flowPathways = datasheet(myScenario, name="stsim_FlowPathway", empty=F, optional=T) %>% # flowPathways = datasheet(myLibrary, scenario = 1, name="stsim_FlowPathway", empty=F, optional=T) %>% 
  mutate_if(is.factor, as.character)

# Identify growth, biomass transfer, emission, decay, and DOM transfer flows
growthFlows <- flowPathways[flowPathways$FromStockTypeID == crossSF("Atmosphere"),]
emissionFlows <- flowPathways[flowPathways$ToStockTypeID == crossSF("Atmosphere"),]
biomassTurnoverFlows <- flowPathways[(flowPathways$FromStockTypeID %in% biomassStockTypes & flowPathways$ToStockTypeID %in% DOMStockTypes),]
DOMTransferFlows <- distinct(rbind(flowPathways[flowPathways$FromStockTypeID==crossSF("Aboveground Slow DOM") & flowPathways$ToStockTypeID == crossSF("Belowground Slow DOM"),],
                                   flowPathways[flowPathways$FromStockTypeID==crossSF("Softwood Stem Snag") & flowPathways$ToStockTypeID == crossSF("Aboveground Medium DOM"),],
                                   flowPathways[flowPathways$FromStockTypeID==crossSF("Softwood Branch Snag") & flowPathways$ToStockTypeID == crossSF("Aboveground Fast DOM"),],
                                   flowPathways[flowPathways$FromStockTypeID==crossSF("Hardwood Stem Snag") & flowPathways$ToStockTypeID == crossSF("Aboveground Medium DOM"),],
                                   flowPathways[flowPathways$FromStockTypeID==crossSF("Hardwood Branch Snag") & flowPathways$ToStockTypeID == crossSF("Aboveground Fast DOM"),]))
decayFlows <- rbind(flowPathways[(flowPathways$FromStockTypeID %in% DOMStockTypes & flowPathways$ToStockTypeID %in% DOMStockTypes) & (!(flowPathways$FlowTypeID %in% DOMTransferFlows$FlowTypeID)),])

####################################################
# CBM parameters that were not in the CBM database #
####################################################
# DOM Pool ID - "SoilPoolID" taken from CBM User manual Appendix 4 (Kull et al. 2016) - Not found in CMB database
DOMPoolID <- data.frame(CBMStock=c("Aboveground Very Fast DOM", "Belowground Very Fast DOM", "Aboveground Fast DOM", "Belowground Fast DOM", 
                                   "Aboveground Medium DOM", "Aboveground Slow DOM", "Belowground Slow DOM", "Softwood Stem Snag", "Softwood Branch Snag",
                                   "Hardwood Stem Snag", "Hardwood Branch Snag", "Black Carbon", "Peat"), 
                        SoilPoolID=c(0:12))
crosswalkStock <- merge(crosswalkStock, DOMPoolID, all=T)

# Get biomass turnover Proportions (not found in CBM database), taken from Kurtz et al. 2009
proportionMerchantableToSnag <- 1
proportionFineRootsToAGVeryFast <- 0.5
proportionFineRootsToBGVeryFast <- 0.5
proportionCoarseRootsToAGFast <- 0.5
proportionCoarseRootsToBGFast <- 0.5

stateAttributesNetGrowthMaster = datasheet(myScenario, name="stsim_StateAttributeValue", empty = T, optional = T)
flowMultiplierMaster = datasheet(myScenario, name="stsim_FlowMultiplier", empty = T, optional = T)
grossMerchantableVolume = datasheet(myScenario, name = "lucasbuilder_MerchantableVolumeCurve")

crosswalkDisturbance = datasheet(myScenario, name = "lucasbuilder_CrosswalkDisturbance") # crosswalkDisturbance <- datasheet(myLibrary, scenario = 7,"lucasbuilder_CrosswalkDisturbance")

# Loop over all entries in crosswalkStratumState
# Set up variable to accumulate during the loop
pathways_all <- c()
final_pathways_df <- data.frame()

for(i in 1: nrow(crosswalkStratumState)){
  #i<-1
  ####################################
  # CBM parameters from CBM Database # 
  ####################################
  # Connect to CBM-CFS3 "ArchiveIndex_Beta_Install.mdb"
  CBMdatabase <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", CBMDatabase))
  
  # Get Admin Boundary ID
  adminBoundaryTable <- sqlFetch(CBMdatabase, "tblAdminBoundaryDefault")
  adminBoundaryID <- adminBoundaryTable$AdminBoundaryID[adminBoundaryTable$AdminBoundaryName == as.character(crosswalkStratumState$AdminBoundaryID[i])]
  
  # Get Ecological Boundary ID
  ecoBoundaryTable <- sqlFetch(CBMdatabase, "tblEcoBoundaryDefault")
  ecoBoundaryID <- ecoBoundaryTable$EcoBoundaryID[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])]
  
  # Get Species and Forest Type IDs
  speciesTypeTable <- sqlFetch(CBMdatabase, "tblSpeciesTypeDefault")
  speciesTypeID <- speciesTypeTable$SpeciesTypeID[speciesTypeTable$SpeciesTypeName == as.character(crosswalkStratumState$SpeciesTypeID[i])]
  forestTypeID <- speciesTypeTable$ForestTypeID[speciesTypeTable$SpeciesTypeID == speciesTypeID]
  
  # Get Forest Type Name
  forestTypeTable <- sqlFetch(CBMdatabase, "tblForestTypeDefault")
  ForestType <- as.character(forestTypeTable$ForestTypeName[forestTypeTable$ForestTypeID == forestTypeID])
  
  # Get Spatial Planning Unit ID (SPUID) from adminBoundary and ecoBoundary
  SPUTable <- sqlFetch(CBMdatabase, "tblSPUDefault")
  SPUID <- SPUTable$SPUID[SPUTable$AdminBoundaryID==adminBoundaryID & SPUTable$EcoBoundaryID==ecoBoundaryID]
  
  # Throw error if SPUID is empty
  if(is.na(SPUID) || is.null(SPUID) || length(SPUID) == 0){
    stop("SPUID is of length 0 or is NA or null")
  }
  
  # Get Stratums and stateclass IDs
  the_stratum <- crosswalkStratumState$StratumID[i]
  the_secondarystratum <- crosswalkStratumState$SecondaryStratumID[i]
  the_class <- crosswalkStratumState$StateClassID[i]
  
  # Get biomass expansion factors
  biomassExpansionTable <- sqlFetch(CBMdatabase,"tblBioTotalStemwoodForestTypeDefault")
  
  # Get biomass to carbon multipliers
  biomassComponentTable <- sqlFetch(CBMdatabase,"tblBiomassComponent")
  biomassToCarbonTable <- sqlFetch(CBMdatabase,"tblBiomassToCarbonDefault")
  biomassToCarbonTable <- merge.data.frame(biomassToCarbonTable, biomassComponentTable)
  
  # Decay multipliers
  # Temperature modifier parameters
  climateTable <- sqlFetch(CBMdatabase,"tblClimateDefault")
  # There are 2 reference years but they seem to have the same values, I'm arbitrarily choosing 1980
  climateRefYear <- 1980
  if (!is.na(crosswalkStratumState$AverageTemperature[i])){
    meanAnnualTemp <- crosswalkStratumState$AverageTemperature[i]
  } else {
    meanAnnualTemp <- climateTable[climateTable$DefaultSPUID==SPUID & climateTable$Year == climateRefYear, "MeanAnnualTemp"]
  }
  # Stand modifier parameters
  # Note that the maxDecayMult in CBM-CFS3 is 1 which makes the StandMod = 1
  # Do not calculate StandMod this round
  # From Kurz et al. 2009: "In CBM-CFS2 the default value for MaxDecayMult was two. In the CBM-CFS3 the value 
  # defaults to one because more recent studies that examined open canopy effects on decomposition indicated 
  # that decomposition rates are not always higher under open canopies and that decomposition rate responses 
  # may be ecosystem specific (Yanai et al., 2000)." 
  maxDecayMult <- ecoBoundaryTable[ecoBoundaryTable$EcoBoundaryID==ecoBoundaryID, "DecayMult"]
  
  # Get DOM parameters
  DOMParametersTable <- sqlFetch(CBMdatabase,"tblDOMParametersDefault")
  DOMParametersTable <- merge(crosswalkStock[crosswalkStock$StockTypeID %in% DOMStockTypes,], DOMParametersTable)
  DOMParametersTable$TempMod <- exp((meanAnnualTemp - DOMParametersTable$ReferenceTemp)*log(DOMParametersTable$Q10)*0.1)
  if(ForestType=="Softwood"){
    DOMParametersTable <- DOMParametersTable[DOMParametersTable$CBMStock != "Hardwood Stem Snag",]
    DOMParametersTable <- DOMParametersTable[DOMParametersTable$CBMStock != "Hardwood Branch Snag",]
  }
  if(ForestType=="Hardwood"){
    DOMParametersTable <- DOMParametersTable[DOMParametersTable$CBMStock != "Softwood Stem Snag",]
    DOMParametersTable <- DOMParametersTable[DOMParametersTable$CBMStock != "Softwood Branch Snag",]
  }
  DOMDecayTable <- merge(decayFlows, DOMParametersTable, by.x="FromStockTypeID", by.y="StockTypeID")
  DOMEmissionTable <- merge(emissionFlows, DOMParametersTable, by.x="FromStockTypeID", by.y="StockTypeID")
  DOMDecayTable$Multiplier <- (1 - DOMDecayTable$PropToAtmosphere) * DOMDecayTable$OrganicMatterDecayRate * DOMDecayTable$TempMod
  DOMEmissionTable$Multiplier <- DOMEmissionTable$PropToAtmosphere * DOMEmissionTable$OrganicMatterDecayRate * DOMEmissionTable$TempMod
  DOMTable <- rbind(DOMDecayTable, DOMEmissionTable)
  
  # Get DOM transfer rates
  DOMTransferTable <- sqlFetch(CBMdatabase, "tblSlowAGToBGTransferRate")
  transferRateSlowAGToBG <- signif(DOMTransferTable$SlowAGToBGTransferRate,6)
  if(ForestType=="Softwood"){
    transferRateStemSnagToDOM <- signif(ecoBoundaryTable$SoftwoodStemSnagToDOM[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
    transferRateBranchSnagToDOM <- signif(ecoBoundaryTable$SoftwoodBranchSnagToDOM[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
  }
  if(ForestType=="Hardwood"){
    transferRateStemSnagToDOM <- signif(ecoBoundaryTable$HardwoodStemSnagToDOM[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
    transferRateBranchSnagToDOM <- signif(ecoBoundaryTable$HardwoodBranchSnagToDOM[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
  }
  # Get biomass turnover rates
  speciesTurnoverRatesTable <- sqlFetch(CBMdatabase, "tblSpeciesTypeDefault")
  turnoverRates <- speciesTurnoverRatesTable[speciesTurnoverRatesTable$SpeciesTypeName==as.character(crosswalkStratumState$SpeciesTypeID[i]),]
  
  # Get the disturbance matrix information
  DMassociation = sqlFetch(CBMdatabase, "tblDMAssociationDefault")
  DMassociation = DMassociation[DMassociation$DefaultEcoBoundaryID == ecoBoundaryID,]
  
  disturbanceType = sqlFetch(CBMdatabase, "tblDisturbanceTypeDefault")
  disturbanceMatrix <- sqlFetch(CBMdatabase, "tblDM")
  
  dmValuesLookup = sqlFetch(CBMdatabase, "tblDMValuesLookup")
  
  sourceName <- sqlFetch(CBMdatabase, "tblSourceName") %>% rename("DMRow" = "Row")
  sinkName <- sqlFetch(CBMdatabase, "tblSinkName") %>% rename("DMColumn" = "Column")
  
  # Close the database (do not forget this, otherwise you lock access database from editing)
  close(CBMdatabase)
  
  # Disturbance Stuff -----------------------------------------------------------
  
  if ((doDisturbances == T) & (nrow(crosswalkDisturbance)>0)) {
    
    #Need to rename DMassociation DistTypeID column
    names(DMassociation)[1] = "DistTypeID"
    
    df = DMassociation %>%
      left_join(disturbanceType, by="DistTypeID") %>% 
      select(DMID,DistTypeID,DistTypeName) %>%
      left_join(disturbanceMatrix, by="DMID") %>% 
      select(DMID,DistTypeID,DistTypeName,DMStructureID) %>%
      left_join(dmValuesLookup, by="DMID") %>%
      left_join(sourceName, by=c("DMStructureID","DMRow")) %>% 
      rename("Source" = "Description") %>%
      left_join(sinkName, by=c("DMStructureID", "DMColumn")) %>% 
      rename("Sink" = "Description") %>%
      mutate(Source = as.character(Source), Sink = as.character(Sink)) %>% 
      filter(DistTypeName %in% crosswalkDisturbance$DisturbanceTypeID)
      
      # Fix typo in CBM database where "Hardwood Coarse Roots" is misspelled as "Hardwood Coarse roots"
      df[df == "Hardwood Coarse roots"] <- "Hardwood Coarse Roots"
    
    # discriminate between hardwood and softwood
    
    opposite <- ifelse(ForestType == "Softwood", "Hardwood", "Softwood")
    
    df_filtered <- df %>% 
      filter(!str_detect(Source, opposite))
    
    sources <- data.frame(CBMSource = unique(df_filtered$Source), 
                          FromStockID = "")
      
    sinks = data.frame(CBMSink = unique(df_filtered$Sink),
                       ToStockID = "")
    
    transitions = data.frame(DistTypeName = unique(df_filtered$DistTypeName),
                             TransitionTypeID = "")
    
    #d = data.frame(CBMStocks = df_filtered$Source)
    #d1 = data.frame(CBMStocks = df_filtered$Sink)
    #d2 = bind_rows(d,d1)
    #d3 = data.frame(CBMStocks = unique(d2$CBMStocks), LUCASStocks = "")
    
    temp_crosswalkStock = datasheet(myScenario, name = "lucasbuilder_CrosswalkStock") #  temp_crosswalkStock <- datasheet(myLibrary, scenario = 8, name ="lucasbuilder_CrosswalkStock")
    
    # temp_crosswalkStock[16,1] <- "Products"
    
    temp_crosswalkDisturbance = datasheet(myScenario, name = "lucasbuilder_CrosswalkDisturbance") %>% # temp_crosswalkDisturbance <- datasheet(myLibrary, scenario = 7, name ="lucasbuilder_CrosswalkDisturbance") %>%
      mutate_if(is.factor, as.character)
    
    temp_pathways_df = df_filtered %>% left_join(temp_crosswalkStock, by = c("Source" = "CBMStock")) %>% 
      rename("FromStockTypeID"="StockTypeID") %>%
      left_join(temp_crosswalkStock, by = c("Sink" = "CBMStock")) %>% 
      rename("ToStockTypeID"="StockTypeID") %>%
      select(DistTypeName, FromStockTypeID, ToStockTypeID, Proportion) %>%
      mutate_if(is.factor, as.character) %>%
      left_join(temp_crosswalkDisturbance, by = c("DistTypeName" = "DisturbanceTypeID")) %>%
      filter(!is.na(TransitionGroupID)) %>% 
      filter(!is.na(FromStockTypeID)) %>%
      #mutate(FromStockTypeID = ifelse(FromStockTypeID != ToStockTypeID, FromStockTypeID, NA)) %>%
      filter(!is.na(FromStockTypeID)) %>%
      rename("Multiplier" = "Proportion") %>%
      mutate(FlowTypeID = "", Multiplier = round(Multiplier, 4)) %>%
      select(FromStockTypeID, ToStockTypeID, TransitionGroupID, DistTypeName, FlowTypeID, Multiplier) 
    
    # head(temp_pathways_df)
    
    temp_pathways_df_clean <- temp_pathways_df %>% # mutate(FlowTypeID = pathways) %>% 
      mutate(FromStratumID = the_stratum, # ToStratumID = the_stratum, 
             FromSecondaryStratumID = the_secondarystratum, #ToSecondaryStratumID = the_secondarystratum,
             FromStateClassID = the_class#, ToStateClassID = the_class
      ) %>% 
      select(-DistTypeName) %>% 
      filter(FromStockTypeID != ToStockTypeID)
    
    for (r in 1:nrow(temp_pathways_df_clean)){ # r=1
      fStock <- temp_pathways_df_clean$FromStockTypeID[r]
      tstock <- temp_pathways_df_clean$ToStockTypeID[r]
      temp_pathways_df_clean$FlowTypeID[r] <- crossTFT$FlowType[which(crossTFT$FromStockType == fStock & crossTFT$ToStockType == tstock)]
    }
    
    pathways <- temp_pathways_df_clean$FlowTypeID
    pathways_all <- unique(c(pathways_all, pathways))
    
    final_pathways_df <- bind_rows(final_pathways_df, temp_pathways_df_clean)
    
    #write.csv(temp_pathways_df, file = "FlowPathways.csv")
    
  }
  
  # Get biomass turnover Proportions (not found in CBM database), taken from Kurtz et al. 2009
  if(ForestType == "Softwood") proportionFoliageToAGVeryFast <- 1
  if(ForestType == "Hardwood") proportionFoliageToAGVeryFast <- 1
  
  ###
  # TODO: put biomass expansion factor stuff in here...
  
  #####################################################
  # Biomass Turnover and DOM Decay and Transfer rates #
  #####################################################
  # DOM transfer rates
  DOMTransferFlows[DOMTransferFlows$FromStockTypeID==crossSF("Aboveground Slow DOM") & DOMTransferFlows$ToStockTypeID == crossSF("Belowground Slow DOM"), "Multiplier"] <- transferRateSlowAGToBG
  
  if(ForestType == "Softwood"){
    DOMTransferFlows[DOMTransferFlows$FromStockTypeID==crossSF("Softwood Stem Snag") & DOMTransferFlows$ToStockTypeID == crossSF("Aboveground Medium DOM"), "Multiplier"] <- transferRateStemSnagToDOM
    DOMTransferFlows[DOMTransferFlows$FromStockTypeID==crossSF("Softwood Branch Snag") & DOMTransferFlows$ToStockTypeID == crossSF("Aboveground Fast DOM"), "Multiplier"] <- transferRateBranchSnagToDOM
  }
  if(ForestType == "Hardwood"){
    DOMTransferFlows[DOMTransferFlows$FromStockTypeID==crossSF("Hardwood Stem Snag") & DOMTransferFlows$ToStockTypeID == crossSF("Aboveground Medium DOM"), "Multiplier"] <- transferRateStemSnagToDOM
    DOMTransferFlows[DOMTransferFlows$FromStockTypeID==crossSF("Hardwood Branch Snag") & DOMTransferFlows$ToStockTypeID == crossSF("Aboveground Fast DOM"), "Multiplier"] <- transferRateBranchSnagToDOM
  }
  
  DOMTable <- rbind(DOMTable[,names(DOMTransferFlows)], DOMTransferFlows)
  
  # Biomass turnover rates
  turnOverRateStemAnnual <- signif(ecoBoundaryTable$StemAnnualTurnOverRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
  turnOverRateFineRootsAGVeryFast <- signif(turnoverRates$FineRootTurnPropSlope, 6)
  turnOverRateFineRootsBGVeryFast <- signif(turnoverRates$FineRootTurnPropSlope, 6)
  turnOverRateCoarseRootsAGFast <- signif(turnoverRates$CoarseRootTurnProp, 6)
  turnOverRateCoarseRootsBGFast <- signif(turnoverRates$CoarseRootTurnProp, 6)
  if(ForestType == "Softwood"){
    turnOverRateBranch <- signif(ecoBoundaryTable$SoftwoodBranchTurnOverRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
    turnOverRateFoliage <- signif(ecoBoundaryTable$SoftwoodFoliageFallRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
  }
  if(ForestType == "Hardwood"){
    turnOverRateBranch <- signif(ecoBoundaryTable$HardwoodBranchTurnOverRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
    turnOverRateFoliage <- signif(ecoBoundaryTable$HardwoodFoliageFallRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryID[i])],6)
  }
  
  # Turnover proportions
  proportionOtherToBranchSnag <- signif(turnoverRates$BranchesToBranchSnag,6)
  proportionOtherToAGFast <- 1 - proportionOtherToBranchSnag
  
  biomassTurnoverTable <- biomassTurnoverFlows
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Merchantable")] <- turnOverRateStemAnnual * proportionMerchantableToSnag 
  if(ForestType == "Softwood") biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Other") & biomassTurnoverTable$ToStockTypeID == crossSF("Softwood Branch Snag")] <- turnOverRateBranch * proportionOtherToBranchSnag
  if(ForestType == "Hardwood") biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Other") & biomassTurnoverTable$ToStockTypeID == crossSF("Hardwood Branch Snag")] <- turnOverRateBranch * proportionOtherToBranchSnag
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Other") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Fast DOM")] <- turnOverRateBranch * proportionOtherToAGFast
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Foliage")] <- turnOverRateFoliage * proportionFoliageToAGVeryFast 
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Fine Roots") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Very Fast DOM")] <- turnOverRateFineRootsAGVeryFast * proportionFineRootsToAGVeryFast
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Fine Roots") & biomassTurnoverTable$ToStockTypeID == crossSF("Belowground Very Fast DOM")] <- turnOverRateFineRootsBGVeryFast * proportionFineRootsToBGVeryFast
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Coarse Roots") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Fast DOM")] <- turnOverRateCoarseRootsAGFast * proportionCoarseRootsToAGFast
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Coarse Roots") & biomassTurnoverTable$ToStockTypeID == crossSF("Belowground Fast DOM")] <- turnOverRateCoarseRootsBGFast * proportionCoarseRootsToBGFast
  
  ########################################################
  # Calculate net growth based on mass-balance equations #
  ########################################################
  
  
  # Original approach using CBM Output. Remove eventually...
  if(useCBMAgeVsCarbonCurves==T){
    stateAttributeValues <- datasheet(myScenario, "stsim_StateAttributeValue", empty=FALSE, optional=TRUE) #  stateAttributeValues <- datasheet(myLibrary, scenario = 12, "stsim_StateAttributeValue", empty=FALSE, optional=TRUE)
    stateAttributeValuesWide <- spread(stateAttributeValues, key="StateAttributeTypeID", value = "Value") %>%
      mutate_if(is.factor, as.character)
    carbonInitialConditions <- datasheet(myScenario, "stsim_InitialStockNonSpatial", empty=FALSE, optional=TRUE) # carbonInitialConditions <- datasheet(myLibrary, scenario = 5, "stsimsf_InitialStockNonSpatial", empty=FALSE, optional=TRUE)
 
    if(!is.na(crosswalkStratumState$StratumID[i]) & !is.na(crosswalkStratumState$SecondaryStratumID[i])){ 
      volumeToCarbon <- filter(stateAttributeValuesWide, StratumID == as.character(crosswalkStratumState$StratumID[i]) &  SecondaryStratumID == as.character(crosswalkStratumState$SecondaryStratumID[i]) & StateClassID == as.character(crosswalkStratumState$StateClassID[i])) }
    if(!is.na(crosswalkStratumState$StratumID[i]) & is.na(crosswalkStratumState$SecondaryStratumID[i])){ 
      volumeToCarbon <- filter(stateAttributeValuesWide, StratumID == as.character(crosswalkStratumState$StratumID[i]) & StateClassID == as.character(crosswalkStratumState$StateClassID[i])) }
    if(is.na(crosswalkStratumState$StratumID[i]) & is.na(crosswalkStratumState$SecondaryStratumID[i])){ 
     volumeToCarbon <- filter(stateAttributeValuesWide, StateClassID == crosswalkStratumState$StateClassID[i])}
    
    volumeToCarbon$c_m <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeID[carbonInitialConditions$StockTypeID == crossSF("Merchantable")])]
    volumeToCarbon$c_foliage <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeID[carbonInitialConditions$StockTypeID == crossSF("Foliage")])]
    volumeToCarbon$c_other <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeID[carbonInitialConditions$StockTypeID == crossSF("Other")])]
    volumeToCarbon$c_fineroots <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeID[carbonInitialConditions$StockTypeID == crossSF("Fine Roots")])]
    volumeToCarbon$c_coarseroots <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeID[carbonInitialConditions$StockTypeID == crossSF("Coarse Roots")])]

  }

  volumeToCarbon$c_m1 <- c(volumeToCarbon$c_m[2:nrow(volumeToCarbon)], NA)
  volumeToCarbon$c_foliage1 <- c(volumeToCarbon$c_foliage[2:nrow(volumeToCarbon)], NA)
  volumeToCarbon$c_other1 <- c(volumeToCarbon$c_other[2:nrow(volumeToCarbon)], NA)
  volumeToCarbon$c_fineroots1 <- c(volumeToCarbon$c_fineroots[2:nrow(volumeToCarbon)], NA)
  volumeToCarbon$c_coarseroots1 <- c(volumeToCarbon$c_coarseroots[2:nrow(volumeToCarbon)], NA)
  
  # Growth happens after biomass transfer
  if(ForestType == "Softwood") volumeToCarbon$g_m <- volumeToCarbon$c_m1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Merchantable") & biomassTurnoverTable$ToStockTypeID == crossSF("Softwood Stem Snag")]) * volumeToCarbon$c_m
  if(ForestType == "Hardwood") volumeToCarbon$g_m <- volumeToCarbon$c_m1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Merchantable") & biomassTurnoverTable$ToStockTypeID == crossSF("Hardwood Stem Snag")]) * volumeToCarbon$c_m
  volumeToCarbon$g_foliage <- volumeToCarbon$c_foliage1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Foliage") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Very Fast DOM")]) * volumeToCarbon$c_foliage
  if(ForestType == "Softwood") volumeToCarbon$g_other <- volumeToCarbon$c_other1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Other") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Fast DOM")] - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Other") & biomassTurnoverTable$ToStockTypeID == crossSF("Softwood Branch Snag")]) * volumeToCarbon$c_other
  if(ForestType == "Hardwood") volumeToCarbon$g_other <- volumeToCarbon$c_other1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Other") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Fast DOM")] - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Other") & biomassTurnoverTable$ToStockTypeID == crossSF("Hardwood Branch Snag")]) * volumeToCarbon$c_other
  volumeToCarbon$g_fineroots <- volumeToCarbon$c_fineroots1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Fine Roots") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Very Fast DOM")]  - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Fine Roots") & biomassTurnoverTable$ToStockTypeID == crossSF("Belowground Very Fast DOM")]) * volumeToCarbon$c_fineroots
  volumeToCarbon$g_coarseroots <- volumeToCarbon$c_coarseroots1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Coarse Roots") & biomassTurnoverTable$ToStockTypeID == crossSF("Aboveground Fast DOM")]  - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeID == crossSF("Coarse Roots") & biomassTurnoverTable$ToStockTypeID == crossSF("Belowground Fast DOM")]) * volumeToCarbon$c_coarseroots
  volumeToCarbon$g_all <- volumeToCarbon$g_m + volumeToCarbon$g_foliage + volumeToCarbon$g_other + volumeToCarbon$g_fineroots + volumeToCarbon$g_coarseroots
  volumeToCarbon <- volumeToCarbon[1:(nrow(volumeToCarbon)-1),]
  
  # Replace NaN values with 0's
  volumeToCarbon[is.nan.data.frame(volumeToCarbon)] <- 0
  volumeToCarbon <- volumeToCarbon %>% mutate_if(is.factor, as.character)
  volumeToCarbon <- cbind(volumeToCarbon[,c("StratumID", "SecondaryStratumID", "StateClassID")], do.call(data.frame,lapply(volumeToCarbon[,!(names(volumeToCarbon) %in% c("StratumID", "SecondaryStratumID", "StateClassID"))], function(x) replace(x, is.infinite(x),0))))
  
  
  #######################
  # STSim-SF datasheets #
  #######################
  # State Attribute Values for net growth based on mass-balance equations
  stateAttributesNetGrowth = datasheet(myScenario, name="stsim_StateAttributeValue", empty = T, optional = T)
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "StratumID"] <- volumeToCarbon$StratumID[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "SecondaryStratumID"] <- volumeToCarbon$SecondaryStratumID[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "StateClassID"] <- volumeToCarbon$StateClassID[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "StateAttributeTypeID"] <- rep(as.character(flowPathways$StateAttributeTypeID[(flowPathways$FromStockTypeID==crossSF("Atmosphere") & flowPathways$ToStockTypeID==crossSF("Merchantable"))]), nrow(volumeToCarbon))
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "AgeMin"] <- volumeToCarbon$AgeMin[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "AgeMax"] <- volumeToCarbon$AgeMax[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "Value"] <- volumeToCarbon$g_all[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[nrow(volumeToCarbon), "AgeMax"] <- NA
  
  stateAttributesNetGrowthMaster = rbind(stateAttributesNetGrowth, stateAttributesNetGrowthMaster)
  
  # SF Flow Pathways
  # Flow Multipliers for biomass net growth based on volume-to-carbon proportions 
  flowMultiplierNetGrowth <- datasheet(myScenario, name="stsim_FlowMultiplier", empty = T, optional = T)
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "StratumID"] <- crosswalkStratumState$StratumID[i]
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "SecondaryStratumID"] <- crosswalkStratumState$SecondaryStratumID[i]
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks),"StateClassID"] <- crosswalkStratumState$StateClassID[i]
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "AgeMin"] <- rep(volumeToCarbon$AgeMin[1:nrow(volumeToCarbon)], numBiomassStocks)
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "AgeMax"] <- rep(stateAttributesNetGrowth$AgeMax, numBiomassStocks)
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "FlowGroupID"] <- c(rep(paste0(as.character(flowPathways$FlowTypeID[(flowPathways$FromStockTypeID==crossSF("Atmosphere") & flowPathways$ToStockTypeID==crossSF("Merchantable"))])," [Type]"), nrow(volumeToCarbon)),
                                                                                         rep(paste0(as.character(flowPathways$FlowTypeID[(flowPathways$FromStockTypeID==crossSF("Atmosphere") & flowPathways$ToStockTypeID==crossSF("Other"))]), " [Type]"),nrow(volumeToCarbon)),
                                                                                         rep(paste0(as.character(flowPathways$FlowTypeID[(flowPathways$FromStockTypeID==crossSF("Atmosphere") & flowPathways$ToStockTypeID==crossSF("Foliage"))]), " [Type]"), nrow(volumeToCarbon)),
                                                                                         rep(paste0(as.character(flowPathways$FlowTypeID[(flowPathways$FromStockTypeID==crossSF("Atmosphere") & flowPathways$ToStockTypeID==crossSF("Fine Roots"))]), " [Type]"), nrow(volumeToCarbon)),
                                                                                         rep(paste0(as.character(flowPathways$FlowTypeID[(flowPathways$FromStockTypeID==crossSF("Atmosphere") & flowPathways$ToStockTypeID==crossSF("Coarse Roots"))]), " [Type]"),  nrow(volumeToCarbon)))
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "Value"] <- c(volumeToCarbon$g_m[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
                                                                                   volumeToCarbon$g_other[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
                                                                                   volumeToCarbon$g_foliage[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
                                                                                   volumeToCarbon$g_fineroots[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
                                                                                   volumeToCarbon$g_coarseroots[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)])
  #flowMultiplierNetGrowth[flowMultiplierNetGrowth$AgeMin == volumeToCarbon$AgeMin[nrow(volumeToCarbon)], "AgeMax"] <- NA
  #flowMultiplierNetGrowth[is.nan.data.frame(flowMultiplierNetGrowth)] <- 0
  
  
  
  #Flow Pathways for biomass turnover rates and DOM transfer and decay rates
  flowPathwayTable <- rbind(biomassTurnoverTable, DOMTable[,names(biomassTurnoverTable)])
  flowMultiplierTurnoverTransferDecayEmission <- datasheet(myScenario, name="stsim_FlowMultiplier", empty=T, optional=T)
  flowMultiplierTurnoverTransferDecayEmission[1:nrow(flowPathwayTable), "StratumID"] <- crosswalkStratumState$StratumID[i]
  flowMultiplierTurnoverTransferDecayEmission[1:nrow(flowPathwayTable), "SecondaryStratumID"] <- crosswalkStratumState$SecondaryStratumID[i]
  flowMultiplierTurnoverTransferDecayEmission[1:nrow(flowPathwayTable), "StateClassID"] <- crosswalkStratumState$StateClassID[i]
  flowMultiplierTurnoverTransferDecayEmission[1:nrow(flowPathwayTable), "FlowGroupID"] = paste0(flowPathwayTable$FlowTypeID," [Type]")
  flowMultiplierTurnoverTransferDecayEmission[1:nrow(flowPathwayTable), "Value"] = flowPathwayTable$Multiplier
  
  # Combine all flow multipliers
  flowMultiplierAll <- rbind(flowMultiplierNetGrowth, flowMultiplierTurnoverTransferDecayEmission)
  flowMultiplierMaster <- rbind(flowMultiplierAll,flowMultiplierMaster)
 
  # } # end VolumeToCarbon if statement  
  
}

# Save flow pathways to scenario
final_pathways_df_unique <- final_pathways_df %>% 
  mutate_if(is.factor, as.character) %>% 
  bind_rows(flowPathways) %>% 
  unique()
saveDatasheet(myScenario, final_pathways_df_unique, name = "stsim_FlowPathway")

saveDatasheet(myScenario, stateAttributesNetGrowthMaster, name = "stsim_StateAttributeValue", append = TRUE)
saveDatasheet(myScenario, flowMultiplierMaster, name="stsim_FlowMultiplier", append=T)


