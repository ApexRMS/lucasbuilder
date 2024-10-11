## stsimcbmcfs3 - flow pathways           
## ApexRMS, Oct 2024

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

crosswalkStratumState <- datasheet(myScenario, "lucasbuilder_CrosswalkSpecies", 
                                   optional = T) # crosswalkStratumState <- datasheet(myLibrary, scenario = 11, "lucasbuilder_CrosswalkSpecies", optional = T)

# load flow type crosswalk for transitions
crossTFT <- read.csv(file.path(pkg_dir,"data/flow-type-crosswalk.csv"))

crosswalkStock <- datasheet(myScenario, "lucasbuilder_CrosswalkStock") # crosswalkStock <- datasheet(myLibrary, scenario = 8, "lucasbuilder_CrosswalkStock")

# SF Flow Pathways
flowPathways = datasheet(myScenario, name="stsim_FlowPathway", empty=F, optional=T) %>% # flowPathways = datasheet(myLibrary, scenario = 1, name="stsim_FlowPathway", empty=F, optional=T) %>% 
  mutate_if(is.factor, as.character)

# Identify growth, biomass transfer, emission, decay, and DOM transfer flows
growthFlows <- flowPathways[flowPathways$FromStockTypeId == crossSF("Atmosphere"),]
emissionFlows <- flowPathways[flowPathways$ToStockTypeId == crossSF("Atmosphere"),]
biomassTurnoverFlows <- flowPathways[(flowPathways$FromStockTypeId %in% biomassStockTypes & flowPathways$ToStockTypeId %in% DOMStockTypes),]
DOMTransferFlows <- distinct(rbind(flowPathways[flowPathways$FromStockTypeId==crossSF("Aboveground Slow DOM") & flowPathways$ToStockTypeId == crossSF("Belowground Slow DOM"),],
                                   flowPathways[flowPathways$FromStockTypeId==crossSF("Softwood Stem Snag") & flowPathways$ToStockTypeId == crossSF("Aboveground Medium DOM"),],
                                   flowPathways[flowPathways$FromStockTypeId==crossSF("Softwood Branch Snag") & flowPathways$ToStockTypeId == crossSF("Aboveground Fast DOM"),],
                                   flowPathways[flowPathways$FromStockTypeId==crossSF("Hardwood Stem Snag") & flowPathways$ToStockTypeId == crossSF("Aboveground Medium DOM"),],
                                   flowPathways[flowPathways$FromStockTypeId==crossSF("Hardwood Branch Snag") & flowPathways$ToStockTypeId == crossSF("Aboveground Fast DOM"),]))
decayFlows <- rbind(flowPathways[(flowPathways$FromStockTypeId %in% DOMStockTypes & flowPathways$ToStockTypeId %in% DOMStockTypes) & (!(flowPathways$FlowTypeId %in% DOMTransferFlows$FlowTypeId)),])

####################################################
# CBM parameters that were not in the CBM database #
####################################################
# DOM Pool Id - "SoilPoolId" taken from CBM User manual Appendix 4 (Kull et al. 2016) - Not found in CMB database
DOMPoolId <- data.frame(CBMStock=c("Aboveground Very Fast DOM", "Belowground Very Fast DOM", "Aboveground Fast DOM", "Belowground Fast DOM", 
                                   "Aboveground Medium DOM", "Aboveground Slow DOM", "Belowground Slow DOM", "Softwood Stem Snag", "Softwood Branch Snag",
                                   "Hardwood Stem Snag", "Hardwood Branch Snag", "Black Carbon", "Peat"), 
                        SoilPoolId=c(0:12))
crosswalkStock <- merge(crosswalkStock, DOMPoolId, all=T)

# Get biomass turnover Proportions (not found in CBM database), taken from Kurtz et al. 2009
proportionMerchantableToSnag <- 1
proportionFineRootsToAGVeryFast <- 0.5
proportionFineRootsToBGVeryFast <- 0.5
proportionCoarseRootsToAGFast <- 0.5
proportionCoarseRootsToBGFast <- 0.5

stateAttributesNetGrowthMaster = datasheet(myScenario, name="stsim_StateAttributeValue", empty = T, optional = T, lookupsAsFactors = F)
flowMultiplierMaster = datasheet(myScenario, name="stsim_FlowMultiplier", empty = T, optional = T, lookupsAsFactors = F)
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
  
  # Get Admin Boundary Id
  adminBoundaryTable <- read.csv(paste0(pkg_dir,"\\data\\tblAdminBoundaryDefault.csv"), stringsAsFactors = F)
  adminBoundaryId <- adminBoundaryTable$AdminBoundaryID[adminBoundaryTable$AdminBoundaryName == as.character(crosswalkStratumState$AdminBoundaryId[i])]
  
  # Get Ecological Boundary Id
  ecoBoundaryTable <- read.csv(paste0(pkg_dir,"\\data\\tblEcoBoundaryDefault.csv"), stringsAsFactors = F)
  ecoBoundaryId <- ecoBoundaryTable$EcoBoundaryID[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryId[i])]
  
  # Get Species and Forest Type Ids
  speciesTypeTable <- read.csv(paste0(pkg_dir,"\\data\\tblSpeciesTypeDefault.csv"), stringsAsFactors = F)
  speciesTypeId <- speciesTypeTable$SpeciesTypeID[speciesTypeTable$SpeciesTypeName == as.character(crosswalkStratumState$SpeciesTypeId[i])]
  forestTypeId <- speciesTypeTable$ForestTypeID[speciesTypeTable$SpeciesTypeID == speciesTypeId]
  
  # Get Forest Type Name
  forestTypeTable <- read.csv(paste0(pkg_dir,"\\data\\tblForestTypeDefault.csv"), stringsAsFactors = F)
  ForestType <- as.character(forestTypeTable$ForestTypeName[forestTypeTable$ForestTypeID == forestTypeId])
  
  # Get Spatial Planning Unit Id (SPUId) from adminBoundary and ecoBoundary
  SPUTable <- read.csv(paste0(pkg_dir,"\\data\\tblSPUDefault.csv"), stringsAsFactors = F)
  SPUId <- SPUTable$SPUID[SPUTable$AdminBoundaryID==adminBoundaryId & SPUTable$EcoBoundaryID==ecoBoundaryId]
  
  # Throw error if SPUId is empty
  if(is.na(SPUId) || is.null(SPUId) || length(SPUId) == 0){
    stop("SPUId is of length 0 or is NA or null")
  }
  
  # Get Stratums and stateclass Ids
  the_stratum <- as.character(crosswalkStratumState$StratumId[i])
  the_secondarystratum <- as.character(crosswalkStratumState$SecondaryStratumId[i])
  the_class <- as.character(crosswalkStratumState$StateClassId[i])
  
  # Get biomass expansion factors
  #biomassExpansionTable <- read.csv(paste0(pkg_dir,"\\data\\tblBioTotalStemwoodForestTypeDefault.csv"), stringsAsFactors = F)
  
  # Get biomass to carbon multipliers
  biomassComponentTable <- read.csv(paste0(pkg_dir,"\\data\\tblBiomassComponent.csv"), stringsAsFactors = F)
  biomassToCarbonTable <- read.csv(paste0(pkg_dir,"\\data\\tblBiomassToCarbonDefault.csv"), stringsAsFactors = F)
  biomassToCarbonTable <- merge.data.frame(biomassToCarbonTable, biomassComponentTable)
  
  # Decay multipliers
  # Temperature modifier parameters
  climateTable <- read.csv(paste0(pkg_dir,"\\data\\tblClimateDefault.csv"), stringsAsFactors = F)
  # There are 2 reference years but they seem to have the same values, I'm arbitrarily choosing 1980
  climateRefYear <- 1980
  if (!is.na(crosswalkStratumState$AverageTemperature[i])){
    meanAnnualTemp <- crosswalkStratumState$AverageTemperature[i]
  } else {
    meanAnnualTemp <- climateTable[climateTable$DefaultSPUID==SPUId & climateTable$Year == climateRefYear, "MeanAnnualTemp"]
  }
  # Stand modifier parameters
  # Note that the maxDecayMult in CBM-CFS3 is 1 which makes the StandMod = 1
  # Do not calculate StandMod this round
  # From Kurz et al. 2009: "In CBM-CFS2 the default value for MaxDecayMult was two. In the CBM-CFS3 the value 
  # defaults to one because more recent studies that examined open canopy effects on decomposition indicated 
  # that decomposition rates are not always higher under open canopies and that decomposition rate responses 
  # may be ecosystem specific (Yanai et al., 2000)." 
  maxDecayMult <- ecoBoundaryTable[ecoBoundaryTable$EcoBoundaryID==ecoBoundaryId, "DecayMult"]
  
  # Get DOM parameters
  DOMParametersTable <- read.csv(paste0(pkg_dir,"\\data\\tblDOMParametersDefault.csv"), stringsAsFactors = F) %>%
    rename(SoilPoolId = SoilPoolID)
  DOMParametersTable <- merge(crosswalkStock[crosswalkStock$StockTypeId %in% DOMStockTypes,], DOMParametersTable)
  DOMParametersTable$TempMod <- exp((meanAnnualTemp - DOMParametersTable$ReferenceTemp)*log(DOMParametersTable$Q10)*0.1)
  if(ForestType=="Softwood"){
    DOMParametersTable <- DOMParametersTable[DOMParametersTable$CBMStock != "Hardwood Stem Snag",]
    DOMParametersTable <- DOMParametersTable[DOMParametersTable$CBMStock != "Hardwood Branch Snag",]
  }
  if(ForestType=="Hardwood"){
    DOMParametersTable <- DOMParametersTable[DOMParametersTable$CBMStock != "Softwood Stem Snag",]
    DOMParametersTable <- DOMParametersTable[DOMParametersTable$CBMStock != "Softwood Branch Snag",]
  }
  DOMDecayTable <- merge(decayFlows, DOMParametersTable, by.x="FromStockTypeId", by.y="StockTypeId")
  DOMEmissionTable <- merge(emissionFlows, DOMParametersTable, by.x="FromStockTypeId", by.y="StockTypeId")
  DOMDecayTable$Multiplier <- (1 - DOMDecayTable$PropToAtmosphere) * DOMDecayTable$OrganicMatterDecayRate * DOMDecayTable$TempMod
  DOMEmissionTable$Multiplier <- DOMEmissionTable$PropToAtmosphere * DOMEmissionTable$OrganicMatterDecayRate * DOMEmissionTable$TempMod
  DOMTable <- rbind(DOMDecayTable, DOMEmissionTable)
  
  # Get DOM transfer rates
  DOMTransferTable <- read.csv(paste0(pkg_dir,"\\data\\tblSlowAGToBGTransferRate.csv"), stringsAsFactors = F)
  transferRateSlowAGToBG <- signif(DOMTransferTable$SlowAGToBGTransferRate,6)
  if(ForestType=="Softwood"){
    transferRateStemSnagToDOM <- signif(ecoBoundaryTable$SoftwoodStemSnagToDOM[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryId[i])],6)
    transferRateBranchSnagToDOM <- signif(ecoBoundaryTable$SoftwoodBranchSnagToDOM[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryId[i])],6)
  }
  if(ForestType=="Hardwood"){
    transferRateStemSnagToDOM <- signif(ecoBoundaryTable$HardwoodStemSnagToDOM[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryId[i])],6)
    transferRateBranchSnagToDOM <- signif(ecoBoundaryTable$HardwoodBranchSnagToDOM[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryId[i])],6)
  }
  # Get biomass turnover rates
  speciesTurnoverRatesTable <- read.csv(paste0(pkg_dir,"\\data\\tblSpeciesTypeDefault.csv"), stringsAsFactors = F)
  turnoverRates <- speciesTurnoverRatesTable[speciesTurnoverRatesTable$SpeciesTypeName==as.character(crosswalkStratumState$SpeciesTypeId[i]),]
  
  # Get the disturbance matrix information
  DMassociation = read.csv(paste0(pkg_dir,"\\data\\tblDMAssociationDefault.csv"), stringsAsFactors = F)
  DMassociation = DMassociation[DMassociation$DefaultEcoBoundaryID == ecoBoundaryId,]
  
  disturbanceType = read.csv(paste0(pkg_dir,"\\data\\tblDisturbanceTypeDefault.csv"), stringsAsFactors = F)
  disturbanceMatrix <- read.csv(paste0(pkg_dir,"\\data\\tblDM.csv"), stringsAsFactors = F)
  
  dmValuesLookup = read.csv(paste0(pkg_dir,"\\data\\tblDMValuesLookup.csv"), stringsAsFactors = F)
  
  sourceName <- read.csv(paste0(pkg_dir,"\\data\\tblSourceName.csv"), stringsAsFactors = F) %>% rename("DMRow" = "Row")
  sinkName <- read.csv(paste0(pkg_dir,"\\data\\tblSinkName.csv"), stringsAsFactors = F) %>% rename("DMColumn" = "Column")
  
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
      filter(DistTypeName %in% crosswalkDisturbance$DisturbanceTypeId)
      
      # Fix typo in CBM database where "Hardwood Coarse Roots" is misspelled as "Hardwood Coarse roots"
      df[df == "Hardwood Coarse roots"] <- "Hardwood Coarse Roots"
    
    # discriminate between hardwood and softwood
    
    opposite <- ifelse(ForestType == "Softwood", "Hardwood", "Softwood")
    
    df_filtered <- df %>% 
      filter(!str_detect(Source, opposite))
    
    sources <- data.frame(CBMSource = unique(df_filtered$Source), 
                          FromStockId = "")
      
    sinks = data.frame(CBMSink = unique(df_filtered$Sink),
                       ToStockId = "")
    
    transitions = data.frame(DistTypeName = unique(df_filtered$DistTypeName),
                             TransitionTypeId = "")
    
    #d = data.frame(CBMStocks = df_filtered$Source)
    #d1 = data.frame(CBMStocks = df_filtered$Sink)
    #d2 = bind_rows(d,d1)
    #d3 = data.frame(CBMStocks = unique(d2$CBMStocks), LUCASStocks = "")
    
    temp_crosswalkStock = datasheet(myScenario, name = "lucasbuilder_CrosswalkStock") #  temp_crosswalkStock <- datasheet(myLibrary, scenario = 8, name ="lucasbuilder_CrosswalkStock")
    
    # temp_crosswalkStock[16,1] <- "Products"
    
    temp_crosswalkDisturbance = datasheet(myScenario, name = "lucasbuilder_CrosswalkDisturbance") %>% # temp_crosswalkDisturbance <- datasheet(myLibrary, scenario = 7, name ="lucasbuilder_CrosswalkDisturbance") %>%
      mutate_if(is.factor, as.character)
    
    temp_pathways_df = df_filtered %>% left_join(temp_crosswalkStock, by = c("Source" = "CBMStock")) %>% 
      rename("FromStockTypeId"="StockTypeId") %>%
      left_join(temp_crosswalkStock, by = c("Sink" = "CBMStock")) %>% 
      rename("ToStockTypeId"="StockTypeId") %>%
      select(DistTypeName, FromStockTypeId, ToStockTypeId, Proportion) %>%
      mutate_if(is.factor, as.character) %>%
      left_join(temp_crosswalkDisturbance, by = c("DistTypeName" = "DisturbanceTypeId")) %>%
      filter(!is.na(TransitionGroupId)) %>% 
      filter(!is.na(FromStockTypeId)) %>%
      #mutate(FromStockTypeId = ifelse(FromStockTypeId != ToStockTypeId, FromStockTypeId, NA)) %>%
      filter(!is.na(FromStockTypeId)) %>%
      rename("Multiplier" = "Proportion") %>%
      mutate(FlowTypeId = "", Multiplier = round(Multiplier, 4)) %>%
      select(FromStockTypeId, ToStockTypeId, TransitionGroupId, DistTypeName, FlowTypeId, Multiplier) 
    
    # head(temp_pathways_df)
    
    temp_pathways_df_clean <- temp_pathways_df %>% # mutate(FlowTypeId = pathways) %>% 
      mutate(FromStratumId = the_stratum, # ToStratumId = the_stratum, 
             FromSecondaryStratumId = the_secondarystratum, #ToSecondaryStratumId = the_secondarystratum,
             FromStateClassId = the_class#, ToStateClassId = the_class
      ) %>% 
      select(-DistTypeName) %>% 
      filter(FromStockTypeId != ToStockTypeId)
    
    for (r in 1:nrow(temp_pathways_df_clean)){ # r=1
      fStock <- temp_pathways_df_clean$FromStockTypeId[r]
      tstock <- temp_pathways_df_clean$ToStockTypeId[r]
      temp_pathways_df_clean$FlowTypeId[r] <- crossTFT$FlowType[which(crossTFT$FromStockType == fStock & crossTFT$ToStockType == tstock)]
    }
    
    pathways <- temp_pathways_df_clean$FlowTypeId
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
  DOMTransferFlows[DOMTransferFlows$FromStockTypeId==crossSF("Aboveground Slow DOM") & DOMTransferFlows$ToStockTypeId == crossSF("Belowground Slow DOM"), "Multiplier"] <- transferRateSlowAGToBG
  
  if(ForestType == "Softwood"){
    DOMTransferFlows[DOMTransferFlows$FromStockTypeId==crossSF("Softwood Stem Snag") & DOMTransferFlows$ToStockTypeId == crossSF("Aboveground Medium DOM"), "Multiplier"] <- transferRateStemSnagToDOM
    DOMTransferFlows[DOMTransferFlows$FromStockTypeId==crossSF("Softwood Branch Snag") & DOMTransferFlows$ToStockTypeId == crossSF("Aboveground Fast DOM"), "Multiplier"] <- transferRateBranchSnagToDOM
  }
  if(ForestType == "Hardwood"){
    DOMTransferFlows[DOMTransferFlows$FromStockTypeId==crossSF("Hardwood Stem Snag") & DOMTransferFlows$ToStockTypeId == crossSF("Aboveground Medium DOM"), "Multiplier"] <- transferRateStemSnagToDOM
    DOMTransferFlows[DOMTransferFlows$FromStockTypeId==crossSF("Hardwood Branch Snag") & DOMTransferFlows$ToStockTypeId == crossSF("Aboveground Fast DOM"), "Multiplier"] <- transferRateBranchSnagToDOM
  }
  
  DOMTable <- rbind(DOMTable[,names(DOMTransferFlows)], DOMTransferFlows)
  
  # Biomass turnover rates
  turnOverRateStemAnnual <- signif(ecoBoundaryTable$StemAnnualTurnOverRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryId[i])],6)
  turnOverRateFineRootsAGVeryFast <- signif(turnoverRates$FineRootTurnPropSlope, 6)
  turnOverRateFineRootsBGVeryFast <- signif(turnoverRates$FineRootTurnPropSlope, 6)
  turnOverRateCoarseRootsAGFast <- signif(turnoverRates$CoarseRootTurnProp, 6)
  turnOverRateCoarseRootsBGFast <- signif(turnoverRates$CoarseRootTurnProp, 6)
  if(ForestType == "Softwood"){
    turnOverRateBranch <- signif(ecoBoundaryTable$SoftwoodBranchTurnOverRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryId[i])],6)
    turnOverRateFoliage <- signif(ecoBoundaryTable$SoftwoodFoliageFallRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryId[i])],6)
  }
  if(ForestType == "Hardwood"){
    turnOverRateBranch <- signif(ecoBoundaryTable$HardwoodBranchTurnOverRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryId[i])],6)
    turnOverRateFoliage <- signif(ecoBoundaryTable$HardwoodFoliageFallRate[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryId[i])],6)
  }
  
  # Turnover proportions
  proportionOtherToBranchSnag <- signif(turnoverRates$BranchesToBranchSnag,6)
  proportionOtherToAGFast <- 1 - proportionOtherToBranchSnag
  
  biomassTurnoverTable <- biomassTurnoverFlows
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Merchantable")] <- turnOverRateStemAnnual * proportionMerchantableToSnag 
  if(ForestType == "Softwood") biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Other") & biomassTurnoverTable$ToStockTypeId == crossSF("Softwood Branch Snag")] <- turnOverRateBranch * proportionOtherToBranchSnag
  if(ForestType == "Hardwood") biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Other") & biomassTurnoverTable$ToStockTypeId == crossSF("Hardwood Branch Snag")] <- turnOverRateBranch * proportionOtherToBranchSnag
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Other") & biomassTurnoverTable$ToStockTypeId == crossSF("Aboveground Fast DOM")] <- turnOverRateBranch * proportionOtherToAGFast
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Foliage")] <- turnOverRateFoliage * proportionFoliageToAGVeryFast 
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Fine Roots") & biomassTurnoverTable$ToStockTypeId == crossSF("Aboveground Very Fast DOM")] <- turnOverRateFineRootsAGVeryFast * proportionFineRootsToAGVeryFast
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Fine Roots") & biomassTurnoverTable$ToStockTypeId == crossSF("Belowground Very Fast DOM")] <- turnOverRateFineRootsBGVeryFast * proportionFineRootsToBGVeryFast
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Coarse Roots") & biomassTurnoverTable$ToStockTypeId == crossSF("Aboveground Fast DOM")] <- turnOverRateCoarseRootsAGFast * proportionCoarseRootsToAGFast
  biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Coarse Roots") & biomassTurnoverTable$ToStockTypeId == crossSF("Belowground Fast DOM")] <- turnOverRateCoarseRootsBGFast * proportionCoarseRootsToBGFast
  
  ########################################################
  # Calculate net growth based on mass-balance equations #
  ########################################################
  
  
  # Original approach using CBM Output. Remove eventually...
  if(useCBMAgeVsCarbonCurves==T){
    stateAttributeValues <- datasheet(myScenario, "stsim_StateAttributeValue", empty=FALSE, optional=TRUE) #  stateAttributeValues <- datasheet(myLibrary, scenario = 12, "stsim_StateAttributeValue", empty=FALSE, optional=TRUE)
    stateAttributeValuesWide <- spread(stateAttributeValues, key="StateAttributeTypeId", value = "Value") %>%
      mutate_if(is.factor, as.character)
    carbonInitialConditions <- datasheet(myScenario, "stsim_InitialStockNonSpatial", empty=FALSE, optional=TRUE) # carbonInitialConditions <- datasheet(myLibrary, scenario = 5, "stsimsf_InitialStockNonSpatial", empty=FALSE, optional=TRUE)
 
    if(!is.na(crosswalkStratumState$StratumId[i]) & !is.na(crosswalkStratumState$SecondaryStratumId[i])){ 
      volumeToCarbon <- filter(stateAttributeValuesWide, StratumId == as.character(crosswalkStratumState$StratumId[i]) &  SecondaryStratumId == as.character(crosswalkStratumState$SecondaryStratumId[i]) & StateClassId == as.character(crosswalkStratumState$StateClassId[i])) }
    if(!is.na(crosswalkStratumState$StratumId[i]) & is.na(crosswalkStratumState$SecondaryStratumId[i])){ 
      volumeToCarbon <- filter(stateAttributeValuesWide, StratumId == as.character(crosswalkStratumState$StratumId[i]) & StateClassId == as.character(crosswalkStratumState$StateClassId[i])) }
    if(is.na(crosswalkStratumState$StratumId[i]) & is.na(crosswalkStratumState$SecondaryStratumId[i])){ 
     volumeToCarbon <- filter(stateAttributeValuesWide, StateClassId == crosswalkStratumState$StateClassId[i])}
    
    volumeToCarbon$c_m <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeId[carbonInitialConditions$StockTypeId == crossSF("Merchantable")])]
    volumeToCarbon$c_foliage <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeId[carbonInitialConditions$StockTypeId == crossSF("Foliage")])]
    volumeToCarbon$c_other <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeId[carbonInitialConditions$StockTypeId == crossSF("Other")])]
    volumeToCarbon$c_fineroots <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeId[carbonInitialConditions$StockTypeId == crossSF("Fine Roots")])]
    volumeToCarbon$c_coarseroots <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeId[carbonInitialConditions$StockTypeId == crossSF("Coarse Roots")])]

  }

  volumeToCarbon$c_m1 <- c(volumeToCarbon$c_m[2:nrow(volumeToCarbon)], NA)
  volumeToCarbon$c_foliage1 <- c(volumeToCarbon$c_foliage[2:nrow(volumeToCarbon)], NA)
  volumeToCarbon$c_other1 <- c(volumeToCarbon$c_other[2:nrow(volumeToCarbon)], NA)
  volumeToCarbon$c_fineroots1 <- c(volumeToCarbon$c_fineroots[2:nrow(volumeToCarbon)], NA)
  volumeToCarbon$c_coarseroots1 <- c(volumeToCarbon$c_coarseroots[2:nrow(volumeToCarbon)], NA)
  
  # Growth happens after biomass transfer
  if(ForestType == "Softwood") volumeToCarbon$g_m <- volumeToCarbon$c_m1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Merchantable") & biomassTurnoverTable$ToStockTypeId == crossSF("Softwood Stem Snag")]) * volumeToCarbon$c_m
  if(ForestType == "Hardwood") volumeToCarbon$g_m <- volumeToCarbon$c_m1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Merchantable") & biomassTurnoverTable$ToStockTypeId == crossSF("Hardwood Stem Snag")]) * volumeToCarbon$c_m
  volumeToCarbon$g_foliage <- volumeToCarbon$c_foliage1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Foliage") & biomassTurnoverTable$ToStockTypeId == crossSF("Aboveground Very Fast DOM")]) * volumeToCarbon$c_foliage
  if(ForestType == "Softwood") volumeToCarbon$g_other <- volumeToCarbon$c_other1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Other") & biomassTurnoverTable$ToStockTypeId == crossSF("Aboveground Fast DOM")] - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Other") & biomassTurnoverTable$ToStockTypeId == crossSF("Softwood Branch Snag")]) * volumeToCarbon$c_other
  if(ForestType == "Hardwood") volumeToCarbon$g_other <- volumeToCarbon$c_other1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Other") & biomassTurnoverTable$ToStockTypeId == crossSF("Aboveground Fast DOM")] - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Other") & biomassTurnoverTable$ToStockTypeId == crossSF("Hardwood Branch Snag")]) * volumeToCarbon$c_other
  volumeToCarbon$g_fineroots <- volumeToCarbon$c_fineroots1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Fine Roots") & biomassTurnoverTable$ToStockTypeId == crossSF("Aboveground Very Fast DOM")]  - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Fine Roots") & biomassTurnoverTable$ToStockTypeId == crossSF("Belowground Very Fast DOM")]) * volumeToCarbon$c_fineroots
  volumeToCarbon$g_coarseroots <- volumeToCarbon$c_coarseroots1 - (1 - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Coarse Roots") & biomassTurnoverTable$ToStockTypeId == crossSF("Aboveground Fast DOM")]  - biomassTurnoverTable$Multiplier[biomassTurnoverTable$FromStockTypeId == crossSF("Coarse Roots") & biomassTurnoverTable$ToStockTypeId == crossSF("Belowground Fast DOM")]) * volumeToCarbon$c_coarseroots
  volumeToCarbon$g_all <- volumeToCarbon$g_m + volumeToCarbon$g_foliage + volumeToCarbon$g_other + volumeToCarbon$g_fineroots + volumeToCarbon$g_coarseroots
  volumeToCarbon <- volumeToCarbon[1:(nrow(volumeToCarbon)-1),]
  
  # Replace NaN values with 0's
  volumeToCarbon[is.nan.data.frame(volumeToCarbon)] <- 0
  volumeToCarbon <- volumeToCarbon %>% mutate_if(is.factor, as.character)
  volumeToCarbon <- cbind(volumeToCarbon[,c("StratumId", "SecondaryStratumId", "StateClassId")], do.call(data.frame,lapply(volumeToCarbon[,!(names(volumeToCarbon) %in% c("StratumId", "SecondaryStratumId", "StateClassId"))], function(x) replace(x, is.infinite(x),0))))
  
  
  #######################
  # STSim-SF datasheets #
  #######################
  # State Attribute Values for net growth based on mass-balance equations
  stateAttributesNetGrowth = datasheet(myScenario, name="stsim_StateAttributeValue", empty = T, optional = T, lookupsAsFactors = F)
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "StratumId"] <- as.character(volumeToCarbon$StratumId[1:nrow(volumeToCarbon)])
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "SecondaryStratumId"] <- as.character(volumeToCarbon$SecondaryStratumId[1:nrow(volumeToCarbon)])
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "StateClassId"] <- as.character(volumeToCarbon$StateClassId[1:nrow(volumeToCarbon)])
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "StateAttributeTypeId"] <- rep(as.character(flowPathways$StateAttributeTypeId[(flowPathways$FromStockTypeId==crossSF("Atmosphere") & flowPathways$ToStockTypeId==crossSF("Merchantable"))]), nrow(volumeToCarbon))
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "AgeMin"] <- volumeToCarbon$AgeMin[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "AgeMax"] <- volumeToCarbon$AgeMax[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[1:nrow(volumeToCarbon), "Value"] <- volumeToCarbon$g_all[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[nrow(volumeToCarbon), "AgeMax"] <- NA
  
  stateAttributesNetGrowthMaster = rbind(stateAttributesNetGrowth, stateAttributesNetGrowthMaster)
  
  # SF Flow Pathways
  # Flow Multipliers for biomass net growth based on volume-to-carbon proportions 
  flowMultiplierNetGrowth <- datasheet(myScenario, name="stsim_FlowMultiplier", empty = T, optional = T, lookupsAsFactors = F)
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "StratumId"] <- as.character(crosswalkStratumState$StratumId[i])
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "SecondaryStratumId"] <- as.character(crosswalkStratumState$SecondaryStratumId[i])
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks),"StateClassId"] <- as.character(crosswalkStratumState$StateClassId[i])
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "AgeMin"] <- rep(volumeToCarbon$AgeMin[1:nrow(volumeToCarbon)], numBiomassStocks)
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "AgeMax"] <- rep(stateAttributesNetGrowth$AgeMax, numBiomassStocks)
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "FlowGroupId"] <- c(rep(paste0(as.character(flowPathways$FlowTypeId[(flowPathways$FromStockTypeId==crossSF("Atmosphere") & flowPathways$ToStockTypeId==crossSF("Merchantable"))])," [Type]"), nrow(volumeToCarbon)),
                                                                                         rep(paste0(as.character(flowPathways$FlowTypeId[(flowPathways$FromStockTypeId==crossSF("Atmosphere") & flowPathways$ToStockTypeId==crossSF("Other"))]), " [Type]"),nrow(volumeToCarbon)),
                                                                                         rep(paste0(as.character(flowPathways$FlowTypeId[(flowPathways$FromStockTypeId==crossSF("Atmosphere") & flowPathways$ToStockTypeId==crossSF("Foliage"))]), " [Type]"), nrow(volumeToCarbon)),
                                                                                         rep(paste0(as.character(flowPathways$FlowTypeId[(flowPathways$FromStockTypeId==crossSF("Atmosphere") & flowPathways$ToStockTypeId==crossSF("Fine Roots"))]), " [Type]"), nrow(volumeToCarbon)),
                                                                                         rep(paste0(as.character(flowPathways$FlowTypeId[(flowPathways$FromStockTypeId==crossSF("Atmosphere") & flowPathways$ToStockTypeId==crossSF("Coarse Roots"))]), " [Type]"),  nrow(volumeToCarbon)))
  flowMultiplierNetGrowth[1:(nrow(volumeToCarbon)*numBiomassStocks), "Value"] <- c(volumeToCarbon$g_m[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
                                                                                   volumeToCarbon$g_other[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
                                                                                   volumeToCarbon$g_foliage[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
                                                                                   volumeToCarbon$g_fineroots[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
                                                                                   volumeToCarbon$g_coarseroots[1:nrow(volumeToCarbon)] / volumeToCarbon$g_all[1:nrow(volumeToCarbon)])
  #flowMultiplierNetGrowth[flowMultiplierNetGrowth$AgeMin == volumeToCarbon$AgeMin[nrow(volumeToCarbon)], "AgeMax"] <- NA
  #flowMultiplierNetGrowth[is.nan.data.frame(flowMultiplierNetGrowth)] <- 0
  
  
  
  #Flow Pathways for biomass turnover rates and DOM transfer and decay rates
  flowPathwayTable <- rbind(biomassTurnoverTable, DOMTable[,names(biomassTurnoverTable)])
  flowMultiplierTurnoverTransferDecayEmission <- datasheet(myScenario, name="stsim_FlowMultiplier", empty=T, optional=T, lookupsAsFactors = F)
  flowMultiplierTurnoverTransferDecayEmission[1:nrow(flowPathwayTable), "StratumId"] <- as.character(crosswalkStratumState$StratumId[i])
  flowMultiplierTurnoverTransferDecayEmission[1:nrow(flowPathwayTable), "SecondaryStratumId"] <- as.character(crosswalkStratumState$SecondaryStratumId[i])
  flowMultiplierTurnoverTransferDecayEmission[1:nrow(flowPathwayTable), "StateClassId"] <- as.character(crosswalkStratumState$StateClassId[i])
  flowMultiplierTurnoverTransferDecayEmission[1:nrow(flowPathwayTable), "FlowGroupId"] = paste0(flowPathwayTable$FlowTypeId," [Type]")
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
saveDatasheet(myScenario, final_pathways_df_unique, name = "stsim_FlowPathway", append = FALSE)

# remove rows that are all NAs
stateAttributesNetGrowthMaster <- stateAttributesNetGrowthMaster %>%
  filter(rowSums(is.na(stateAttributesNetGrowthMaster)) != ncol(stateAttributesNetGrowthMaster))

flowMultiplierMaster <- flowMultiplierMaster %>%
  filter(rowSums(is.na(flowMultiplierMaster)) != ncol(flowMultiplierMaster))

saveDatasheet(myScenario, stateAttributesNetGrowthMaster, name = "stsim_StateAttributeValue", append = TRUE)
saveDatasheet(myScenario, flowMultiplierMaster, name="stsim_FlowMultiplier", append=T)


