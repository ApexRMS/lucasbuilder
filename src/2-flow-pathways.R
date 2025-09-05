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
#useCBMAgeVsCarbonCurves=T
useCBMAgeVsCarbonCurves = F


###################################
# Get CBM database and crosswalks #
###################################

crosswalkStratumState <- datasheet(
  myScenario,
  "lucasbuilder_CrosswalkSpecies",
  optional = T
) # crosswalkStratumState <- datasheet(myLibrary, scenario = 11, "lucasbuilder_CrosswalkSpecies", optional = T)

# load flow type crosswalk for transitions
crossTFT <- read.csv(file.path(pkg_dir, "data/flow-type-crosswalk.csv"))

crosswalkStock <- datasheet(myScenario, "lucasbuilder_CrosswalkStock") # crosswalkStock <- datasheet(myLibrary, scenario = 8, "lucasbuilder_CrosswalkStock")

# SF Flow Pathways
flowPathways = datasheet(
  myScenario,
  name = "stsim_FlowPathway",
  empty = F,
  optional = T
) %>% # flowPathways = datasheet(myLibrary, scenario = 1, name="stsim_FlowPathway", empty=F, optional=T) %>%
  mutate_if(is.factor, as.character)

# Identify growth, biomass transfer, emission, decay, and DOM transfer flows
growthFlows <- flowPathways[
  flowPathways$FromStockTypeId == crossSF("Atmosphere"),
]
emissionFlows <- flowPathways[
  flowPathways$ToStockTypeId == crossSF("Atmosphere"),
]
biomassTurnoverFlows <- flowPathways[
  (flowPathways$FromStockTypeId %in%
    biomassStockTypes &
    flowPathways$ToStockTypeId %in% DOMStockTypes),
]
DOMTransferFlows <- distinct(rbind(
  flowPathways[
    flowPathways$FromStockTypeId == crossSF("Aboveground Slow DOM") &
      flowPathways$ToStockTypeId == crossSF("Belowground Slow DOM"),
  ],
  flowPathways[
    flowPathways$FromStockTypeId == crossSF("Softwood Stem Snag") &
      flowPathways$ToStockTypeId == crossSF("Aboveground Medium DOM"),
  ],
  flowPathways[
    flowPathways$FromStockTypeId == crossSF("Softwood Branch Snag") &
      flowPathways$ToStockTypeId == crossSF("Aboveground Fast DOM"),
  ],
  flowPathways[
    flowPathways$FromStockTypeId == crossSF("Hardwood Stem Snag") &
      flowPathways$ToStockTypeId == crossSF("Aboveground Medium DOM"),
  ],
  flowPathways[
    flowPathways$FromStockTypeId == crossSF("Hardwood Branch Snag") &
      flowPathways$ToStockTypeId == crossSF("Aboveground Fast DOM"),
  ]
))
decayFlows <- rbind(flowPathways[
  (flowPathways$FromStockTypeId %in%
    DOMStockTypes &
    flowPathways$ToStockTypeId %in% DOMStockTypes) &
    (!(flowPathways$FlowTypeId %in% DOMTransferFlows$FlowTypeId)),
])

####################################################
# CBM parameters that were not in the CBM database #
####################################################
# DOM Pool Id - "SoilPoolId" taken from CBM User manual Appendix 4 (Kull et al. 2016) - Not found in CMB database
DOMPoolId <- data.frame(
  CBMStock = c(
    "Aboveground Very Fast DOM",
    "Belowground Very Fast DOM",
    "Aboveground Fast DOM",
    "Belowground Fast DOM",
    "Aboveground Medium DOM",
    "Aboveground Slow DOM",
    "Belowground Slow DOM",
    "Softwood Stem Snag",
    "Softwood Branch Snag",
    "Hardwood Stem Snag",
    "Hardwood Branch Snag",
    "Black Carbon",
    "Peat"
  ),
  SoilPoolId = c(0:12)
)
crosswalkStock <- merge(crosswalkStock, DOMPoolId, all = T)

# Get biomass turnover Proportions (not found in CBM database), taken from Kurtz et al. 2009
proportionMerchantableToSnag <- 1
proportionFineRootsToAGVeryFast <- 0.5
proportionFineRootsToBGVeryFast <- 0.5
proportionCoarseRootsToAGFast <- 0.5
proportionCoarseRootsToBGFast <- 0.5

stateAttributesNetGrowthMaster = datasheet(
  myScenario,
  name = "stsim_StateAttributeValue",
  empty = T,
  optional = T,
  lookupsAsFactors = F
)
flowMultiplierMaster = datasheet(
  myScenario,
  name = "stsim_FlowMultiplier",
  empty = T,
  optional = T,
  lookupsAsFactors = F
)
grossMerchantableVolumeAll = datasheet(
  myScenario,
  name = "lucasbuilder_MerchantableVolumeCurve",
  optional = T,
  lookupsAsFactors = F
)

crosswalkDisturbance = datasheet(
  myScenario,
  name = "lucasbuilder_CrosswalkDisturbance"
) # crosswalkDisturbance <- datasheet(myLibrary, scenario = 7,"lucasbuilder_CrosswalkDisturbance")

# Loop over all entries in crosswalkStratumState
# Set up variable to accumulate during the loop
pathways_all <- c()
final_pathways_df <- data.frame()

for (i in 1:nrow(crosswalkStratumState)) {
  #i<-1
  ####################################
  # CBM parameters from CBM Database #
  ####################################

  # Merchantable volume curve data
  grossMerchantableVolume <- grossMerchantableVolumeAll %>%
    filter(
      StateClassId == as.character(crosswalkStratumState$StateClassId[i]) &
        StratumId == as.character(crosswalkStratumState$StratumId[i]) &
        SecondaryStratumId ==
          as.character(crosswalkStratumState$SecondaryStratumId[i])
    ) %>%
    arrange(Age)

  # Get Admin Boundary Id
  adminBoundaryTable <- read.csv(
    paste0(pkg_dir, "\\data\\tblAdminBoundaryDefault.csv"),
    stringsAsFactors = F
  )
  adminBoundaryId <- adminBoundaryTable$AdminBoundaryID[
    adminBoundaryTable$AdminBoundaryName ==
      as.character(crosswalkStratumState$AdminBoundaryId[i])
  ]

  # Get Ecological Boundary Id
  ecoBoundaryTable <- read.csv(
    paste0(pkg_dir, "\\data\\tblEcoBoundaryDefault.csv"),
    stringsAsFactors = F
  )
  ecoBoundaryId <- ecoBoundaryTable$EcoBoundaryID[
    ecoBoundaryTable$EcoBoundaryName ==
      as.character(crosswalkStratumState$EcoBoundaryId[i])
  ]

  # Get Species and Forest Type Ids
  speciesTypeTable <- read.csv(
    paste0(pkg_dir, "\\data\\tblSpeciesTypeDefault.csv"),
    stringsAsFactors = F
  )
  speciesTypeId <- speciesTypeTable$SpeciesTypeID[
    speciesTypeTable$SpeciesTypeName ==
      as.character(crosswalkStratumState$SpeciesTypeId[i])
  ]
  forestTypeId <- speciesTypeTable$ForestTypeID[
    speciesTypeTable$SpeciesTypeID == speciesTypeId
  ]
  genusTypeId <- speciesTypeTable$GenusID[
    speciesTypeTable$SpeciesTypeID == speciesTypeId
  ]

  # Get Forest Type Name
  forestTypeTable <- read.csv(
    paste0(pkg_dir, "\\data\\tblForestTypeDefault.csv"),
    stringsAsFactors = F
  )
  ForestType <- as.character(forestTypeTable$ForestTypeName[
    forestTypeTable$ForestTypeID == forestTypeId
  ])
  forestType <- ForestType

  # Get Spatial Planning Unit Id (SPUId) from adminBoundary and ecoBoundary
  SPUTable <- read.csv(
    paste0(pkg_dir, "\\data\\tblSPUDefault.csv"),
    stringsAsFactors = F
  )
  SPUId <- SPUTable$SPUID[
    SPUTable$AdminBoundaryID == adminBoundaryId &
      SPUTable$EcoBoundaryID == ecoBoundaryId
  ]

  # Throw error if SPUId is empty
  if (is.na(SPUId) || is.null(SPUId) || length(SPUId) == 0) {
    stop("SPUId is of length 0 or is NA or null")
  }

  # Get Stratums and stateclass Ids
  the_stratum <- as.character(crosswalkStratumState$StratumId[i])
  the_secondarystratum <- as.character(crosswalkStratumState$SecondaryStratumId[
    i
  ])
  the_class <- as.character(crosswalkStratumState$StateClassId[i])

  # Get biomass expansion factors
  if (grepl("- Genus type", crosswalkStratumState$SpeciesTypeId[i])) {
    biomassExpansionTable <- read.csv(
      paste0(pkg_dir, "\\data\\tblBioTotalStemwoodGenusDefault.csv"),
      stringsAsFactors = F
    )

    biomassExpansionTableRow <- biomassExpansionTable %>%
      filter(DefaultSPUID == SPUId, DefaultGenusID == genusTypeId)
  } else {
    biomassExpansionTable <- read.csv(
      paste0(pkg_dir, "\\data\\tblBioTotalStemwoodSpeciesTypeDefault.csv"),
      stringsAsFactors = F
    )

    biomassExpansionTableRow <- biomassExpansionTable %>%
      filter(DefaultSPUID == SPUId, DefaultSpeciesTypeID == speciesTypeId)

    if (nrow(biomassExpansionTableRow) == 0) {
      #print(paste0(the_class," is missing expansion factors, trying genus type instead"))

      biomassExpansionTable <- read.csv(
        paste0(pkg_dir, "\\data\\tblBioTotalStemwoodGenusDefault.csv"),
        stringsAsFactors = F
      )

      biomassExpansionTableRow <- biomassExpansionTable %>%
        filter(DefaultSPUID == SPUId, DefaultGenusID == genusTypeId)

      if (nrow(biomassExpansionTableRow) == 0) {
        #print(paste0(the_class," is still missing expansion factors, trying forest type instead"))

        biomassExpansionTable <- read.csv(
          paste0(pkg_dir, "\\data\\tblBioTotalStemwoodForestTypeDefault.csv"),
          stringsAsFactors = F
        )

        biomassExpansionTableRow <- biomassExpansionTable %>%
          filter(DefaultSPUID == SPUId, DefaultForestTypeID == forestTypeId)
      }
    }
  }

  # Get biomass to carbon multipliers
  biomassComponentTable <- read.csv(
    paste0(pkg_dir, "\\data\\tblBiomassComponent.csv"),
    stringsAsFactors = F
  )
  biomassToCarbonTable <- read.csv(
    paste0(pkg_dir, "\\data\\tblBiomassToCarbonDefault.csv"),
    stringsAsFactors = F
  )
  biomassToCarbonTable <- merge.data.frame(
    biomassToCarbonTable,
    biomassComponentTable
  )

  # Decay multipliers
  # Temperature modifier parameters
  climateTable <- read.csv(
    paste0(pkg_dir, "\\data\\tblClimateDefault.csv"),
    stringsAsFactors = F
  )
  # There are 2 reference years but they seem to have the same values, I'm arbitrarily choosing 1980
  climateRefYear <- 1980
  if (!is.na(crosswalkStratumState$AverageTemperature[i])) {
    meanAnnualTemp <- crosswalkStratumState$AverageTemperature[i]
  } else {
    meanAnnualTemp <- climateTable[
      climateTable$DefaultSPUID == SPUId & climateTable$Year == climateRefYear,
      "MeanAnnualTemp"
    ]
  }
  # Stand modifier parameters
  # Note that the maxDecayMult in CBM-CFS3 is 1 which makes the StandMod = 1
  # Do not calculate StandMod this round
  # From Kurz et al. 2009: "In CBM-CFS2 the default value for MaxDecayMult was two. In the CBM-CFS3 the value
  # defaults to one because more recent studies that examined open canopy effects on decomposition indicated
  # that decomposition rates are not always higher under open canopies and that decomposition rate responses
  # may be ecosystem specific (Yanai et al., 2000)."
  maxDecayMult <- ecoBoundaryTable[
    ecoBoundaryTable$EcoBoundaryID == ecoBoundaryId,
    "DecayMult"
  ]

  # Get DOM parameters
  DOMParametersTable <- read.csv(
    paste0(pkg_dir, "\\data\\tblDOMParametersDefault.csv"),
    stringsAsFactors = F
  ) %>%
    rename(SoilPoolId = SoilPoolID)
  DOMParametersTable <- merge(
    crosswalkStock[crosswalkStock$StockTypeId %in% DOMStockTypes, ],
    DOMParametersTable
  )
  DOMParametersTable$TempMod <- exp(
    (meanAnnualTemp - DOMParametersTable$ReferenceTemp) *
      log(DOMParametersTable$Q10) *
      0.1
  )
  if (ForestType == "Softwood") {
    DOMParametersTable <- DOMParametersTable[
      DOMParametersTable$CBMStock != "Hardwood Stem Snag",
    ]
    DOMParametersTable <- DOMParametersTable[
      DOMParametersTable$CBMStock != "Hardwood Branch Snag",
    ]
  }
  if (ForestType == "Hardwood") {
    DOMParametersTable <- DOMParametersTable[
      DOMParametersTable$CBMStock != "Softwood Stem Snag",
    ]
    DOMParametersTable <- DOMParametersTable[
      DOMParametersTable$CBMStock != "Softwood Branch Snag",
    ]
  }

  DOMParametersTable <- DOMParametersTable %>%
    mutate(
      OrganicMatterDecayRateTempCorected = ifelse(
        OrganicMatterDecayRate * TempMod > 1,
        1,
        OrganicMatterDecayRate * TempMod
      )
    )

  DOMDecayTable <- merge(
    decayFlows,
    DOMParametersTable,
    by.x = "FromStockTypeId",
    by.y = "StockTypeId"
  )
  DOMEmissionTable <- merge(
    emissionFlows,
    DOMParametersTable,
    by.x = "FromStockTypeId",
    by.y = "StockTypeId"
  )
  DOMDecayTable$Multiplier <- (1 - DOMDecayTable$PropToAtmosphere) *
    DOMDecayTable$OrganicMatterDecayRateTempCorected
  DOMEmissionTable$Multiplier <- DOMEmissionTable$PropToAtmosphere *
    DOMEmissionTable$OrganicMatterDecayRateTempCorected
  DOMTable <- rbind(DOMDecayTable, DOMEmissionTable)

  # Get DOM transfer rates
  DOMTransferTable <- read.csv(
    paste0(pkg_dir, "\\data\\tblSlowAGToBGTransferRate.csv"),
    stringsAsFactors = F
  )
  transferRateSlowAGToBG <- signif(DOMTransferTable$SlowAGToBGTransferRate, 6)
  if (ForestType == "Softwood") {
    transferRateStemSnagToDOM <- signif(
      ecoBoundaryTable$SoftwoodStemSnagToDOM[
        ecoBoundaryTable$EcoBoundaryName ==
          as.character(crosswalkStratumState$EcoBoundaryId[i])
      ],
      6
    )
    transferRateBranchSnagToDOM <- signif(
      ecoBoundaryTable$SoftwoodBranchSnagToDOM[
        ecoBoundaryTable$EcoBoundaryName ==
          as.character(crosswalkStratumState$EcoBoundaryId[i])
      ],
      6
    )
  }
  if (ForestType == "Hardwood") {
    transferRateStemSnagToDOM <- signif(
      ecoBoundaryTable$HardwoodStemSnagToDOM[
        ecoBoundaryTable$EcoBoundaryName ==
          as.character(crosswalkStratumState$EcoBoundaryId[i])
      ],
      6
    )
    transferRateBranchSnagToDOM <- signif(
      ecoBoundaryTable$HardwoodBranchSnagToDOM[
        ecoBoundaryTable$EcoBoundaryName ==
          as.character(crosswalkStratumState$EcoBoundaryId[i])
      ],
      6
    )
  }
  # Get biomass turnover rates
  speciesTurnoverRatesTable <- read.csv(
    paste0(pkg_dir, "\\data\\tblSpeciesTypeDefault.csv"),
    stringsAsFactors = F
  )
  turnoverRates <- speciesTurnoverRatesTable[
    speciesTurnoverRatesTable$SpeciesTypeName ==
      as.character(crosswalkStratumState$SpeciesTypeId[i]),
  ]

  # Get the disturbance matrix information
  DMassociation = read.csv(
    paste0(pkg_dir, "\\data\\tblDMAssociationDefault.csv"),
    stringsAsFactors = F
  )
  DMassociation = DMassociation[
    DMassociation$DefaultEcoBoundaryID == ecoBoundaryId,
  ]

  disturbanceType = read.csv(
    paste0(pkg_dir, "\\data\\tblDisturbanceTypeDefault.csv"),
    stringsAsFactors = F
  )
  disturbanceMatrix <- read.csv(
    paste0(pkg_dir, "\\data\\tblDM.csv"),
    stringsAsFactors = F
  )

  dmValuesLookup = read.csv(
    paste0(pkg_dir, "\\data\\tblDMValuesLookup.csv"),
    stringsAsFactors = F
  )

  sourceName <- read.csv(
    paste0(pkg_dir, "\\data\\tblSourceName.csv"),
    stringsAsFactors = F
  ) %>%
    rename("DMRow" = "Row")
  sinkName <- read.csv(
    paste0(pkg_dir, "\\data\\tblSinkName.csv"),
    stringsAsFactors = F
  ) %>%
    rename("DMColumn" = "Column")

  # Disturbance Stuff -----------------------------------------------------------

  if ((doDisturbances == T) & (nrow(crosswalkDisturbance) > 0)) {
    #Need to rename DMassociation DistTypeID column
    names(DMassociation)[1] = "DistTypeID"

    df = DMassociation %>%
      left_join(disturbanceType, by = "DistTypeID") %>%
      select(DMID, DistTypeID, DistTypeName) %>%
      left_join(disturbanceMatrix, by = "DMID") %>%
      select(DMID, DistTypeID, DistTypeName, DMStructureID) %>%
      left_join(dmValuesLookup, by = "DMID") %>%
      left_join(sourceName, by = c("DMStructureID", "DMRow")) %>%
      rename("Source" = "Description") %>%
      left_join(sinkName, by = c("DMStructureID", "DMColumn")) %>%
      rename("Sink" = "Description") %>%
      mutate(Source = as.character(Source), Sink = as.character(Sink)) %>%
      filter(DistTypeName %in% crosswalkDisturbance$DisturbanceTypeId)

    # Fix typo in CBM database where "Hardwood Coarse Roots" is misspelled as "Hardwood Coarse roots"
    df[df == "Hardwood Coarse roots"] <- "Hardwood Coarse Roots"

    # discriminate between hardwood and softwood

    opposite <- ifelse(ForestType == "Softwood", "Hardwood", "Softwood")

    df_filtered <- df %>% filter(!str_detect(Source, opposite))

    sources <- data.frame(
      CBMSource = unique(df_filtered$Source),
      FromStockId = ""
    )

    sinks = data.frame(CBMSink = unique(df_filtered$Sink), ToStockId = "")

    transitions = data.frame(
      DistTypeName = unique(df_filtered$DistTypeName),
      TransitionTypeId = ""
    )

    #d = data.frame(CBMStocks = df_filtered$Source)
    #d1 = data.frame(CBMStocks = df_filtered$Sink)
    #d2 = bind_rows(d,d1)
    #d3 = data.frame(CBMStocks = unique(d2$CBMStocks), LUCASStocks = "")

    temp_crosswalkStock = datasheet(
      myScenario,
      name = "lucasbuilder_CrosswalkStock"
    ) #  temp_crosswalkStock <- datasheet(myLibrary, scenario = 8, name ="lucasbuilder_CrosswalkStock")

    # temp_crosswalkStock[16,1] <- "Products"

    temp_crosswalkDisturbance = datasheet(
      myScenario,
      name = "lucasbuilder_CrosswalkDisturbance"
    ) %>% # temp_crosswalkDisturbance <- datasheet(myLibrary, scenario = 7, name ="lucasbuilder_CrosswalkDisturbance") %>%
      mutate_if(is.factor, as.character)

    temp_pathways_df = df_filtered %>%
      left_join(temp_crosswalkStock, by = c("Source" = "CBMStock")) %>%
      rename("FromStockTypeId" = "StockTypeId") %>%
      left_join(temp_crosswalkStock, by = c("Sink" = "CBMStock")) %>%
      rename("ToStockTypeId" = "StockTypeId") %>%
      select(DistTypeName, FromStockTypeId, ToStockTypeId, Proportion) %>%
      mutate_if(is.factor, as.character) %>%
      left_join(
        temp_crosswalkDisturbance,
        by = c("DistTypeName" = "DisturbanceTypeId")
      ) %>%
      filter(!is.na(TransitionGroupId)) %>%
      filter(!is.na(FromStockTypeId)) %>%
      #mutate(FromStockTypeId = ifelse(FromStockTypeId != ToStockTypeId, FromStockTypeId, NA)) %>%
      filter(!is.na(FromStockTypeId)) %>%
      rename("Multiplier" = "Proportion") %>%
      mutate(FlowTypeId = "", Multiplier = round(Multiplier, 4)) %>%
      select(
        FromStockTypeId,
        ToStockTypeId,
        TransitionGroupId,
        DistTypeName,
        FlowTypeId,
        Multiplier
      )

    # head(temp_pathways_df)

    temp_pathways_df_clean <- temp_pathways_df %>% # mutate(FlowTypeId = pathways) %>%
      mutate(
        FromStratumId = the_stratum, # ToStratumId = the_stratum,
        FromSecondaryStratumId = the_secondarystratum, #ToSecondaryStratumId = the_secondarystratum,
        FromStateClassId = the_class #, ToStateClassId = the_class
      ) %>%
      select(-DistTypeName) %>%
      filter(FromStockTypeId != ToStockTypeId)

    for (r in 1:nrow(temp_pathways_df_clean)) {
      # r=1
      fStock <- temp_pathways_df_clean$FromStockTypeId[r]
      tstock <- temp_pathways_df_clean$ToStockTypeId[r]
      temp_pathways_df_clean$FlowTypeId[r] <- crossTFT$FlowType[which(
        crossTFT$FromStockType == fStock & crossTFT$ToStockType == tstock
      )]
    }

    pathways <- temp_pathways_df_clean$FlowTypeId
    pathways_all <- unique(c(pathways_all, pathways))

    final_pathways_df <- bind_rows(final_pathways_df, temp_pathways_df_clean)

    #write.csv(temp_pathways_df, file = "FlowPathways.csv")
  }

  # Get biomass turnover Proportions (not found in CBM database), taken from Kurtz et al. 2009
  if (ForestType == "Softwood") {
    proportionFoliageToAGVeryFast <- 1
  }
  if (ForestType == "Hardwood") {
    proportionFoliageToAGVeryFast <- 1
  }

  #####################################################
  # Volume to biomass
  # Parameters for equations in Boudewyn et al. 2007
  #####################################################

  if (useCBMAgeVsCarbonCurves == F) {
    A <- biomassExpansionTableRow %>% pull(A)
    B <- biomassExpansionTableRow %>% pull(B)

    a_nonmerch <- biomassExpansionTableRow %>% pull(a_nonmerch)
    b_nonmerch <- biomassExpansionTableRow %>% pull(b_nonmerch)
    k_nonmerch <- biomassExpansionTableRow %>% pull(k_nonmerch)
    cap_nonmerch <- biomassExpansionTableRow %>% pull(cap_nonmerch)

    a_sap <- biomassExpansionTableRow %>% pull(a_sap)
    b_sap <- biomassExpansionTableRow %>% pull(b_sap)
    k_sap <- biomassExpansionTableRow %>% pull(k_sap)
    cap_sap <- biomassExpansionTableRow %>% pull(cap_sap)

    a1 <- biomassExpansionTableRow %>% pull(a1)
    a2 <- biomassExpansionTableRow %>% pull(a2)
    a3 <- biomassExpansionTableRow %>% pull(a3)

    b1 <- biomassExpansionTableRow %>% pull(b1)
    b2 <- biomassExpansionTableRow %>% pull(b2)
    b3 <- biomassExpansionTableRow %>% pull(b3)

    c1 <- biomassExpansionTableRow %>% pull(c1)
    c2 <- biomassExpansionTableRow %>% pull(c2)
    c3 <- biomassExpansionTableRow %>% pull(c3)

    minVolume <- biomassExpansionTableRow %>% pull(min_volume)
    maxVolume <- biomassExpansionTableRow %>% pull(max_volume)

    # Grab the proportion of the stem that is top or stump by admin region
    adminBoundaryTableSub <- adminBoundaryTable %>%
      filter(AdminBoundaryID == adminBoundaryId)

    propTop <- adminBoundaryTableSub %>%
      pull(paste0(forestType, "TopProportion")) /
      100
    propStump <- adminBoundaryTableSub %>%
      pull(paste0(forestType, "StumpProportion")) /
      100

    # Set up dataframe to hold results
    volumeToCarbon <- data.frame(
      age = grossMerchantableVolume$Age,
      volume = grossMerchantableVolume$MerchantableVolume
    )

    # Total stem wood biomass/ha for live, merchantable size trees
    # b_m = total stem wood biomass of merchantable-sized live trees (biomass includes stumps and tops), in metric tonnes per ha
    volumeToCarbon$b_m <- A * volumeToCarbon$volume^B

    # Total stem wood biomass/ha for live, non-merchantable size trees
    # Nonmerchantable expansion factor
    volumeToCarbon$nonmerchfactor <- k_nonmerch +
      a_nonmerch * volumeToCarbon$b_m^b_nonmerch

    # Add Cap
    volumeToCarbon$nonmerchfactor <- ifelse(
      volumeToCarbon$nonmerchfactor > cap_nonmerch,
      cap_nonmerch,
      volumeToCarbon$nonmerchfactor
    )

    # b_nm = stem wood biomass of live, merchantable and nonmerchantable-sized trees (tonnes/ha)
    volumeToCarbon$b_nm <- volumeToCarbon$nonmerchfactor * volumeToCarbon$b_m
    # b_n = stem wood biomass of live, nonmerchantable-sized trees (tonnes/ha)
    volumeToCarbon$b_n <- (volumeToCarbon$nonmerchfactor * volumeToCarbon$b_m) -
      volumeToCarbon$b_m

    # Total stem wood biomass/ha for live sapling size trees
    #Sapling expansion factor
    volumeToCarbon$saplingfactor <- k_sap + a_sap * (volumeToCarbon$b_nm^b_sap)

    # Add Cap
    volumeToCarbon$saplingfactor <- ifelse(
      volumeToCarbon$saplingfactor > cap_sap,
      cap_sap,
      volumeToCarbon$saplingfactor
    )

    # b_s = stem wood biomass of live, sapling-sized trees (tonnes/ha)
    volumeToCarbon$b_s <- (volumeToCarbon$saplingfactor * volumeToCarbon$b_nm) -
      volumeToCarbon$b_nm

    # Total stemwood of all live trees
    volumeToCarbon$b_sw <- volumeToCarbon$b_m +
      volumeToCarbon$b_n +
      volumeToCarbon$b_s

    # Compute proportions
    volumeToCarbon$denominator <- (1 +
      exp(
        a1 + a2 * volumeToCarbon$volume + a3 * log(volumeToCarbon$volume + 5)
      ) +
      exp(
        b1 + b2 * volumeToCarbon$volume + b3 * log(volumeToCarbon$volume + 5)
      ) +
      exp(
        c1 + c2 * volumeToCarbon$volume + c3 * log(volumeToCarbon$volume + 5)
      ))

    # Stem wood proportion
    volumeToCarbon$p_stemwood <- 1 / volumeToCarbon$denominator

    # Stem bark proportion
    volumeToCarbon$p_bark <- exp(
      a1 + a2 * volumeToCarbon$volume + a3 * log(volumeToCarbon$volume + 5)
    ) /
      volumeToCarbon$denominator

    # Branches proportion
    volumeToCarbon$p_branches <- exp(
      b1 + b2 * volumeToCarbon$volume + b3 * log(volumeToCarbon$volume + 5)
    ) /
      volumeToCarbon$denominator

    # Foliage proportion
    volumeToCarbon$p_foliage <- exp(
      c1 + c2 * volumeToCarbon$volume + c3 * log(volumeToCarbon$volume + 5)
    ) /
      volumeToCarbon$denominator

    #Total tree biomass/ha live
    volumeToCarbon$b <- volumeToCarbon$b_sw / volumeToCarbon$p_stemwood

    # Biomass based on b (total biomass of live trees)
    volumeToCarbon$b_bark <- volumeToCarbon$b * volumeToCarbon$p_bark
    volumeToCarbon$b_branches <- volumeToCarbon$b * volumeToCarbon$p_branches
    volumeToCarbon$b_foliage <- volumeToCarbon$b * volumeToCarbon$p_foliage
    #volumeToCarbon$b_other <- volumeToCarbon$b_bark + volumeToCarbon$b_branches + volumeToCarbon$b_n + volumeToCarbon$b_s

    # Partition bark on the merchantable stem or the nonmerchantable/sapling trees
    volumeToCarbon$b_bark_m <- volumeToCarbon$b_bark *
      (volumeToCarbon$b_m / volumeToCarbon$b_sw)
    volumeToCarbon$b_bark_ns <- volumeToCarbon$b_bark *
      ((volumeToCarbon$b_n + volumeToCarbon$b_s) / volumeToCarbon$b_sw)

    # Merchantable in CBM includes bark but does not include the top and stump
    volumeToCarbon$b_m_CBM <- (volumeToCarbon$b_m * (1 - propTop - propStump)) +
      (volumeToCarbon$b_bark_m * (1 - propTop - propStump))

    # The other pool includes the remaining biomass pools, including the stumps and tops
    volumeToCarbon$b_other <- (volumeToCarbon$b_m * (propTop + propStump)) +
      (volumeToCarbon$b_bark_m * (propTop + propStump)) +
      volumeToCarbon$b_branches +
      volumeToCarbon$b_n +
      volumeToCarbon$b_s +
      volumeToCarbon$b_bark_ns

    #Replace NoN values with 0's
    is.nan.data.frame <- function(x) {
      do.call(cbind, lapply(x, is.nan))
    }

    volumeToCarbon[is.nan.data.frame(volumeToCarbon)] <- 0

    # smoothing functions

    # Smooth foliage
    min1 <- max(volumeToCarbon$age[volumeToCarbon$volume <= minVolume])

    minF2 <- volumeToCarbon$age[which(
      diff(sign(diff(volumeToCarbon$b_foliage))) == 2
    )]

    if (length(minF2) == 0) {
      minF <- min1
    } else {
      minF <- max(min1, minF2)
    }

    volumeToCarbon = volumeToCarbon %>%
      arrange(age) %>%
      mutate(b_foliageSmooth = if_else(age <= minF, NA, b_foliage)) %>%
      mutate(b_foliageSmooth = if_else(age == 0, 0, b_foliageSmooth)) %>%
      mutate(
        b_foliageSmooth = na_interpolation(
          b_foliageSmooth,
          option = "spline",
          method = "natural"
        )
      )

    # Smooth other wood

    minO2 <- volumeToCarbon$age[which(
      diff(sign(diff(volumeToCarbon$b_other))) == 2
    )]

    if (length(minO2) == 0) {
      minO <- min1
    } else {
      minO <- max(min1, minO2)
    }

    volumeToCarbon = volumeToCarbon %>%
      arrange(age) %>%
      mutate(b_otherSmooth = if_else(age <= minO, NA, b_other)) %>%
      mutate(b_otherSmooth = if_else(age == 0, 0, b_otherSmooth)) %>%
      mutate(
        b_otherSmooth = na_interpolation(
          b_otherSmooth,
          option = "spline",
          method = "natural"
        )
      )

    # Smooth merchantable

    minM2 <- volumeToCarbon$age[which(
      diff(sign(diff(volumeToCarbon$b_m_CBM))) == 2
    )]

    if (length(minM2) == 0) {
      minM <- min1
    } else {
      minM <- max(min1, minM2)
    }

    volumeToCarbon = volumeToCarbon %>%
      arrange(age) %>%
      mutate(b_mSmooth = if_else(age <= minM, NA, b_m_CBM)) %>%
      mutate(b_mSmooth = if_else(age == 0, 0, b_mSmooth)) %>%
      mutate(
        b_mSmooth = na_interpolation(
          b_mSmooth,
          option = "spline",
          method = "natural"
        )
      )

    volumeToCarbon <- volumeToCarbon %>%
      mutate(b_aboveground = b_mSmooth + b_foliageSmooth + b_otherSmooth)

    ## Biomass to carbon
    isSoftwood <- if (forestType == "Softwood") 1 else 0
    volumeToCarbon$c_aboveground <- volumeToCarbon$b_aboveground *
      biomassToCarbonTable[
        biomassToCarbonTable$BiomassComponentName == "Other biomass component" &
          biomassToCarbonTable$Softwood == isSoftwood,
        "Multiplier"
      ]
    volumeToCarbon$c_other <- volumeToCarbon$b_otherSmooth *
      biomassToCarbonTable[
        biomassToCarbonTable$BiomassComponentName == "Other biomass component" &
          biomassToCarbonTable$Softwood == isSoftwood,
        "Multiplier"
      ]
    volumeToCarbon$c_foliage <- volumeToCarbon$b_foliageSmooth *
      biomassToCarbonTable[
        biomassToCarbonTable$BiomassComponentName ==
          "Foliage biomass component" &
          biomassToCarbonTable$Softwood == isSoftwood,
        "Multiplier"
      ]
    volumeToCarbon$c_m <- volumeToCarbon$b_mSmooth *
      biomassToCarbonTable[
        biomassToCarbonTable$BiomassComponentName ==
          "Merchantable biomass component" &
          biomassToCarbonTable$Softwood == isSoftwood,
        "Multiplier"
      ]
  }

  ###################################################
  # Volume to belowground biomass carbon conversion
  # Parameters from Li et al. 2003
  ###################################################

  if (useCBMAgeVsCarbonCurves == F) {
    if (forestType == "Softwood") {
      volumeToCarbon$b_roots <- 0.222 * volumeToCarbon$b_aboveground
    } else {
      volumeToCarbon$b_roots <- 1.576 * volumeToCarbon$b_aboveground^0.615
    }
    #if(isSoftwood) {volumeToCarbon$b_roots <- 0.2222 * volumeToCarbon$b_aboveground} else {volumeToCarbon$b_roots <- 1.576 * volumeToCarbon$b_aboveground ^ 0.615}
    volumeToCarbon$p_fineroots <- 0.072 +
      0.354 * exp(-0.060 * volumeToCarbon$b_roots)
    volumeToCarbon$b_fineroots <- volumeToCarbon$p_fineroots *
      volumeToCarbon$b_roots
    volumeToCarbon$b_coarseroots <- volumeToCarbon$b_roots -
      volumeToCarbon$b_fineroots
    # Add in hardwood equation

    ## Biomass to carbon
    isSoftwood <- if (forestType == "Softwood") 1 else 0
    volumeToCarbon$c_fineroots <- volumeToCarbon$b_fineroots *
      biomassToCarbonTable[
        biomassToCarbonTable$BiomassComponentName ==
          "Fine root biomass component" &
          biomassToCarbonTable$Softwood == isSoftwood,
        "Multiplier"
      ]
    volumeToCarbon$c_coarseroots <- volumeToCarbon$b_coarseroots *
      biomassToCarbonTable[
        biomassToCarbonTable$BiomassComponentName ==
          "Coarse root biomass component" &
          biomassToCarbonTable$Softwood == isSoftwood,
        "Multiplier"
      ]
    volumeToCarbon$c_belowground <- volumeToCarbon$c_fineroots +
      volumeToCarbon$c_coarseroots
  }

  #####################################################
  # Biomass Turnover and DOM Decay and Transfer rates #
  #####################################################
  # DOM transfer rates
  DOMTransferFlows[
    DOMTransferFlows$FromStockTypeId == crossSF("Aboveground Slow DOM") &
      DOMTransferFlows$ToStockTypeId == crossSF("Belowground Slow DOM"),
    "Multiplier"
  ] <- transferRateSlowAGToBG

  if (ForestType == "Softwood") {
    DOMTransferFlows[
      DOMTransferFlows$FromStockTypeId == crossSF("Softwood Stem Snag") &
        DOMTransferFlows$ToStockTypeId == crossSF("Aboveground Medium DOM"),
      "Multiplier"
    ] <- transferRateStemSnagToDOM
    DOMTransferFlows[
      DOMTransferFlows$FromStockTypeId == crossSF("Softwood Branch Snag") &
        DOMTransferFlows$ToStockTypeId == crossSF("Aboveground Fast DOM"),
      "Multiplier"
    ] <- transferRateBranchSnagToDOM
  }
  if (ForestType == "Hardwood") {
    DOMTransferFlows[
      DOMTransferFlows$FromStockTypeId == crossSF("Hardwood Stem Snag") &
        DOMTransferFlows$ToStockTypeId == crossSF("Aboveground Medium DOM"),
      "Multiplier"
    ] <- transferRateStemSnagToDOM
    DOMTransferFlows[
      DOMTransferFlows$FromStockTypeId == crossSF("Hardwood Branch Snag") &
        DOMTransferFlows$ToStockTypeId == crossSF("Aboveground Fast DOM"),
      "Multiplier"
    ] <- transferRateBranchSnagToDOM
  }

  DOMTable <- rbind(DOMTable[, names(DOMTransferFlows)], DOMTransferFlows)

  # Biomass turnover rates
  turnOverRateStemAnnual <- signif(
    ecoBoundaryTable$StemAnnualTurnOverRate[
      ecoBoundaryTable$EcoBoundaryName ==
        as.character(crosswalkStratumState$EcoBoundaryId[i])
    ],
    6
  )
  turnOverRateFineRootsAGVeryFast <- signif(
    turnoverRates$FineRootTurnPropSlope,
    6
  )
  turnOverRateFineRootsBGVeryFast <- signif(
    turnoverRates$FineRootTurnPropSlope,
    6
  )
  turnOverRateCoarseRootsAGFast <- signif(turnoverRates$CoarseRootTurnProp, 6)
  turnOverRateCoarseRootsBGFast <- signif(turnoverRates$CoarseRootTurnProp, 6)
  if (ForestType == "Softwood") {
    turnOverRateBranch <- signif(
      ecoBoundaryTable$SoftwoodBranchTurnOverRate[
        ecoBoundaryTable$EcoBoundaryName ==
          as.character(crosswalkStratumState$EcoBoundaryId[i])
      ],
      6
    )
    turnOverRateFoliage <- signif(
      ecoBoundaryTable$SoftwoodFoliageFallRate[
        ecoBoundaryTable$EcoBoundaryName ==
          as.character(crosswalkStratumState$EcoBoundaryId[i])
      ],
      6
    )
  }
  if (ForestType == "Hardwood") {
    turnOverRateBranch <- signif(
      ecoBoundaryTable$HardwoodBranchTurnOverRate[
        ecoBoundaryTable$EcoBoundaryName ==
          as.character(crosswalkStratumState$EcoBoundaryId[i])
      ],
      6
    )
    turnOverRateFoliage <- signif(
      ecoBoundaryTable$HardwoodFoliageFallRate[
        ecoBoundaryTable$EcoBoundaryName ==
          as.character(crosswalkStratumState$EcoBoundaryId[i])
      ],
      6
    )
  }

  # Turnover proportions
  proportionOtherToBranchSnag <- signif(turnoverRates$BranchesToBranchSnag, 6)
  proportionOtherToAGFast <- 1 - proportionOtherToBranchSnag

  biomassTurnoverTable <- biomassTurnoverFlows
  biomassTurnoverTable$Multiplier[
    biomassTurnoverTable$FromStockTypeId == crossSF("Merchantable")
  ] <- turnOverRateStemAnnual * proportionMerchantableToSnag
  if (ForestType == "Softwood") {
    biomassTurnoverTable$Multiplier[
      biomassTurnoverTable$FromStockTypeId == crossSF("Other") &
        biomassTurnoverTable$ToStockTypeId == crossSF("Softwood Branch Snag")
    ] <- turnOverRateBranch * proportionOtherToBranchSnag
  }
  if (ForestType == "Hardwood") {
    biomassTurnoverTable$Multiplier[
      biomassTurnoverTable$FromStockTypeId == crossSF("Other") &
        biomassTurnoverTable$ToStockTypeId == crossSF("Hardwood Branch Snag")
    ] <- turnOverRateBranch * proportionOtherToBranchSnag
  }
  biomassTurnoverTable$Multiplier[
    biomassTurnoverTable$FromStockTypeId == crossSF("Other") &
      biomassTurnoverTable$ToStockTypeId == crossSF("Aboveground Fast DOM")
  ] <- turnOverRateBranch * proportionOtherToAGFast
  biomassTurnoverTable$Multiplier[
    biomassTurnoverTable$FromStockTypeId == crossSF("Foliage")
  ] <- turnOverRateFoliage * proportionFoliageToAGVeryFast
  biomassTurnoverTable$Multiplier[
    biomassTurnoverTable$FromStockTypeId == crossSF("Fine Roots") &
      biomassTurnoverTable$ToStockTypeId == crossSF("Aboveground Very Fast DOM")
  ] <- turnOverRateFineRootsAGVeryFast * proportionFineRootsToAGVeryFast
  biomassTurnoverTable$Multiplier[
    biomassTurnoverTable$FromStockTypeId == crossSF("Fine Roots") &
      biomassTurnoverTable$ToStockTypeId == crossSF("Belowground Very Fast DOM")
  ] <- turnOverRateFineRootsBGVeryFast * proportionFineRootsToBGVeryFast
  biomassTurnoverTable$Multiplier[
    biomassTurnoverTable$FromStockTypeId == crossSF("Coarse Roots") &
      biomassTurnoverTable$ToStockTypeId == crossSF("Aboveground Fast DOM")
  ] <- turnOverRateCoarseRootsAGFast * proportionCoarseRootsToAGFast
  biomassTurnoverTable$Multiplier[
    biomassTurnoverTable$FromStockTypeId == crossSF("Coarse Roots") &
      biomassTurnoverTable$ToStockTypeId == crossSF("Belowground Fast DOM")
  ] <- turnOverRateCoarseRootsBGFast * proportionCoarseRootsToBGFast

  ########################################################
  # Calculate net growth based on mass-balance equations #
  ########################################################

  # # Original approach using CBM Output. Remove eventually...
  # if(useCBMAgeVsCarbonCurves==T){
  #   stateAttributeValues <- datasheet(myScenario, "stsim_StateAttributeValue", empty=FALSE, optional=TRUE) #  stateAttributeValues <- datasheet(myLibrary, scenario = 12, "stsim_StateAttributeValue", empty=FALSE, optional=TRUE)
  #   stateAttributeValuesWide <- spread(stateAttributeValues, key="StateAttributeTypeId", value = "Value") %>%
  #     mutate_if(is.factor, as.character)
  #   carbonInitialConditions <- datasheet(myScenario, "stsim_InitialStockNonSpatial", empty=FALSE, optional=TRUE) # carbonInitialConditions <- datasheet(myLibrary, scenario = 5, "stsimsf_InitialStockNonSpatial", empty=FALSE, optional=TRUE)
  #
  #   if(!is.na(crosswalkStratumState$StratumId[i]) & !is.na(crosswalkStratumState$SecondaryStratumId[i])){
  #     volumeToCarbon <- filter(stateAttributeValuesWide, StratumId == as.character(crosswalkStratumState$StratumId[i]) &  SecondaryStratumId == as.character(crosswalkStratumState$SecondaryStratumId[i]) & StateClassId == as.character(crosswalkStratumState$StateClassId[i])) }
  #   if(!is.na(crosswalkStratumState$StratumId[i]) & is.na(crosswalkStratumState$SecondaryStratumId[i])){
  #     volumeToCarbon <- filter(stateAttributeValuesWide, StratumId == as.character(crosswalkStratumState$StratumId[i]) & StateClassId == as.character(crosswalkStratumState$StateClassId[i])) }
  #   if(is.na(crosswalkStratumState$StratumId[i]) & is.na(crosswalkStratumState$SecondaryStratumId[i])){
  #    volumeToCarbon <- filter(stateAttributeValuesWide, StateClassId == crosswalkStratumState$StateClassId[i])}
  #
  #   volumeToCarbon$c_m <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeId[carbonInitialConditions$StockTypeId == crossSF("Merchantable")])]
  #   volumeToCarbon$c_foliage <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeId[carbonInitialConditions$StockTypeId == crossSF("Foliage")])]
  #   volumeToCarbon$c_other <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeId[carbonInitialConditions$StockTypeId == crossSF("Other")])]
  #   volumeToCarbon$c_fineroots <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeId[carbonInitialConditions$StockTypeId == crossSF("Fine Roots")])]
  #   volumeToCarbon$c_coarseroots <- volumeToCarbon[, as.character(carbonInitialConditions$StateAttributeTypeId[carbonInitialConditions$StockTypeId == crossSF("Coarse Roots")])]
  #
  # }

  volumeToCarbon$c_m1 <- c(volumeToCarbon$c_m[2:nrow(volumeToCarbon)], NA)
  volumeToCarbon$c_foliage1 <- c(
    volumeToCarbon$c_foliage[2:nrow(volumeToCarbon)],
    NA
  )
  volumeToCarbon$c_other1 <- c(
    volumeToCarbon$c_other[2:nrow(volumeToCarbon)],
    NA
  )
  volumeToCarbon$c_fineroots1 <- c(
    volumeToCarbon$c_fineroots[2:nrow(volumeToCarbon)],
    NA
  )
  volumeToCarbon$c_coarseroots1 <- c(
    volumeToCarbon$c_coarseroots[2:nrow(volumeToCarbon)],
    NA
  )

  # Growth happens after biomass transfer
  if (ForestType == "Softwood") {
    volumeToCarbon$g_m <- volumeToCarbon$c_m1 -
      (1 -
        biomassTurnoverTable$Multiplier[
          biomassTurnoverTable$FromStockTypeId == crossSF("Merchantable") &
            biomassTurnoverTable$ToStockTypeId == crossSF("Softwood Stem Snag")
        ]) *
        volumeToCarbon$c_m
  }
  if (ForestType == "Hardwood") {
    volumeToCarbon$g_m <- volumeToCarbon$c_m1 -
      (1 -
        biomassTurnoverTable$Multiplier[
          biomassTurnoverTable$FromStockTypeId == crossSF("Merchantable") &
            biomassTurnoverTable$ToStockTypeId == crossSF("Hardwood Stem Snag")
        ]) *
        volumeToCarbon$c_m
  }
  volumeToCarbon$g_foliage <- volumeToCarbon$c_foliage1 -
    (1 -
      biomassTurnoverTable$Multiplier[
        biomassTurnoverTable$FromStockTypeId == crossSF("Foliage") &
          biomassTurnoverTable$ToStockTypeId ==
            crossSF("Aboveground Very Fast DOM")
      ]) *
      volumeToCarbon$c_foliage
  if (ForestType == "Softwood") {
    volumeToCarbon$g_other <- volumeToCarbon$c_other1 -
      (1 -
        biomassTurnoverTable$Multiplier[
          biomassTurnoverTable$FromStockTypeId == crossSF("Other") &
            biomassTurnoverTable$ToStockTypeId ==
              crossSF("Aboveground Fast DOM")
        ] -
        biomassTurnoverTable$Multiplier[
          biomassTurnoverTable$FromStockTypeId == crossSF("Other") &
            biomassTurnoverTable$ToStockTypeId ==
              crossSF("Softwood Branch Snag")
        ]) *
        volumeToCarbon$c_other
  }
  if (ForestType == "Hardwood") {
    volumeToCarbon$g_other <- volumeToCarbon$c_other1 -
      (1 -
        biomassTurnoverTable$Multiplier[
          biomassTurnoverTable$FromStockTypeId == crossSF("Other") &
            biomassTurnoverTable$ToStockTypeId ==
              crossSF("Aboveground Fast DOM")
        ] -
        biomassTurnoverTable$Multiplier[
          biomassTurnoverTable$FromStockTypeId == crossSF("Other") &
            biomassTurnoverTable$ToStockTypeId ==
              crossSF("Hardwood Branch Snag")
        ]) *
        volumeToCarbon$c_other
  }
  volumeToCarbon$g_fineroots <- volumeToCarbon$c_fineroots1 -
    (1 -
      biomassTurnoverTable$Multiplier[
        biomassTurnoverTable$FromStockTypeId == crossSF("Fine Roots") &
          biomassTurnoverTable$ToStockTypeId ==
            crossSF("Aboveground Very Fast DOM")
      ] -
      biomassTurnoverTable$Multiplier[
        biomassTurnoverTable$FromStockTypeId == crossSF("Fine Roots") &
          biomassTurnoverTable$ToStockTypeId ==
            crossSF("Belowground Very Fast DOM")
      ]) *
      volumeToCarbon$c_fineroots
  volumeToCarbon$g_coarseroots <- volumeToCarbon$c_coarseroots1 -
    (1 -
      biomassTurnoverTable$Multiplier[
        biomassTurnoverTable$FromStockTypeId == crossSF("Coarse Roots") &
          biomassTurnoverTable$ToStockTypeId == crossSF("Aboveground Fast DOM")
      ] -
      biomassTurnoverTable$Multiplier[
        biomassTurnoverTable$FromStockTypeId == crossSF("Coarse Roots") &
          biomassTurnoverTable$ToStockTypeId == crossSF("Belowground Fast DOM")
      ]) *
      volumeToCarbon$c_coarseroots
  volumeToCarbon$g_all <- volumeToCarbon$g_m +
    volumeToCarbon$g_foliage +
    volumeToCarbon$g_other +
    volumeToCarbon$g_fineroots +
    volumeToCarbon$g_coarseroots
  volumeToCarbon <- volumeToCarbon[1:(nrow(volumeToCarbon) - 1), ]

  # Replace NaN values with 0's
  volumeToCarbon[is.nan.data.frame(volumeToCarbon)] <- 0
  volumeToCarbon <- volumeToCarbon %>% mutate_if(is.factor, as.character)

  volumeToCarbon$StratumId <- the_stratum
  volumeToCarbon$SecondaryStratumId <- the_secondarystratum
  volumeToCarbon$StateClassId <- the_class

  volumeToCarbon <- cbind(
    volumeToCarbon[, c("StratumId", "SecondaryStratumId", "StateClassId")],
    do.call(
      data.frame,
      lapply(
        volumeToCarbon[,
          !(names(volumeToCarbon) %in%
            c("StratumId", "SecondaryStratumId", "StateClassId"))
        ],
        function(x) replace(x, is.infinite(x), 0)
      )
    )
  )

  #######################
  # STSim-SF datasheets #
  #######################
  # State Attribute Values for net growth based on mass-balance equations
  stateAttributesNetGrowth = datasheet(
    myScenario,
    name = "stsim_StateAttributeValue",
    empty = T,
    optional = T,
    lookupsAsFactors = F
  )
  stateAttributesNetGrowth[
    1:nrow(volumeToCarbon),
    "StratumId"
  ] <- as.character(volumeToCarbon$StratumId[1:nrow(volumeToCarbon)])
  stateAttributesNetGrowth[
    1:nrow(volumeToCarbon),
    "SecondaryStratumId"
  ] <- as.character(volumeToCarbon$SecondaryStratumId[1:nrow(volumeToCarbon)])
  stateAttributesNetGrowth[
    1:nrow(volumeToCarbon),
    "StateClassId"
  ] <- as.character(volumeToCarbon$StateClassId[1:nrow(volumeToCarbon)])
  stateAttributesNetGrowth[
    1:nrow(volumeToCarbon),
    "StateAttributeTypeId"
  ] <- rep(
    as.character(flowPathways$StateAttributeTypeId[
      (flowPathways$FromStockTypeId == crossSF("Atmosphere") &
        flowPathways$ToStockTypeId == crossSF("Merchantable"))
    ]),
    nrow(volumeToCarbon)
  )
  stateAttributesNetGrowth[
    1:nrow(volumeToCarbon),
    "AgeMin"
  ] <- volumeToCarbon$age[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[
    1:nrow(volumeToCarbon),
    "AgeMax"
  ] <- volumeToCarbon$age[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[
    1:nrow(volumeToCarbon),
    "Value"
  ] <- volumeToCarbon$g_all[1:nrow(volumeToCarbon)]
  stateAttributesNetGrowth[nrow(volumeToCarbon), "AgeMax"] <- NA

  stateAttributesNetGrowthMaster = rbind(
    stateAttributesNetGrowth,
    stateAttributesNetGrowthMaster
  )

  # SF Flow Pathways
  # Flow Multipliers for biomass net growth based on volume-to-carbon proportions
  flowMultiplierNetGrowth <- datasheet(
    myScenario,
    name = "stsim_FlowMultiplier",
    empty = T,
    optional = T,
    lookupsAsFactors = F
  )
  flowMultiplierNetGrowth[
    1:(nrow(volumeToCarbon) * numBiomassStocks),
    "StratumId"
  ] <- as.character(crosswalkStratumState$StratumId[i])
  flowMultiplierNetGrowth[
    1:(nrow(volumeToCarbon) * numBiomassStocks),
    "SecondaryStratumId"
  ] <- as.character(crosswalkStratumState$SecondaryStratumId[i])
  flowMultiplierNetGrowth[
    1:(nrow(volumeToCarbon) * numBiomassStocks),
    "StateClassId"
  ] <- as.character(crosswalkStratumState$StateClassId[i])
  flowMultiplierNetGrowth[
    1:(nrow(volumeToCarbon) * numBiomassStocks),
    "AgeMin"
  ] <- rep(volumeToCarbon$age[1:nrow(volumeToCarbon)], numBiomassStocks)
  flowMultiplierNetGrowth[
    1:(nrow(volumeToCarbon) * numBiomassStocks),
    "AgeMax"
  ] <- rep(stateAttributesNetGrowth$AgeMax, numBiomassStocks)
  flowMultiplierNetGrowth[
    1:(nrow(volumeToCarbon) * numBiomassStocks),
    "FlowGroupId"
  ] <- c(
    rep(
      paste0(
        as.character(flowPathways$FlowTypeId[
          (flowPathways$FromStockTypeId == crossSF("Atmosphere") &
            flowPathways$ToStockTypeId == crossSF("Merchantable"))
        ]),
        " [Type]"
      ),
      nrow(volumeToCarbon)
    ),
    rep(
      paste0(
        as.character(flowPathways$FlowTypeId[
          (flowPathways$FromStockTypeId == crossSF("Atmosphere") &
            flowPathways$ToStockTypeId == crossSF("Other"))
        ]),
        " [Type]"
      ),
      nrow(volumeToCarbon)
    ),
    rep(
      paste0(
        as.character(flowPathways$FlowTypeId[
          (flowPathways$FromStockTypeId == crossSF("Atmosphere") &
            flowPathways$ToStockTypeId == crossSF("Foliage"))
        ]),
        " [Type]"
      ),
      nrow(volumeToCarbon)
    ),
    rep(
      paste0(
        as.character(flowPathways$FlowTypeId[
          (flowPathways$FromStockTypeId == crossSF("Atmosphere") &
            flowPathways$ToStockTypeId == crossSF("Fine Roots"))
        ]),
        " [Type]"
      ),
      nrow(volumeToCarbon)
    ),
    rep(
      paste0(
        as.character(flowPathways$FlowTypeId[
          (flowPathways$FromStockTypeId == crossSF("Atmosphere") &
            flowPathways$ToStockTypeId == crossSF("Coarse Roots"))
        ]),
        " [Type]"
      ),
      nrow(volumeToCarbon)
    )
  )
  flowMultiplierNetGrowth[
    1:(nrow(volumeToCarbon) * numBiomassStocks),
    "Value"
  ] <- c(
    volumeToCarbon$g_m[1:nrow(volumeToCarbon)] /
      volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
    volumeToCarbon$g_other[1:nrow(volumeToCarbon)] /
      volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
    volumeToCarbon$g_foliage[1:nrow(volumeToCarbon)] /
      volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
    volumeToCarbon$g_fineroots[1:nrow(volumeToCarbon)] /
      volumeToCarbon$g_all[1:nrow(volumeToCarbon)],
    volumeToCarbon$g_coarseroots[1:nrow(volumeToCarbon)] /
      volumeToCarbon$g_all[1:nrow(volumeToCarbon)]
  )
  #flowMultiplierNetGrowth[flowMultiplierNetGrowth$AgeMin == volumeToCarbon$AgeMin[nrow(volumeToCarbon)], "AgeMax"] <- NA
  #flowMultiplierNetGrowth[is.nan.data.frame(flowMultiplierNetGrowth)] <- 0

  #Flow Pathways for biomass turnover rates and DOM transfer and decay rates
  flowPathwayTable <- rbind(
    biomassTurnoverTable,
    DOMTable[, names(biomassTurnoverTable)]
  )
  flowMultiplierTurnoverTransferDecayEmission <- datasheet(
    myScenario,
    name = "stsim_FlowMultiplier",
    empty = T,
    optional = T,
    lookupsAsFactors = F
  )
  flowMultiplierTurnoverTransferDecayEmission[
    1:nrow(flowPathwayTable),
    "StratumId"
  ] <- as.character(crosswalkStratumState$StratumId[i])
  flowMultiplierTurnoverTransferDecayEmission[
    1:nrow(flowPathwayTable),
    "SecondaryStratumId"
  ] <- as.character(crosswalkStratumState$SecondaryStratumId[i])
  flowMultiplierTurnoverTransferDecayEmission[
    1:nrow(flowPathwayTable),
    "StateClassId"
  ] <- as.character(crosswalkStratumState$StateClassId[i])
  flowMultiplierTurnoverTransferDecayEmission[
    1:nrow(flowPathwayTable),
    "FlowGroupId"
  ] = paste0(flowPathwayTable$FlowTypeId, " [Type]")
  flowMultiplierTurnoverTransferDecayEmission[
    1:nrow(flowPathwayTable),
    "Value"
  ] = flowPathwayTable$Multiplier

  # Combine all flow multipliers
  flowMultiplierAll <- rbind(
    flowMultiplierNetGrowth,
    flowMultiplierTurnoverTransferDecayEmission
  )
  flowMultiplierMaster <- rbind(flowMultiplierAll, flowMultiplierMaster)

  # } # end VolumeToCarbon if statement
}

# Save flow pathways to scenario
final_pathways_df_unique <- final_pathways_df %>%
  mutate_if(is.factor, as.character) %>%
  bind_rows(flowPathways) %>%
  unique()
saveDatasheet(
  myScenario,
  final_pathways_df_unique,
  name = "stsim_FlowPathway",
  append = FALSE
)

# remove rows that are all NAs
stateAttributesNetGrowthMaster <- stateAttributesNetGrowthMaster %>%
  filter(
    rowSums(is.na(stateAttributesNetGrowthMaster)) !=
      ncol(stateAttributesNetGrowthMaster)
  )

flowMultiplierMaster <- flowMultiplierMaster %>%
  filter(rowSums(is.na(flowMultiplierMaster)) != ncol(flowMultiplierMaster))

saveDatasheet(
  myScenario,
  stateAttributesNetGrowthMaster,
  name = "stsim_StateAttributeValue",
  append = TRUE
)
saveDatasheet(
  myScenario,
  flowMultiplierMaster,
  name = "stsim_FlowMultiplier",
  append = T
)
