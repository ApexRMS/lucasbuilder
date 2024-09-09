###################################################
# Volume to aboveground biomass carbon conversion #
# Created by Bronwyn Rayfield in 2019             #
# Updated by Amanda Schwantes Sep, 2024, to run   #
#   independently from the package designer       #
# Aug 26, 2024, updated validation dataset        #
# Sep 9, 2024, tested with more forest types      #
###################################################

library(rsyncrosim)
library(RODBC)
library(tidyverse)
library(imputeTS)

old <- options(pillar.sigfig = 10)

# Specify file paths
rootPath <- "C:/Users/AmandaSchwantes/Documents/GitHub/lucasbuilder/"

pathToCurves <- paste0(rootPath,"data/user-example-inputs-merch/")

pathToCBMoutput <- paste0(rootPath,"data/user-example-inputs-merch/cbm-cfs3-simulation-results/")

pathToInputTables <- paste0(rootPath,"src/data/")

pathToSaveFigures <- paste0(rootPath,"output/FiguresVolumeToBiomass/")

pathToCrosswalkStratumState <- paste0(rootPath, "data/user-example-inputs-merch/")

useCBMAgeVsCarbonCurves <- F

# Loop through each row (state class) in crosswalk table
crosswalkStratumState <- read.csv(paste0(pathToCrosswalkStratumState,"Crosswalk - Spatial Unit and Species Type.csv"),
                                  stringsAsFactors = F)

grossMerchantableVolumeAll <- read.csv(paste0(pathToCurves,"MerchantableVolumeCurves.csv"),
                                       stringsAsFactors = F)

for (i in 1:nrow(crosswalkStratumState)){
  
  # Merchantable volume curve data
  grossMerchantableVolume <- grossMerchantableVolumeAll %>%
    filter(StateClassId == crosswalkStratumState$StateClassId[i]) %>%
    arrange(Age)
  
  #CBM Validation data
  validationAbovegroundBiomassC <- read.table(paste0(pathToCBMoutput,crosswalkStratumState$CBMOutputFile[i]),
                                              header = TRUE,
                                              sep = ",")
  
  # Get Admin Boundary Id
  adminBoundaryTable <- read.csv(paste0(pathToInputTables,"tblAdminBoundaryDefault.csv"),
                                 stringsAsFactors = F)
  adminBoundaryID <- adminBoundaryTable$AdminBoundaryID[adminBoundaryTable$AdminBoundaryName == as.character(crosswalkStratumState$AdminBoundaryId[i])]
  
  # Get Ecological Boundary Id
  ecoBoundaryTable <- read.csv(paste0(pathToInputTables,"tblEcoBoundaryDefault.csv"),
                               stringsAsFactors = F)
  ecoBoundaryID <- ecoBoundaryTable$EcoBoundaryID[ecoBoundaryTable$EcoBoundaryName == as.character(crosswalkStratumState$EcoBoundaryId[i])]
  
  # Get Species and Forest Type Ids
  speciesTypeTable <- read.csv(paste0(pathToInputTables,"tblSpeciesTypeDefault.csv"),
                               stringsAsFactors = F)
  speciesTypeID <- speciesTypeTable$SpeciesTypeID[speciesTypeTable$SpeciesTypeName == as.character(crosswalkStratumState$SpeciesTypeId[i])]
  forestTypeID <- speciesTypeTable$ForestTypeID[speciesTypeTable$SpeciesTypeID == speciesTypeID]
  genusTypeID <- speciesTypeTable$GenusID[speciesTypeTable$SpeciesTypeID == speciesTypeID]
  
  # Get Forest Type Name
  forestTypeTable <- read.csv(paste0(pathToInputTables,"tblForestTypeDefault.csv"),
                              stringsAsFactors = F)
  ForestType <- as.character(forestTypeTable$ForestTypeName[forestTypeTable$ForestTypeID == forestTypeID])
  forestType <- ForestType
  
  # Get Spatial Planning Unit ID (SPUID) from adminBoundary and ecoBoundary
  SPUTable <- read.csv(paste0(pathToInputTables,"tblSPUDefault.csv"),
                       stringsAsFactors = F)
  SPUID <- SPUTable$SPUID[SPUTable$AdminBoundaryID==adminBoundaryID & SPUTable$EcoBoundaryID==ecoBoundaryID]
  
  # Throw error if SPUID is empty
  if(is.na(SPUID) || is.null(SPUID) || length(SPUID) == 0){
    stop("SPUID is of length 0 or is NA or null")
  }
  
  # Get Stratums and stateclass IDs
  the_stratum <- crosswalkStratumState$StratumId[i]
  the_secondarystratum <- crosswalkStratumState$SecondaryStratumId[i]
  the_class <- crosswalkStratumState$StateClassId[i]
  
  # Get biomass expansion factors
  if (grepl("- Genus type", crosswalkStratumState$SpeciesTypeId[i])){
    
    biomassExpansionTable <- read.csv(paste0(pathToInputTables,"tblBioTotalStemwoodGenusDefault.csv"),
                                      stringsAsFactors = F)
    
    biomassExpansionTableRow <- biomassExpansionTable %>%
      filter(DefaultSPUID == SPUID, DefaultGenusID == genusTypeID)
    
  } else {
    
    biomassExpansionTable <- read.csv(paste0(pathToInputTables,"tblBioTotalStemwoodSpeciesTypeDefault.csv"),
                                      stringsAsFactors = F)
    
    biomassExpansionTableRow <- biomassExpansionTable %>%
      filter(DefaultSPUID == SPUID, DefaultSpeciesTypeID == speciesTypeID)
    
    if (nrow(biomassExpansionTableRow) == 0) {
      
      print(paste0(the_class," is missing expansion factors, trying genus type instead"))
      
      biomassExpansionTable <- read.csv(paste0(pathToInputTables,"tblBioTotalStemwoodGenusDefault.csv"),
                                        stringsAsFactors = F)
      
      biomassExpansionTableRow <- biomassExpansionTable %>%
        filter(DefaultSPUID == SPUID, DefaultGenusID == genusTypeID)
      
      if (nrow(biomassExpansionTableRow) == 0) {
        
        print(paste0(the_class," is still missing expansion factors, trying forest type instead"))
        
        biomassExpansionTable <- read.csv(paste0(pathToInputTables,"tblBioTotalStemwoodForestTypeDefault.csv"),
                                          stringsAsFactors = F)
        
        biomassExpansionTableRow <- biomassExpansionTable %>%
          filter(DefaultSPUID == SPUID, DefaultForestTypeID==forestTypeID)
        
      }
      
    }
    
  }
  
  # Get biomass to carbon multipliers
  biomassComponentTable <- read.csv(paste0(pathToInputTables,"tblBiomassComponent.csv"),
                                    stringsAsFactors = F)
  biomassToCarbonTable <- read.csv(paste0(pathToInputTables,"tblBiomassToCarbonDefault.csv"),
                                   stringsAsFactors = F)
  biomassToCarbonTable <- merge.data.frame(biomassToCarbonTable, biomassComponentTable) 
  
  ## Volume to biomass
  # Parameters for equations in Boudewyn et al. 2007
  
  # if(useCBMAgeVsCarbonCurves==T){
  #   volumeToCarbon <- validationBiomassCWide
  #   names(volumeToCarbon) <- c("age", "c_foliage", "c_other", "c_m", "c_coarseroots", "c_fineroots")
  # }
  
  if(useCBMAgeVsCarbonCurves==F){
    
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
      filter(AdminBoundaryID == adminBoundaryID)
    
    propTop <- adminBoundaryTableSub %>% pull(paste0(forestType,"TopProportion"))/100
    propStump <- adminBoundaryTableSub %>% pull(paste0(forestType,"StumpProportion"))/100
    
    # Set up dataframe to hold results
    volumeToCarbon <- data.frame(age = grossMerchantableVolume$Age, 
                                 volume = grossMerchantableVolume$Volume)
    
    # Total stem wood biomass/ha for live, merchantable size trees
    # b_m = total stem wood biomass of merchantable-sized live trees (biomass includes stumps and tops), in metric tonnes per ha
    volumeToCarbon$b_m <- A * volumeToCarbon$volume ^ B
    
    # Total stem wood biomass/ha for live, non-merchantable size trees
    # Nonmerchantable expansion factor
    volumeToCarbon$nonmerchfactor <- k_nonmerch + a_nonmerch * volumeToCarbon$b_m ^ b_nonmerch
    
    # Add Cap
    volumeToCarbon$nonmerchfactor <- ifelse(volumeToCarbon$nonmerchfactor > cap_nonmerch,cap_nonmerch,volumeToCarbon$nonmerchfactor)
    
    # b_nm = stem wood biomass of live, merchantable and nonmerchantable-sized trees (tonnes/ha)
    volumeToCarbon$b_nm <- volumeToCarbon$nonmerchfactor * volumeToCarbon$b_m
    # b_n = stem wood biomass of live, nonmerchantable-sized trees (tonnes/ha)
    volumeToCarbon$b_n <- (volumeToCarbon$nonmerchfactor * volumeToCarbon$b_m) - volumeToCarbon$b_m
    
    # Total stem wood biomass/ha for live sapling size trees
    #Sapling expansion factor
    volumeToCarbon$saplingfactor <- k_sap + a_sap * (volumeToCarbon$b_nm ^ b_sap)
    
    
    # Add Cap
    volumeToCarbon$saplingfactor <- ifelse(volumeToCarbon$saplingfactor > cap_sap,cap_sap,volumeToCarbon$saplingfactor)
    
    # b_s = stem wood biomass of live, sapling-sized trees (tonnes/ha)
    volumeToCarbon$b_s <- (volumeToCarbon$saplingfactor * volumeToCarbon$b_nm) - volumeToCarbon$b_nm
    
    # Total stemwood of all live trees
    volumeToCarbon$b_sw <- volumeToCarbon$b_m + volumeToCarbon$b_n + volumeToCarbon$b_s
    
    # Compute proportions
    volumeToCarbon$denominator <- (1 + exp(a1 + a2 * volumeToCarbon$volume + a3 * log(volumeToCarbon$volume + 5)) 
                                   + exp(b1 + b2 * volumeToCarbon$volume + b3 * log(volumeToCarbon$volume + 5)) 
                                   + exp(c1 + c2 * volumeToCarbon$volume + c3 * log(volumeToCarbon$volume + 5)))
    
    # Stem wood proportion
    volumeToCarbon$p_stemwood <- 1 / volumeToCarbon$denominator  
    
    # Stem bark proportion  
    volumeToCarbon$p_bark <- exp(a1 + a2 * volumeToCarbon$volume + a3 * log(volumeToCarbon$volume + 5)) / volumeToCarbon$denominator
    
    # Branches proportion
    volumeToCarbon$p_branches <- exp(b1 + b2 * volumeToCarbon$volume + b3 * log(volumeToCarbon$volume + 5)) / volumeToCarbon$denominator
    
    # Foliage proportion
    volumeToCarbon$p_foliage <- exp(c1 + c2 * volumeToCarbon$volume + c3 * log(volumeToCarbon$volume + 5)) / volumeToCarbon$denominator
    
    #Total tree biomass/ha live
    volumeToCarbon$b <- volumeToCarbon$b_sw / volumeToCarbon$p_stemwood
    
    # Biomass based on b (total biomass of live trees)
    volumeToCarbon$b_bark <- volumeToCarbon$b * volumeToCarbon$p_bark
    volumeToCarbon$b_branches <- volumeToCarbon$b * volumeToCarbon$p_branches
    volumeToCarbon$b_foliage <- volumeToCarbon$b * volumeToCarbon$p_foliage
    #volumeToCarbon$b_other <- volumeToCarbon$b_bark + volumeToCarbon$b_branches + volumeToCarbon$b_n + volumeToCarbon$b_s
    
    # Partition bark on the merchantable stem or the nonmerchantable/sapling trees
    volumeToCarbon$b_bark_m <- volumeToCarbon$b_bark*(volumeToCarbon$b_m/volumeToCarbon$b_sw)
    volumeToCarbon$b_bark_ns <- volumeToCarbon$b_bark*((volumeToCarbon$b_n + volumeToCarbon$b_s)/volumeToCarbon$b_sw)
    
    # Merchantable in CBM includes bark but does not include the top and stump 
    volumeToCarbon$b_m_CBM <- (volumeToCarbon$b_m * (1-propTop-propStump)) + 
      (volumeToCarbon$b_bark_m * (1-propTop-propStump))
    
    # The other pool includes the remaining biomass pools, including the stumps and tops
    volumeToCarbon$b_other <- (volumeToCarbon$b_m * (propTop + propStump)) + 
      (volumeToCarbon$b_bark_m * (propTop + propStump))  + 
      volumeToCarbon$b_branches + 
      volumeToCarbon$b_n + 
      volumeToCarbon$b_s +
      volumeToCarbon$b_bark_ns
    
    #Replace NoN values with 0's
    is.nan.data.frame <- function(x)
      do.call(cbind, lapply(x, is.nan))
    
    volumeToCarbon[is.nan.data.frame(volumeToCarbon)] <- 0
    
    # smoothing functions
    
    # Smooth foliage
    min1 <- max(volumeToCarbon$age[volumeToCarbon$volume <= minVolume])
    
    minF2 <- volumeToCarbon$age[which(diff(sign(diff(volumeToCarbon$b_foliage))) == 2)]
    
    if (length(minF2) == 0){
      minF <- min1
    } else {
      minF <- max(min1,minF2)
    }
    
    png(paste0(pathToSaveFigures,"Smooth/",
               gsub(" ","-",gsub(": ","-",gsub("/","-",the_class))),
               "-foliageOrig-",
               "50years.png"))
    plot(volumeToCarbon$age,volumeToCarbon$b_foliage, xlim = c(0,50), ylim = c(0,200), type = "l")
    abline(v = minF)
    dev.off()
    
    volumeToCarbon = volumeToCarbon %>% 
      arrange(age) %>% 
      mutate(b_foliageSmooth = if_else(age <= minF, NA, b_foliage)) %>% 
      mutate(b_foliageSmooth = if_else(age == 0, 0, b_foliageSmooth)) %>%
      mutate(b_foliageSmooth = na_interpolation(b_foliageSmooth, option = "spline", method = "natural"))
    
    png(paste0(pathToSaveFigures,"Smooth/",
               gsub(" ","-",gsub(": ","-",gsub("/","-",the_class))),
               "-foliageSmooth-",
               "50years.png"))
    plot(volumeToCarbon$age,volumeToCarbon$b_foliageSmooth, xlim = c(0,50), ylim = c(0,200), type = "l")
    abline(v = minF)
    dev.off()
    
    # Smooth other wood
    
    minO2 <- volumeToCarbon$age[which(diff(sign(diff(volumeToCarbon$b_other))) == 2)]
  
    if (length(minO2) == 0){
      minO <- min1
    } else {
      minO <- max(min1,minO2)
    }
    
    png(paste0(pathToSaveFigures,"Smooth/",
               gsub(" ","-",gsub(": ","-",gsub("/","-",the_class))),
               "-otherOrig-",
               "50years.png"))
    plot(volumeToCarbon$age,volumeToCarbon$b_other, xlim = c(0,50), ylim = c(0,200), type = "l")
    abline(v = minO)
    dev.off()
    
    volumeToCarbon = volumeToCarbon %>% 
      arrange(age) %>% 
      mutate(b_otherSmooth = if_else(age <= minO, NA, b_other)) %>% 
      mutate(b_otherSmooth = if_else(age == 0, 0, b_otherSmooth)) %>%
      mutate(b_otherSmooth = na_interpolation(b_otherSmooth, option = "spline", method = "natural"))
    
    png(paste0(pathToSaveFigures,"Smooth/",
               gsub(" ","-",gsub(": ","-",gsub("/","-",the_class))),
               "-otherSmooth-",
               "50years.png"))
    plot(volumeToCarbon$age,volumeToCarbon$b_otherSmooth, xlim = c(0,50), ylim = c(0,200), type = "l")
    abline(v = minO)
    dev.off()
    
    # Smooth merchantable
    
    minM2 <- volumeToCarbon$age[which(diff(sign(diff(volumeToCarbon$b_m_CBM))) == 2)]

    if (length(minM2) == 0){
      minM <- min1
    } else {
      minM <- max(min1,minM2)
    }
    
    png(paste0(pathToSaveFigures,"Smooth/",
               gsub(" ","-",gsub(": ","-",gsub("/","-",the_class))),
               "-merchOrig-",
               "50years.png"))
    plot(volumeToCarbon$age,volumeToCarbon$b_m_CBM, xlim = c(0,50), ylim = c(0,200), type = "l")
    abline(v = minM)
    dev.off()
    
    volumeToCarbon = volumeToCarbon %>% 
      arrange(age) %>% 
      mutate(b_mSmooth = if_else(age <= minM, NA, b_m_CBM)) %>% 
      mutate(b_mSmooth = if_else(age == 0, 0, b_mSmooth)) %>%
      mutate(b_mSmooth = na_interpolation(b_mSmooth, option = "spline", method = "natural"))
    
    png(paste0(pathToSaveFigures,"Smooth/",
               gsub(" ","-",gsub(": ","-",gsub("/","-",the_class))),
               "-merchSmooth-",
               "50years.png"))
    plot(volumeToCarbon$age,volumeToCarbon$b_mSmooth, xlim = c(0,50), ylim = c(0,200), type = "l")
    abline(v = minM)
    dev.off()
    
    volumeToCarbon <- volumeToCarbon %>%
      mutate(b_aboveground = b_mSmooth+b_foliageSmooth+b_otherSmooth)
    
    ## Biomass to carbon
    isSoftwood <- if(forestType == "Softwood") 1 else 0
    volumeToCarbon$c_aboveground <- volumeToCarbon$b_aboveground * biomassToCarbonTable[biomassToCarbonTable$BiomassComponentName=="Other biomass component" & 
                                                                                          biomassToCarbonTable$Softwood==isSoftwood, "Multiplier"]
    volumeToCarbon$c_other <- volumeToCarbon$b_otherSmooth * biomassToCarbonTable[biomassToCarbonTable$BiomassComponentName=="Other biomass component" & 
                                                                                    biomassToCarbonTable$Softwood==isSoftwood, "Multiplier"]
    volumeToCarbon$c_foliage <- volumeToCarbon$b_foliageSmooth * biomassToCarbonTable[biomassToCarbonTable$BiomassComponentName=="Foliage biomass component" & 
                                                                                        biomassToCarbonTable$Softwood==isSoftwood, "Multiplier"]
    volumeToCarbon$c_m <- volumeToCarbon$b_mSmooth * biomassToCarbonTable[biomassToCarbonTable$BiomassComponentName=="Merchantable biomass component" & 
                                                                            biomassToCarbonTable$Softwood==isSoftwood, "Multiplier"]
    
    
    # filter validation data
    validationAbovegroundBiomassC <- validationAbovegroundBiomassC %>%
      rename(Timestep = Time.Step,
             `Biomass: Merchantable` = paste0(forestType,".Merchantable"),
             `Biomass: Foliage` = paste0(forestType,".Foliage"),
             `Biomass: Other Wood` = paste0(forestType,".Other"),
             `Biomass: Fine Roots` = paste0(forestType,".Fine.Roots"),
             `Biomass: Coarse Roots` = paste0(forestType,".Coarse.Roots")) %>%
      filter(Timestep < 301) %>%
      mutate(BiomassAboveground = `Biomass: Merchantable`+`Biomass: Foliage`+`Biomass: Other Wood`) %>%
      mutate(BiomassBelowground = `Biomass: Fine Roots`+`Biomass: Coarse Roots`)
    
    validationBiomassCWide <- validationAbovegroundBiomassC
    
    maxY <- max(validationBiomassCWide$`Biomass: Merchantable`)*2
    
    titleAll <- paste0(the_class,": Aboveground Pools")
    
    png(paste0(pathToSaveFigures,
               gsub(" ","-",gsub(": ","-",gsub("/","-",the_class))),
               "-above-",
               "300years.png"), res = 300, width = 6, height = 6, units = "in")
    #Plot aboveground carbon stocks with validation
    plot(validationAbovegroundBiomassC$Timestep, validationAbovegroundBiomassC[,"Biomass: Merchantable"], col="#1B9E77", xlim=c(0,300), ylim=c(0,maxY), type="l",xlab = "timestep",ylab = "carbon stock (tons C/ha)", main = titleAll, cex.main = 0.8)
    lines(validationAbovegroundBiomassC$Timestep, validationAbovegroundBiomassC[,"Biomass: Foliage"], col="#D95F02")
    lines(validationAbovegroundBiomassC$Timestep, validationAbovegroundBiomassC[,"Biomass: Other Wood"], col="#7570B3")
    lines(volumeToCarbon$age, volumeToCarbon$c_m, col="#1B9E77", lty=3, lwd=2)
    lines(volumeToCarbon$age, volumeToCarbon$c_foliage, col="#D95F02", lty=3, lwd=2)
    lines(volumeToCarbon$age, volumeToCarbon$c_other, col="#7570B3", lty=3, lwd=2)
    legend(0,maxY, legend=c("CBM Orig Merchantable", "CBM Orig Foliage", "CBM Orig Other", "CBM Replicate Merchantable", "CBM Replicate Foliage", "CBM Replicate Other"),
           col=c("#1B9E77", "#D95F02", "#7570B3","#1B9E77", "#D95F02", "#7570B3"), lty=c(1,1,1,3,3,3), lwd=c(1,1,1,2,2,2), cex = 0.8)
    dev.off()
    
    png(paste0(pathToSaveFigures,
               gsub(" ","-",gsub(": ","-",gsub("/","-",the_class))),
               "-above-",
               "50years.png"), res = 300, width = 6, height = 6, units = "in")
    plot(validationAbovegroundBiomassC$Timestep, validationAbovegroundBiomassC[,"Biomass: Merchantable"], col="#1B9E77", xlim=c(0,50), ylim=c(0,maxY), type="l",xlab = "timestep",ylab = "carbon stock (tons C/ha)", main = titleAll, cex.main = 0.8)
    lines(validationAbovegroundBiomassC$Timestep, validationAbovegroundBiomassC[,"Biomass: Foliage"], col="#D95F02")
    lines(validationAbovegroundBiomassC$Timestep, validationAbovegroundBiomassC[,"Biomass: Other Wood"], col="#7570B3")
    lines(volumeToCarbon$age, volumeToCarbon$c_m, col="#1B9E77", lty=3, lwd=2)
    lines(volumeToCarbon$age, volumeToCarbon$c_foliage, col="#D95F02", lty=3, lwd=2)
    lines(volumeToCarbon$age, volumeToCarbon$c_other, col="#7570B3", lty=3, lwd=2)
    legend(0,maxY, legend=c("CBM Orig Merchantable", "CBM Orig Foliage", "CBM Orig Other", "CBM Replicate Merchantable", "CBM Replicate Foliage", "CBM Replicate Other"),
           col=c("#1B9E77", "#D95F02", "#7570B3","#1B9E77","#D95F02","#7570B3"), lty=c(1,1,1,3,3,3), lwd=c(1,1,1,2,2,2), cex = 0.8)
    dev.off()
    
    png(paste0(pathToSaveFigures,
               gsub(" ","-",gsub(": ","-",gsub("/","-",the_class))),
               "-aboveTotal-",
               "300years.png"), res = 300, width = 6, height = 6, units = "in")
    plot(validationAbovegroundBiomassC$Timestep, validationAbovegroundBiomassC[,"BiomassAboveground"], col="Black", xlim=c(0,300), ylim=c(0,maxY*1.3), type="l",xlab = "timestep",ylab = "carbon stock (tons C/ha)", main = titleAll, cex.main = 0.8)
    lines(volumeToCarbon$age, volumeToCarbon$c_aboveground, col="Black", lty=3, lwd=2)
    legend(0,maxY*1.3, legend=c("CBM Orig Aboveground", "CBM Replicate Aboveground"),
           col=c("Black", "Black"), lty=c(1,3), lwd=c(1,2), cex = 0.8)
    dev.off()
    
    png(paste0(pathToSaveFigures,
               gsub(" ","-",gsub(": ","-",gsub("/","-",the_class))),
               "-aboveTotal-",
               "50years.png"), res = 300, width = 6, height = 6, units = "in")
    plot(validationAbovegroundBiomassC$Timestep, validationAbovegroundBiomassC[,"BiomassAboveground"], col="Black", xlim=c(0,50), ylim=c(0,maxY*1.3), type="l",xlab = "timestep",ylab = "carbon stock (tons C/ha)", main = titleAll, cex.main = 0.8)
    lines(volumeToCarbon$age, volumeToCarbon$c_aboveground, col="Black", lty=3, lwd=2)
    legend(0,maxY*1.3, legend=c("CBM Orig Aboveground", "CBM Replicate Aboveground"),
           col=c("Black", "Black"), lty=c(1,3), lwd=c(1,2), cex = 0.8)
    dev.off()
    
  }
  
  ###################################################
  # Volume to belowground biomass carbon conversion #
  ###################################################
  # if(useCBMAgeVsCarbonCurves==T){
  #   volumeToCarbon <- validationBiomassCWide
  #   names(volumeToCarbon) <- c("age", "c_foliage", "c_other", "c_m", "c_coarseroots", "c_fineroots")
  # }
  if(useCBMAgeVsCarbonCurves==F){
    ## Volume to biomass
    # Parameters from Li et al. 2003
    
    if(forestType == "Softwood") {volumeToCarbon$b_roots <- 0.222 * volumeToCarbon$b_aboveground} else {volumeToCarbon$b_roots <- 1.576 * volumeToCarbon$b_aboveground ^ 0.615}
    #if(isSoftwood) {volumeToCarbon$b_roots <- 0.2222 * volumeToCarbon$b_aboveground} else {volumeToCarbon$b_roots <- 1.576 * volumeToCarbon$b_aboveground ^ 0.615}
    volumeToCarbon$p_fineroots <- 0.072 + 0.354 * exp(-0.060 * volumeToCarbon$b_roots)
    volumeToCarbon$b_fineroots <- volumeToCarbon$p_fineroots * volumeToCarbon$b_roots
    volumeToCarbon$b_coarseroots <- volumeToCarbon$b_roots - volumeToCarbon$b_fineroots
    # Add in hardwood equation
    
    ## Biomass to carbon
    isSoftwood <- if(forestType == "Softwood") 1 else 0
    volumeToCarbon$c_fineroots <- volumeToCarbon$b_fineroots * biomassToCarbonTable[biomassToCarbonTable$BiomassComponentName=="Fine root biomass component" & 
                                                                                      biomassToCarbonTable$Softwood==isSoftwood, "Multiplier"]
    volumeToCarbon$c_coarseroots <- volumeToCarbon$b_coarseroots * biomassToCarbonTable[biomassToCarbonTable$BiomassComponentName=="Coarse root biomass component" & 
                                                                                          biomassToCarbonTable$Softwood==isSoftwood, "Multiplier"]
    volumeToCarbon$c_belowground <- volumeToCarbon$c_fineroots + volumeToCarbon$c_coarseroots
    
    maxY2 <- max(validationBiomassCWide$`Biomass: Coarse Roots`)*2
    
    titleAll2 <- paste0(the_class,": Belowground Pools")
    
    png(paste0(pathToSaveFigures,
               gsub(" ","-",gsub(": ","-",gsub("/","-",the_class))),
               "-below-",
               "300years.png"), res = 300, width = 6, height = 6, units = "in")
    #Plot belowground carbon stocks with validation
    plot(validationBiomassCWide$Timestep, validationBiomassCWide[,"Biomass: Fine Roots"], col="#E7298A", xlim=c(0,300), ylim=c(0,maxY2), type="l",xlab = "timestep",ylab = "carbon stock (tons C/ha)", main = titleAll2, cex.main = 0.8)
    lines(validationBiomassCWide$Timestep, validationBiomassCWide[,"Biomass: Coarse Roots"], col="#66A61E")
    lines(volumeToCarbon$age, volumeToCarbon$c_fineroots, col="#E7298A", lty=3, lwd=2)
    lines(volumeToCarbon$age, volumeToCarbon$c_coarseroots, col="#66A61E", lty=3, lwd=2)
    legend(0,maxY2, legend=c("CBM Orig Fine roots", "CBM Orig Coarse roots", "CBM Replicate Fine roots", "CBM Replicate Coarse roots"),
           col=c("#E7298A", "#66A61E", "#E7298A", "#66A61E"), lty=c(1,1,3,3), lwd=c(1,1,2,2), cex = 0.8)
    dev.off()
    
    png(paste0(pathToSaveFigures,
               gsub(" ","-",gsub(": ","-",gsub("/","-",the_class))),
               "-below-",
               "50years.png"), res = 300, width = 6, height = 6, units = "in")
    #Plot belowground carbon stocks with validation
    plot(validationBiomassCWide$Timestep, validationBiomassCWide[,"Biomass: Fine Roots"], col="#E7298A", xlim=c(0,50), ylim=c(0,maxY2), type="l",xlab = "timestep",ylab = "carbon stock (tons C/ha)", main = titleAll2, cex.main = 0.8)
    lines(validationBiomassCWide$Timestep, validationBiomassCWide[,"Biomass: Coarse Roots"], col="#66A61E")
    lines(volumeToCarbon$age, volumeToCarbon$c_fineroots, col="#E7298A", lty=3, lwd=2)
    lines(volumeToCarbon$age, volumeToCarbon$c_coarseroots, col="#66A61E", lty=3, lwd=2)
    legend(0,maxY2, legend=c("CBM Orig Fine roots", "CBM Orig Coarse roots", "CBM Replicate Fine roots", "CBM Replicate Coarse roots"),
           col=c("#E7298A", "#66A61E", "#E7298A", "#66A61E"), lty=c(1,1,3,3), lwd=c(1,1,2,2), cex = 0.8)
    dev.off()
    
    png(paste0(pathToSaveFigures,
               gsub(" ","-",gsub(": ","-",gsub("/","-",the_class))),
               "-belowTotal-",
               "300years.png"), res = 300, width = 6, height = 6, units = "in")
    plot(validationBiomassCWide$Timestep, validationBiomassCWide[,"BiomassBelowground"], col="Black", xlim=c(0,300), ylim=c(0,maxY2*1.3), type="l",xlab = "timestep",ylab = "carbon stock (tons C/ha)", main = titleAll2, cex.main = 0.8)
    lines(volumeToCarbon$age, volumeToCarbon$c_belowground, col="Black", lty=3, lwd=2)
    legend(0,maxY2*1.3, legend=c("CBM Orig Belowground", "CBM Replicate Belowground"),
           col=c("Black", "Black"), lty=c(1,3), lwd=c(1,2), cex = 0.8)
    dev.off()
    
    png(paste0(pathToSaveFigures,
               gsub(" ","-",gsub(": ","-",gsub("/","-",the_class))),
               "-belowTotal-",
               "50years.png"), res = 300, width = 6, height = 6, units = "in")
    plot(validationBiomassCWide$Timestep, validationBiomassCWide[,"BiomassBelowground"], col="Black", xlim=c(0,50), ylim=c(0,maxY2*1.3), type="l",xlab = "timestep",ylab = "carbon stock (tons C/ha)", main = titleAll2, cex.main = 0.8)
    lines(volumeToCarbon$age, volumeToCarbon$c_belowground, col="Black", lty=3, lwd=2)
    legend(0,maxY2*1.3, legend=c("CBM Orig Belowground", "CBM Replicate Belowground"),
           col=c("Black", "Black"), lty=c(1,3), lwd=c(1,2), cex = 0.8)
    dev.off()
  }
  
  
}

