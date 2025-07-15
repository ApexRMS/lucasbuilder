# ApexRMS - Oct 2024
# script creates "lucasbuilder-conus" template library for lucasbuilder package
# template library includes merchantable volume curves


library(rsyncrosim)
library(readxl)
library(tidyverse)

options(stringsAsFactors=FALSE)

# Settings ----

mySession <- session()
libraryName <- paste0(getwd(),"/model/lucasbuilder-conus")
myProjectName <- "Definitions"
initialInputsDirectory <- paste0(dirname(getwd()), "/data/user-example-inputs-merch/")
definitionsPath <- paste0(initialInputsDirectory,"ConusLibrary/")

# Build base library ----

# Create library
myLibrary <- ssimLibrary(libraryName, 
                         packages = c("stsim", "lucasbuilder"),
                         session = mySession)
myProject <- project(myLibrary, project=myProjectName)

description(myLibrary) <- "Library for the LUCAS Builder package containing the 
29 forest types required to run forest carbon simulations for the 
Conterminous United States (CONUS)."

#######################
## Predefined Inputs ##
#######################
# set library/project defaults and generate scenarios that remain constant

# Project definitions ----

# ## Strata ----
# 
# # Add [unspecified] strata as a default option
# sheetName <- "stsim_Stratum"
# mySheet <- datasheet(myProject, name = sheetName)
# mySheetFull <- add_row(mySheet, Name = "[Unspecified]")
# saveDatasheet(myProject, mySheetFull, sheetName, append = F)


# Library definitions -----
sheetName <- "core_Option"
mySheet <- datasheet(myProject, sheetName)
mySheet$UseConda = TRUE
saveDatasheet(myProject, mySheet, sheetName)

## Transitions ----

sheetName <- "stsim_TransitionType"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath,"Transition Type.csv"))
names(mySheetFull) <- gsub("ID","Id",names(mySheetFull))
saveDatasheet(myProject, mySheetFull, sheetName)

sheetName <- "stsim_TransitionGroup"
csvName <- "Transition Group.csv"
myData <- read.csv(paste0(initialInputsDirectory, csvName))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_TransitionTypeGroup"
csvName <- "Transition Types by Group.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_TransitionSimulationGroup"
csvName <- "Transition Simulation Groups.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
names(myData) <- gsub("ID","Id",names(myData))
saveDatasheet(myProject, myData, sheetName)

# Distributions
sheetName <- "core_DistributionType"
csvName <- "Distributions.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)

# External Variable
sheetName <- "core_ExternalVariableType"
csvName <- "External Variables.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)


## Ages ----

### Age groups
sheetName <- "stsim_AgeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(dirname(getwd()), "/data/Age Groups.xlsx"), sheet = "Age Groups") %>%
  data.frame()
saveDatasheet(myProject, mySheetFull, sheetName)

## Advanced ----

### Attribute group
sheetName <- "stsim_AttributeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "Attribute Group.csv"))
saveDatasheet(myProject, mySheetFull, sheetName)

### State attribute type
sheetName <- "stsim_StateAttributeType"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "State Attribute Type.csv"))
names(mySheetFull) <- gsub("ID","Id",names(mySheetFull))
saveDatasheet(myProject, mySheetFull, sheetName)

### Stock/Flow definitions ----

#### Stock type
sheetName <- "stsim_StockType"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "Stock Type.csv"))
saveDatasheet(myProject, mySheetFull, sheetName)

#### Stock group
sheetName <- "stsim_StockGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "Stock Group.csv"))
saveDatasheet(myProject, mySheetFull, sheetName)

#### Flow type
sheetName <- "stsim_FlowType"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "Flow Type.csv"))
saveDatasheet(myProject, mySheetFull, sheetName)

#### Flow group
sheetName <- "stsim_FlowGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "Flow Group.csv"))
saveDatasheet(myProject, mySheetFull, sheetName)

### CBM Definitions ----

#### Ecological boundary
sheetName <- "lucasbuilder_EcoBoundary"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "Ecological Boundary LUCAS.csv"))
saveDatasheet(myProject, mySheetFull, sheetName)

#### Administrative boundary
sheetName <- "lucasbuilder_AdminBoundary"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "Administrative Boundary LUCAS.csv"))
saveDatasheet(myProject, mySheetFull, sheetName)

#### Species type
sheetName <- "lucasbuilder_SpeciesType"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "Species Type.csv"))
saveDatasheet(myProject, mySheetFull, sheetName)

#### Disturbance type
sheetName <- "lucasbuilder_DisturbanceType"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "Disturbance Type.csv"))
saveDatasheet(myProject, mySheetFull, sheetName)

#### CBM-CFS3 stock
sheetName <- "lucasbuilder_CBMCFS3Stock"
mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "CBM Crosswalk Carbon Stock.csv")) %>%
  select(CBMStock) %>%
  rename(Name = CBMStock) 
saveDatasheet(myProject, mySheetFull, sheetName)

## Terminology ----
sheetName <- "stsim_Terminology"
mySheet <- datasheet(myProject, name=sheetName)
mySheet$AmountLabel[1] <- "Area"
mySheet$AmountUnits[1] <- "hectares"
mySheet$StateLabelX[1] <- "LULC"
mySheet$StateLabelY[1] <- "SubClass"
mySheet$PrimaryStratumLabel[1] <- "Ecological Boundary"
mySheet$SecondaryStratumLabel[1] <- "Administrative Boundary"
mySheet$TimestepUnits[1] <- "Year"
mySheet$StockUnits[1] <- "metric tons C"
saveDatasheet(myProject, mySheet, sheetName)

# Predefined Input Scenario data ----

# # Find the Parent Project ID to create a folder within this Project
# pid <-  project(myLibrary)$projectId[1]
# 
# # Write the console command for "Run Scenario" folder
# command <- paste0("\"", filepath(mySession), "/SyncroSim.Console.Exe\"",
#                    " --create --folder --lib=", filepath(myLibrary),
#                    " --name=1-Predefined-Inputs --tpid=", pid)
# 
# # Invoke a system command
# sysOut <- system(command, intern=TRUE)

# datasheet(myProject, optional = T) # datasheet(myScenario, optional = T)

folder1 <- folder(ssimObject = myProject, "1 - Predefined Inputs")

## Flow pathways
myScenarioName <- "Flow Pathways"
myScenario <- scenario(myProject, scenario = myScenarioName, folder = folderId(folder1))

sheetName <- "stsim_FlowPathwayDiagram"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "Flow Pathway Diagram.csv"))
names(mySheetFull) <- gsub("ID","Id",names(mySheetFull))
saveDatasheet(myScenario, mySheetFull, sheetName)

sheetName <- "stsim_FlowPathway"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "Flow Pathways.csv"))
names(mySheetFull) <- gsub("ID","Id",names(mySheetFull))
saveDatasheet(myScenario, mySheetFull, sheetName)

## Flow order
myScenarioName <- "Flow Order"
myScenario = scenario(myProject, scenario = myScenarioName, folder = folderId(folder1))

sheetName <- "stsim_FlowOrder"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "Flow Order.csv"))
names(mySheetFull) <- gsub("ID","Id",names(mySheetFull))
saveDatasheet(myScenario, mySheetFull, sheetName)

sheetName <- "stsim_FlowOrderOptions"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
#mySheetFull <- addRow(mySheet, data.frame(ApplyBeforeTransitions = T, ApplyEquallyRankedSimultaneously = T))
mySheetFull <- data.frame(ApplyBeforeTransitions = T, ApplyEquallyRankedSimultaneously = T)
saveDatasheet(myScenario, mySheetFull, sheetName)

## Flow group membership
myScenarioName <- "Flow Group Membership"
myScenario <- scenario(myProject, scenario = myScenarioName, folder = folderId(folder1))

sheetName <- "stsim_FlowTypeGroupMembership"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath,"Flow Type-Group Membership.csv"))
names(mySheetFull) <- gsub("ID","Id",names(mySheetFull))
saveDatasheet(myScenario, mySheetFull, sheetName)

## Stock group membership
myScenarioName <- "Stock Group Membership"
myScenario <- scenario(myProject, scenario = myScenarioName, folder = folderId(folder1))

sheetName <- "stsim_StockTypeGroupMembership"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "Stock Type-Group Membership.csv"))
names(mySheetFull) <- gsub("ID","Id",names(mySheetFull))
saveDatasheet(myScenario, mySheetFull, sheetName)

## Initial stocks
myScenarioName <- "Initial Stocks"
myScenario <- scenario(myProject, scenario = myScenarioName, folder = folderId(folder1))

sheetName <- "stsim_InitialStockNonSpatial"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath, "Initial Stock - Non Spatial.csv"))
names(mySheetFull) <- gsub("ID","Id",names(mySheetFull))
saveDatasheet(myScenario, mySheetFull, sheetName)

## Stock flow output options
myScenarioName <- "Stock Flow Output Options"
myScenario <- scenario(myProject, scenario = myScenarioName, folder = folderId(folder1))
sheetName <- "stsim_OutputOptionsStockFlow"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read_xlsx(path = paste0(dirname(getwd()), "/data/Stock-Flow Output Options.xlsx"), sheet = "Stock-Flow Output") %>%
  data.frame()
# names(mySheetFull) <- names(mySheet)
saveDatasheet(myScenario, mySheetFull, sheetName)

## Crosswalk to ST-Sim
### Disturbance
myScenarioName <- "CBM Crosswalk - Disturbance"
myScenario <- scenario(myProject, scenario = myScenarioName, folder = folderId(folder1))

sheetName <- "lucasbuilder_CrosswalkDisturbance" # datasheet containing the LUCAS-CBM Stock crosswalk table
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath,"CBM Crosswalk Disturbance.csv"))
names(mySheetFull) <- gsub("ID","Id",names(mySheetFull))
saveDatasheet(myScenario, mySheetFull, sheetName)

### Carbon stock
myScenarioName <- "CBM Crosswalk - Stocks"
myScenario <- scenario(myProject, scenario = myScenarioName, folder = folderId(folder1))

sheetName <- "lucasbuilder_CrosswalkStock" # datasheet containing the LUCAS-CBM Stock crosswalk table
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull <- read.csv(paste0(definitionsPath,"CBM Crosswalk Carbon Stock.csv"))
names(mySheetFull) <- gsub("ID","Id",names(mySheetFull))
saveDatasheet(myScenario, mySheetFull, sheetName)

### Output options non-spatial 
myScenario <- scenario(myProject, scenario = "Output Options [Non-spatial]", folder = folderId(folder1))
sheetName <- "stsim_OutputOptions"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1, "SummaryOutputSC"] <- T
mySheet[1, "SummaryOutputSCTimesteps"] <- 1
mySheet[1, "SummaryOutputSCZeroValues"] <- F
mySheet[1, "SummaryOutputTR"] <- T
mySheet[1, "SummaryOutputTRTimesteps"] <- 1
mySheet[1, "SummaryOutputTRIntervalMean"] <- F
mySheet[1, "SummaryOutputTRSC"] <- T
mySheet[1, "SummaryOutputTRSCTimesteps"] <- 1
mySheet[1, "SummaryOutputSA"] <- T
mySheet[1, "SummaryOutputSATimesteps"] <- 1
mySheet[1, "SummaryOutputTA"] <- T
mySheet[1, "SummaryOutputTATimesteps"] <- 1
mySheet[1, "SummaryOutputOmitSS"] <- F
mySheet[1, "SummaryOutputOmitTS"] <- F
saveDatasheet(myScenario, mySheet, sheetName)

#########################
## USER DEFINED INPUTS ##
#########################
# generate scenarios for all required user inputs

# # Write the console command for "Run Scenario" folder
# command <- paste0("\"", filepath(mySession), "/SyncroSim.Console.Exe\"",
#                   " --create --folder --lib=", filepath(myLibrary),
#                   " --name=2-User-Defined-Inputs --tpid=", pid)
# 
# # Invoke a system command
# sysOut <- system(command, intern=TRUE)

folder2 <- folder(ssimObject = myProject, folder = "2 - User Defined Inputs")
folder2.1 <- folder(ssimObject = myProject, folder = "2.1 - Run Setup Inputs", parentFolder = folder2)
folder2.2 <- folder(ssimObject = myProject, folder = "2.2 - Run Forecast Inputs", parentFolder = folder2)

## Project Definitions ---- 

# load in SUST crosswalk data
## user could also define these values in Syncrosim UI 

CBMDir <- paste0(initialInputsDirectory,"cbm-cfs3-simulation-results/")
crosswalkSUSTPath <- paste0(initialInputsDirectory,"Crosswalk - Spatial Unit and Species Type.csv")

crosswalkSUSTFull <- read.csv(crosswalkSUSTPath, check.names = F)
crosswalkSUSTFull$CBMOutputFile <- paste0(CBMDir, crosswalkSUSTFull$CBMOutputFile)

# ### Primary stratum [OPTIONAL]
# ## This adds all unique primary stratum names from the crosswalk table to the definitions of the new model
# sheetName <- "stsim_Stratum"
# mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
# mySheetFull <- data.frame(Name = unique(crosswalkSUSTFull$StratumId),
#                           Id = 1:length(unique(crosswalkSUSTFull$StratumId)))
# # for(i in seq(1:nrow(crosswalkSUSTFull))) {
# #   crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
# #   myValue = crosswalkSUST$`ST-Sim Stratum`
# #   mySheet = addRow(mySheet, data.frame(Name = myValue,
# #                                        Id = 1))
# #   mySheetFull = bind_rows(mySheetFull, mySheet) %>% unique()
# # }
# saveDatasheet(myProject, mySheetFull, sheetName)

sheetName <- "stsim_Stratum"
csvName <- "Ecological Boundary.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
names(myData) <- gsub("ID", "Id", names(myData))
saveDatasheet(myProject, myData, sheetName)


# ### Secondary stratum [OPTIONAL]
# sheetName <- "stsim_SecondaryStratum"
# mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
# mySheetFull <- data.frame(Name = unique(crosswalkSUSTFull$SecondaryStratumId),
#                           Id = 1:length(unique(crosswalkSUSTFull$SecondaryStratumId)))
# # for(i in seq(1:nrow(crosswalkSUSTFull))) {
# #   crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
# #   myValue = crosswalkSUST$`ST-Sim Secondary Stratum`
# #   mySheet = addRow(mySheet, data.frame(Name = myValue,
# #                                        Id = 1))
# #   mySheetFull = bind_rows(mySheetFull, mySheet) %>% unique()
# # }
# saveDatasheet(myProject, mySheetFull, sheetName)

sheetName <- "stsim_SecondaryStratum"
csvName <- "Administrative Boundary.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
names(myData) <- gsub("ID", "Id", names(myData))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_TertiaryStratum"
csvName <- "Tertiary Stratum.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
names(myData) <- gsub("ID", "Id", names(myData))
saveDatasheet(myProject, myData, sheetName)

# ### State Label X 
# sheetName <- "stsim_StateLabelX"
# mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
# mySheetFull = data.frame()
# for(i in seq(1:nrow(crosswalkSUSTFull))) {
#   crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
#   myValue = crosswalkSUST$`ST-Sim State Class`
#   mySheet = addRow(mySheet, data.frame(Name = myValue))
#   mySheetFull = bind_rows(mySheetFull, mySheet) %>% unique()
# }
# saveDatasheet(myProject, mySheetFull, sheetName)
# 
# 
# ### State Label Y 
# sheetName <- "stsim_StateLabelY"
# mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
# mySheetFull = data.frame()
# for(i in seq(1:nrow(crosswalkSUSTFull))) {
#   crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
#   myValue = crosswalkSUST$`ST-Sim State Class`
#   mySheet = addRow(mySheet, data.frame(Name = myValue))
#   mySheetFull = bind_rows(mySheetFull, mySheet) %>% unique()
# }
# saveDatasheet(myProject, mySheetFull, sheetName)
# 
# 
# ### State Class Type [REQUIRED]
# sheetName <- "stsim_StateClass"
# mySheet <- datasheet(myProject, name=sheetName, optional=T, empty = T)
# mySheetFull <- read_xlsx(path = paste0(initialInputsDirectory, "user-example-inputs/State Class.xlsx"), sheet = "State Class") %>%
#   data.frame()
# names(mySheetFull) <- names(mySheet)
# saveDatasheet(myProject, mySheetFull, sheetName)

sheetName <- "stsim_StateLabelX"
csvName <- "LULC.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_StateLabelY"
csvName <- "Subclass.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
saveDatasheet(myProject, myData, sheetName)

sheetName <- "stsim_StateClass"
csvName <- "State Class.csv"
myData <- read.csv(paste0(definitionsPath, csvName))
names(myData) <- gsub("ID", "Id", names(myData))
saveDatasheet(myProject, myData, sheetName)


## Project Definitions ---- [Needed for Flow-pathways/spin-up Transformers]

maxAge <- 300 
initialStandAge <- 0
standArea <- 1

# Age type 
sheetName <- "stsim_AgeType"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet[1,"Frequency"] <- 1
mySheet[1,"MaximumAge"] <- maxAge
saveDatasheet(myProject, mySheet, name=sheetName)

## Age group
sheetName <- "stsim_AgeGroup"
mySheet <- datasheet(myProject, name=sheetName, optional=T)
mySheet[1:(maxAge/20),"MaximumAge"] <- c(seq(from=20, to=(maxAge-1), by=20), maxAge-1)
saveDatasheet(myProject, mySheet, name=sheetName)


### Scenario Definitions ----

#### Load CBM-CFS3 Output Dependencies ---- 

### Run Control - set as default

maxTimestep <- maxAge
maxIteration <- 1
minTimestep <- 0
minIteration <- 1

myScenario <- scenario(myProject, scenario = "Run Control", folder = folderId(folder2.1) )# <- paste0("Run Control [Non-spatial; ", maxTimestep, " years; ", maxIteration, " MC]"))
sheetName <- "stsim_RunControl"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1,"MinimumIteration"] <- minIteration
mySheet[1,"MaximumIteration"] <- maxIteration
mySheet[1,"MinimumTimestep"] <- minTimestep
mySheet[1,"MaximumTimestep"] <- maxTimestep
mySheet[1,"IsSpatial"] <- FALSE
saveDatasheet(myScenario, mySheet, sheetName)

### Species Type Crosswalk 
myScenarioName <- "CBM Crosswalk - Spatial Unit and Species Type"
myScenario = scenario(myProject, scenario = myScenarioName, folder = folderId(folder2.1))
sheetName <- "lucasbuilder_CrosswalkSpecies"
saveDatasheet(myScenario, crosswalkSUSTFull, sheetName)

### Merchantable Volume Curve 
myScenarioName <- "Merchantable Volume Curves"
myScenario <- scenario(myProject, scenario = myScenarioName, folder = folderId(folder2.1))
sheetName <- "lucasbuilder_MerchantableVolumeCurve"
mySheetFull <- read.csv(paste0(initialInputsDirectory,"MerchantableVolumeCurves.csv"))
mySheetFull2 <- read.csv(paste0(initialInputsDirectory,"MerchantableVolumeCurvesRedWood.csv"))

mySheetFull <- mySheetFull %>%
  filter(StateClassId != "Forest: Redwood Group" ) %>%
  addRow(mySheetFull2) %>%
  mutate(StratumId = as.character(unique(crosswalkSUSTFull$StratumId)),
         SecondaryStratumId = as.character(unique(crosswalkSUSTFull$SecondaryStratumId))) %>%
  arrange(Age) %>% 
  rename(MerchantableVolume = Volume)

saveDatasheet(myScenario, mySheetFull, sheetName, append = F)

#### Generate Flow Multiplier Dependencies ---- 

### Initial Conditions 
myScenarioName <- "Initial Conditions"
myScenario = scenario(myProject, scenario = myScenarioName, folder = folderId(folder2.1))
sheetName <- "stsim_InitialConditionsNonSpatial"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheet[1, "TotalAmount"] <- standArea * nrow(crosswalkSUSTFull)
mySheet[1, "NumCells"] <- nrow(crosswalkSUSTFull)
mySheet[1, "CalcFromDist"] <- T
saveDatasheet(myScenario, mySheet, sheetName)

sheetName <- "stsim_InitialConditionsNonSpatialDistribution"
mySheet <- datasheet(myScenario, name = sheetName, empty = T, optional = T)
mySheetFull = data.frame()

for(i in seq(1:nrow(crosswalkSUSTFull))) {
  crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
  mySheet <- data.frame(StratumId = crosswalkSUST$StratumId,
                        SecondaryStratumId = crosswalkSUST$SecondaryStratumId,
                        StateClassId = crosswalkSUST$StateClassId,
                        AgeMin = initialStandAge,
                        RelativeAmount = standArea,
                        TSTGroupId = "Fire: High Severity [Type]",
                        TSTMin = 0, 
                        TSTMax = 0)
  mySheetFull = bind_rows(mySheetFull, mySheet) %>% unique()
}
saveDatasheet(myScenario, mySheetFull, sheetName, append = F)

#### CBM-CFS3 Spin-up Dependencies ---- 

### Transition Pathways Diagram 
# Note, only deterministic transitions are defined here.
myScenario <- scenario(myProject, scenario <- "Transition Pathways", folder = folderId(folder2.1))
sheetName <- "stsim_DeterministicTransition"
mySheet <- datasheet(myScenario, name=sheetName, optional=T, empty = T)
mySheetFull = data.frame()

for(i in seq(1:nrow(crosswalkSUSTFull))) {
  crosswalkSUST <- crosswalkSUSTFull %>% slice(i)
  myValue = crosswalkSUST$StateClassId
  mySheet = data.frame(StateClassIdSource = as.character(myValue),
                       Location = paste0("A", c(i)))
  mySheetFull = bind_rows(mySheetFull, mySheet)
} 
saveDatasheet(myScenario, mySheetFull, sheetName)

### CBM spin-up 
myScenarioName = "Spin-up"
myScenario = scenario(myProject, scenario = myScenarioName, folder = folderId(folder2.1))
sheetName = "lucasbuilder_Spinup"
sheetData = read.csv(paste0(definitionsPath,"Spin-up.csv"))
names(sheetData) <- gsub("ID","Id",names(sheetData))
saveDatasheet(myScenario, sheetData, sheetName)

###############
## RUN SETUP ##
###############
# generate run scenarios for each transformer

# # Write the console command for "Run Scenario" folder
# command <- paste0("\"", filepath(mySession), "/SyncroSim.Console.Exe\"",
#                   " --create --folder --lib=", filepath(myLibrary),
#                   " --name=3-Run-Setup --tpid=", pid)
# 
# # Invoke a system command
# sysOut <- system(command, intern=TRUE)

folder3 <- folder(ssimObject = myProject, folder = "3 - Run Setup")

### Load CBM Output
myScenarioName <- "CBM Output"
myScenario = scenario(myProject, scenario = myScenarioName, folder = folderId(folder3))
dependency(myScenario) <- c("Run Control",
                            "CBM Crosswalk - Stocks",
                            "CBM Crosswalk - Spatial Unit and Species Type")

# set "Load CBM-CFS3 Output" transformer to run for this scenario
sheetName <- "core_Pipeline"
mySheetFull <- data.frame(StageNameId = "1 - Load CBM Output",
                          RunOrder = 1)
saveDatasheet(myScenario, mySheetFull, sheetName)


### Generate Flow Multipliers
myScenarioName <- "Calculate Flow Rates"
myScenario = scenario(myProject, scenario = myScenarioName, folder = folderId(folder3))
dependency(myScenario) <- c("Run Control",
                            "CBM Crosswalk - Stocks",
                            "CBM Crosswalk - Spatial Unit and Species Type",
                            "Merchantable Volume Curves",
                            "Flow Pathways",
                            "Stock Group Membership",
                            "Flow Group Membership",
                            "Initial Stocks",
                            "Stock Flow Output Options",
                            "CBM Crosswalk - Disturbance")

# set "Flow Pathways" transformer to run for this scenario
sheetName <- "core_Pipeline"
mySheet <- datasheet(myScenario, name=sheetName, optional=T)
mySheet <- data.frame(
  StageNameId = "2 - Generate Flow Pathways",
  RunOrder = 1)
saveDatasheet(myScenario, mySheet, sheetName)


### Run Spin-up ## NOW NEEDS TO BE BROKEN IN 3 SEPARATE STAGES
myScenarioName <- "Run Spin-up"
myScenario = scenario(myProject, scenario = myScenarioName, folder = folderId(folder3))
dependency(myScenario) <- c("Flow Order",
                            "Spin-up",
                            "Transition Pathways",
                            "Calculate Flow Rates")

# set "spin-up" transformer to run for this scenario
sheetName <- "core_Pipeline"
mySheetFull <- data.frame(StageNameId = c("3 Spin-up Pre-Process (before ST-Sim)",
                                          "ST-Sim",
                                          "5 Spin-up Post-Process (after ST-Sim)"),
                          RunOrder = c(1, 2, 3))
saveDatasheet(myScenario, mySheetFull, sheetName)

# ignore dependencies for "OutputStock"
ignoreDependencies(myScenario) <- "stsim_OutputStock"

### IN UI: set ignore dependencies for "OutputStock"

##################
## Run Forecast ##
##################

# # Write the console command for "Run Scenario" folder
# command <- paste0("\"", filepath(mySession), "/SyncroSim.Console.Exe\"",
#                   " --create --folder --lib=", filepath(myLibrary),
#                   " --name=4-Run-Forecast --tpid=", pid)
# 
# # Invoke a system command
# sysOut <- system(command, intern=TRUE)

folder4 <- folder(ssimObject = myProject, folder = "4 - Run Forecast")

### Single cell
myScenarioName <- "Single Cell - No Disturbance"
myScenario = scenario(myProject, scenario = myScenarioName, folder = folderId(folder4))
dependency(myScenario) <- c("Run Control",
                            "Flow Order",
                            "Initial Conditions",
                            "Output Options [Non-spatial]",
                            "Transition Pathways",
                            "Run Spin-up")

# set "spin-up" transformer to run for this scenario
sheetName <- "core_Pipeline"
mySheetFull <- data.frame(StageNameId = c("ST-Sim"),
                          RunOrder = c(1))
saveDatasheet(myScenario, mySheetFull, sheetName)

### IN UI: set ignore dependencies for "OutputStock" and "Pipeline" 
ignoreDependencies(myScenario) <- "stsim_OutputStock"
ignoreDependencies(myScenario) <- "core_Pipeline"


##########################
## Folders in UI ##
##########################

# 1 - Run Setup Inputs 
# 2 - Run Forecast Inputs
# 3 - Run Setup
# 4 - Run Forecast

##############################
## create charts/maps in UI ##
##############################

# Charts ---
# 01 - Single Cell - Biomass
# 02 - Single Cell - Aboveground DOM
# 03 - Single Cell - Belowground DOM
# 04 - Spin-up - Biomass
# 05 - Spin-up - Aboveground DOM
# 06 - Spin-up - Belowground DOM
# 07 - Landscape - Area Burned 
# 08 - Landscape - Biomass
# 09 - Landscape - Aboveground DOM
# 10 - Landscape - Belowground DOM

# --> Stocks > Total > Disaggregate by: Stock Type/Group > Include data for: State Class (select one), Stock Type/Group (select biomass or A/B DOM types)

# Options > 
# Options [checked boxes]:
## Yaxis min to zero
## Fixed Y axis intervals
## Show legend
## Show scenario name
## Show scenario ID
## Show tooltips
## show tiles
# Format:
# 1 decimal place (y)


stockVariables <- datasheet(myProject, "stsim_StockGroup") %>%
  pull(Name)

biomass <- stockVariables[grepl("Biomass", stockVariables)]
myChart <- chart(myProject, chart="01 - Single Cell - Biomass")
myChart <- chartOptionsYAxis(myChart, minZero = TRUE, decimals=1, title = "Metric Tons C")
myChart <- chartData(myChart, type="Line", addY = biomass)

aboveground <- stockVariables[grepl("DOM: Aboveground", stockVariables)]
myChart <- chart(myProject, chart="02 - Single Cell - Aboveground DOM")
myChart <- chartOptionsYAxis(myChart, minZero = TRUE, decimals=1, title = "Metric Tons C")
myChart <- chartData(myChart, type="Line", addY = aboveground)

belowground <- stockVariables[grepl("DOM: Belowground", stockVariables)]
myChart <- chart(myProject, chart="03 - Single Cell - Belowground DOM")
myChart <- chartOptionsYAxis(myChart, minZero = TRUE, decimals=1, title = "Metric Tons C")
myChart <- chartData(myChart, type="Line", addY = belowground)


# Maps ---
# 1 - Biomass
# 2 - Flows 
# --> net growth and emissions
# 3 - Age

# in options check the "no data as zero" box for all charts except wildfire  

# # Make a console call to create/move scenarios to folders -----------
# 
# # Find the Parent Project ID to create a folder within this Project
# pid <-  project(myLibrary)$projectId[1]
# 
# # Write the console command for "Run Scenario" folder
# command <- paste0("\"", filepath(ssimSession), "/SyncroSim.Console.Exe\"",
#                   " --create --folder --lib=", filepath(myLibrary),
#                   " --name=Run-Scenarios --tpid=", pid)
# 
# # Invoke a system command
# sysOut <- system(command, intern=TRUE)
# 
# # Grab the folder ID as a variable
# folderId <- strsplit(sysOut, ": ")[[1]][2]
# 
# # Make a console call to move Scenarios to the folder
# # Pick the first Scenario in the Library to move
# sid <- scenario(myLibrary)$scenarioId[10]
# 
# # Write the console command; tfid is the folder ID flag
# command <- paste0("\"", filepath(ssimSession), "/SyncroSim.Console.Exe\"",
#                   " --move --scenario --lib=", filepath(myLibrary), " --sid=",
#                   sid, " --tfid=", folderId, " --tpid=", pid)
# sysOut <- system(command, intern=TRUE) # Invoke a system command

