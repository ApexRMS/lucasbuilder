## stsimcbmcfs3 - spinup post
## ApexRMS, August 2024

# Run with R-4.1.1
# Spin-up post-processing script

# Source helper functions
pkg_dir <- (Sys.getenv("ssim_package_directory"))
source(file.path(pkg_dir, "0-dependencies.R"))
source(file.path(pkg_dir, "0-helper-functions.R"))

myLibrary <- ssimLibrary()
myProject <- project()
myScenario <- scenario()

# (1) Extract source and destination datasheets ---------------------------

spinup <- datasheet(myScenario, "lucasbuilder_Spinup", optional=TRUE) %>% 
  mutate_if(is.factor, as.character)

initial_stocks <- datasheet(myScenario, "stsim_InitialStockNonSpatial") %>% 
  mutate_if(is.factor, as.character)

output_stocks <- datasheet(myScenario, "stsim_OutputStock") %>%
  mutate_if(is.factor, as.character) %>% 
  mutate(StockTypeId = strip_type(StockGroupId)) %>% 
  left_join(initial_stocks, by = "StockTypeId")

# remove all entries with no match in the initial_stocks table
# To see what is removed:
# output_stocks %>% filter(is.na(StateAttributeTypeId)) %>% pull(StockTypeId) %>% table

output_stocks_noNA <- output_stocks %>% drop_na(StateAttributeTypeId)

state_attributes <- datasheet(myScenario, "stsim_StateAttributeValue", optional = T) %>% 
  mutate_if(is.factor, as.character) %>% 
  filter(!str_detect(StateAttributeTypeId, "Carbon Initial Conditions"))

# (2) Wrangle outputs into state attribute table --------------------------

spinup_unique <- unique(spinup)
nrow_unique <- nrow(spinup_unique)

state_attributes_final <- state_attributes

for (rownb in 1:nrow_unique){
  
  # Determine spinup duration for this cell
  the_row <- slice(spinup, rownb)
  
  nb_cycles <- the_row$SpinupDuration
  interval_dist <- the_row$ReturnInterval
  spinup_duration <- nb_cycles*interval_dist
  last_cycle_duration <- the_row$MaxAgeForLastCycle
  TSTGroup <- the_row$MostRecentDisturbanceTGId
  stratum <- the_row$StratumId
  secondary_stratum <- the_row$SecondaryStratumId
  tertiary_stratum <- the_row$TertiaryStratumId
  state_class <- the_row$StateClassId
  
  # If primary stratum is blank create a new primary stratum called "All" (changed to "[Unspecified]")
  # NB: this primary stratum was added to project definitions in spinup_pre.R
  remove_stratum <- FALSE # keep track of whether stratum needs to be removed later
  if(is.na(stratum)){
    stratum <- "[Unspecified]" # "All"
    remove_stratum <- TRUE # remove stratum from state attribute datasheet before saving
  }
  
  if(is.na(secondary_stratum)){
    output_stocks_filtered_secondary_stratum <- output_stocks_noNA
  } else{
    output_stocks_filtered_secondary_stratum <- output_stocks_noNA %>% 
      filter(SecondaryStratumId == secondary_stratum)
  }
  
  output_stocks_filtered <- output_stocks_filtered_secondary_stratum %>% 
    filter(Timestep >= spinup_duration, 
           Timestep <= (spinup_duration + last_cycle_duration),
           StratumId == stratum,
           TertiaryStratumId == tertiary_stratum,
           StateClassId == state_class) %>% 
    mutate(TSTMin = Timestep - spinup_duration, 
           TSTMax = TSTMin, 
           TSTGroupId = TSTGroup,
           TertiaryStratumId = NA) %>%
    rename(Value = Amount) %>% 
    select(-c(StockGroupId, StockTypeId, TertiaryStratumId))
  
  if(remove_stratum){
    output_stocks_filtered$StratumId = NA
  }
  
  state_attributes_final <- state_attributes_final %>% 
    bind_rows(output_stocks_filtered)
  
}

state_attributes_final$Iteration = NA
state_attributes_final$Timestep = NA

# Save 
saveDatasheet(myScenario, data = unique(state_attributes_final), 
              name = "stsim_StateAttributeValue")