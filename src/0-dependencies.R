## stsimcbmcfs3 - dependencies
## ApexRMS, Sep 2024

# Run with R-4.1.1
# This script installs missing dependencies and loads all dependencies

depend <- c("rsyncrosim", "RODBC", "tidyverse","imputeTS") 
ndepend <- length(depend)
present <- installed.packages()[ , "Package"]
needed <- depend[!(depend %in% present)] 
nneeded <- length(needed)
if(nneeded > 0){
  install.packages(needed, repos = "https://cloud.r-project.org/")
}
for(dep in 1:ndepend){
  suppressMessages(eval(bquote(library(.(depend[dep])))))
}