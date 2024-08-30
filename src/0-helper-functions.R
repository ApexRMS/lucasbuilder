## stsimcbmcfs3 - helper functions
## ApexRMS, January 2022

# Run with R-4.1.1
# This script loads helper functions used by transformer scripts

cut_label <- function(labels, side = "left", sep = ":"){
  
  if (sum(is.na(labels)) == 0){
    splitted <- sapply(labels, stringr::str_split, pattern = sep)
  } else{
    stop("NAs in labels")
  }
  
  if(side == "left"){
    indexed <- sapply(sapply(splitted, `[[`, id))
  } else if(side == "right") {
    indexed <- mapply(`[[`, splitted, sapply(splitted, length))
  } else {
    stop("Argument <side> must be one of 'left' or 'right'")
  }
  
  output <- unname(sapply(indexed, stringr::str_trim))
  
  return(output)
}

strip_type <- function(labels) {
  
  output <- sapply(labels, function(x){gsub(x, pattern = " \\[Type\\]", replacement = "")})
  
  return(output)
}

paste_type <- function(labels) {
  
  output <- sapply(labels, function(x){paste0(x, " [Type]")})
  
  return(output)
}

is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))
}
