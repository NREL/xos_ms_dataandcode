# Purpose: Develops the two functions that process the NIRS Transflectance Spectra.
#   Order of operations for signal processing: 
#   1. Spectra are Truncated to Region of Interest (For)
#   2. Spectra are corrected for light scatter via Standard Normal Variate Algorithm
#   3. Spectra are derivitized and smoothed using Savitsky Golay Algorithm
#   4. Additional Truncation is performed to more specified region of interest (preprocess_spectra_overtone ONLY)


# Build Function to process spectra for use in full region models: 
# parameters that can be altered reger to savitsky golay algorithm options
#   theDiff: the order of derivative to use for signal processing
#   thePoly: the order of polynomial to fit the data to
#   theWindow: the number of points to use when fitting the data

preprocess_spectra <- function(data, theDiff=1, thePoly =2, theWindow = 7){
  # make a copy of the data   
  spec <- data 
  # make a vector that includes the names of the spectra columns of interest for processing (truncating spectra as part of process)
  speccols <- colnames(spec)[which(colnames(spec)=="1350.00"):which(colnames(spec)=="2499.50")] 
  # subset the data frame to include only the spectra columns of interest
  spec <- spec %>% select(all_of(speccols))
  # perform standard normal variate algorithm and savitksy golay algorithm on spectra
  # data is output as a matrix by these algorithms, translate it back into data frame form
  spec_SNV_SG <- spec %>% 
    prospectr::standardNormalVariate() %>%
    prospectr::savitzkyGolay(m = theDiff, p = thePoly, w = theWindow) 
  return(spec_SNV_SG)
}

# Build Function to process spectra for use in overtone region models: 
# parameters that can be altered reger to savitsky golay algorithm options
#   theDiff: the order of derivative to use for signal processing
#   thePoly: the order of polynomial to fit the data to
#   theWindow: the number of points to use when fitting the data

preprocess_spectra_overtone <- function(data, theDiff=1, thePoly =2, theWindow = 7){
# make a copy of the data   
  spec <- data 
# make a vector that includes the names of the spectra columns of interest for processing (truncating spectra as part of process)
  speccols <- colnames(spec)[which(colnames(spec)=="1350.00"):which(colnames(spec)=="2499.50")]
# subset the data frame to include only the spectra columns of interest
  spec <- spec %>% select(all_of(speccols)) #select out the spectra information
# perform standard normal variate algorithm and savitksy golay algorithm on spectra
# data is output as a matrix by these algorithms, translate it back into data frame form
  spec_SNV_SG <- spec %>% 
    prospectr::standardNormalVariate() %>%
    prospectr::savitzkyGolay(m = theDiff, p = thePoly, w = theWindow) %>% data.frame() 
#fix column names from data frame transformation (adds X to beginning)
  colnames(spec_SNV_SG) <- as.numeric(substring(colnames(spec_SNV_SG),2))
# create a character vector of column names to remove from analysis (eg- anything outside of 2100-2450nm window)
  truncs <- c(seq(400,2100, by =.5),seq(2450,2499.5, by =.5)) %>% as.character()
# create a vector of spectra column names based on SNV-SG adjusted column names
  cols <- colnames(spec_SNV_SG)
# subset spectra column name list based on what columns are outside the 2100-2450nm window (as defined by the character vector 'truncs' )
  cols <- cols[!cols %in% truncs]
# select only truncated spectra columns 
  spec_SNV_SG_t <- spec_SNV_SG %>% 
    select(all_of(cols))%>% as.matrix()
  return(spec_SNV_SG_t)
}
