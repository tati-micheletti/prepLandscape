createdisturbCanLadOldYear <- function(disturbCanLadOldYearURL, 
                                       dPath,
                                       rasterToMatch_extendedLandscapeFine,
                                       hashRTM_LandFine){
  message("Creating disturbCanLadOldYear...")
  disturbCanLadOldYear <- reproducible::prepInputs(url = disturbCanLadOldYearURL,
                                                   destinationPath = dPath,
                                                   alsoExtract = "similar", 
                                                   fun = "terra::rast",
                                                   to = rasterToMatch_extendedLandscapeFine,
                                                   method = 'near',
                                                   hashRTM_LandFine = hashRTM_LandFine) |>
    Cache(omitArgs = 'to', .functionName = 'load_disturbCanLadOldYear')
  return(disturbCanLadOldYear)
}