createDisturbCanLadOldType <- function(disturbCanLadOldTypeURL, 
                                       dPath,
                                       rasterToMatch_extendedLandscapeFine,
                                       hashRTM_LandFine){
  message("Creating disturbCanLadOldType...")
  disturbCanLadOldType <- reproducible::prepInputs(url = disturbCanLadOldTypeURL,
                                                   destinationPath = dPath,
                                                   alsoExtract = "similar", 
                                                   fun = "terra::rast",
                                                   to = rasterToMatch_extendedLandscapeFine,
                                                   method = 'near',
                                                   hashRTM_LandFine = hashRTM_LandFine) |>
    Cache(omitArgs = 'to', .functionName = 'load_disturbCanLadOldType')
  return(disturbCanLadOldType)
}