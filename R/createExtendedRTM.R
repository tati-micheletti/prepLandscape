createExtendedRTM <- function(rasterToMatch_extendedLandscapeFine,
                              hashRTM_LandFine){
  message("Creating rasterToMatch_extendedLandscape...")
  rasterToMatch_extendedLandscape <- terra::aggregate(x = rasterToMatch_extendedLandscapeFine,
                                                          fact = 8,
                                                      hashRTM_LandFine = hashRTM_LandFine)|>
    Cache(omitArgs = 'x', .functionName = 'prep_rasterToMatch_extendedLandscape')
  return(rasterToMatch_extendedLandscape)
}