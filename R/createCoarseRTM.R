createCoarseRTM <- function(rasterToMatch_extendedLandscapeFine,
                            hashRTM_LandFine) {
  message("Creating rasterToMatch_extendedLandscapeCoarse...")
  rasterToMatch_extendedLandscapeCoarse <- terra::aggregate(rasterToMatch_extendedLandscapeFine,
                                                            fact = 16,
                                                            hashRTM_LandFine = hashRTM_LandFine)|>
    Cache(omitArgs = 'x', .functionName = 'prep_rasterToMatch_extendedLandscapeCoarse')
  return(rasterToMatch_extendedLandscapeCoarse)
}