createFineRTM <- function(studyArea_extendedLandscape,
                          RTM){
  message("Creating rasterToMatch_extendedLandscapeFine...")
  hashVal <- fastdigest::fastdigest(studyArea_extendedLandscape)
  rasterToMatch_extendedLandscapeFine <- terra::rasterize(x = studyArea_extendedLandscape,
                                                          y = RTM, vals = 1)|>
    Cache(.functionName = 'prep_rasterToMatch_extendedLandscapeFine_Rasterize',
          omitArgs = c("x", "y"))
  hashValX <- fastdigest::fastdigest(rasterToMatch_extendedLandscapeFine)
  rasterToMatch_extendedLandscapeFine2 <- terra::mask(x = rasterToMatch_extendedLandscapeFine,
                                                     mask = studyArea_extendedLandscape)|>
    Cache(.cacheExtra = list(hashValX = hashValX,
                              hashValMask = hashVal), 
          .functionName = 'prep_rasterToMatch_extendedLandscapeFine_mask', 
          omitArgs = c("x", "mask"))
  return(rasterToMatch_extendedLandscapeFine2)
}