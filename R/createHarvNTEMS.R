createHarvNTEMS <- function(harvNTEMSurl, studyArea_extendedLandscape,
                            dPath, hashSA_Land){
  message("Creating harvNTEMS...")
  harvNTEMS <- reproducible::prepInputs(url = harvNTEMSurl,
                                            destinationPath = dPath,
                                            to = studyArea_extendedLandscape,
                                            fun = 'terra::rast',
                                            method = 'near',
                                        hashSA_Land = hashSA_Land) |>
    Cache(.functionName = 'prepInputs_harvNTEMS', omitArgs = "to")
  return(harvNTEMS)
  
}