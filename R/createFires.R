createFires <- function(nbacURL, nfdbURL, dPath, studyArea_extendedLandscape, hashSA_Land){
  message("Creating fires...")
  fires <- combine_fire_DB(nbacURL, nfdbURL, dPath,
                               studyArea = studyArea_extendedLandscape,
                               studyAreaName = Par$.studyAreaName,
                               savePath = NULL,
                           hashSA_Land = hashSA_Land) |>
    Cache(omitArgs = 'studyArea', .functionName = 'combine_fire_DB')
  return(fires)
}