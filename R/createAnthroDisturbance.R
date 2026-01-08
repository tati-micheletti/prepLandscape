createAnthroDisturbance <- function(dPath, dataPath, studyArea_extendedLandscape, hashSA_Land){
  message("Creating anthropogenic disturbance layers...")
  anthroDisturb <- prep_anthroDisturbance(inputsPath = dPath, 
                                              studyArea = studyArea_extendedLandscape,
                                              dataPath = dataPath, 
                                              source = 'ECCC',
                                              hashSA_Land = hashSA_Land) |>
    Cache(omitArgs = c('studyArea', 'inputsPath', 'dataPath'), 
          .functionName = 'prep_anthroDisturbance', useCloud = FALSE)
  return(anthroDisturb)
}
