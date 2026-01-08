
prep_everything <- function(histLandYears, fires, rasterToMatch, rtms, rtmFuns, backgroundYr, bufferRTM,
                            harvNTEMS, disturbCanLadOldYear, disturbCanLadOldType, dPath, hashfires,
                            hashRTM_Land, hashHarvNTEMS, hashDisturbCanLadOldYear, hashDisturbCanLadOldType,
                            ...) {

  # load disturbances
  ##### FIRES ----
  histFire <- make_timeSinceDisturb_rast(layer = fires, 
                                         rast = rasterToMatch,
                                         disturbanceType = 'Fire',
                                         minyr = min(histLandYears), #maxyr = max(histLandYears),
                                         backgrndYear = backgroundYr, 
                                         where2save = NULL,
                                         hashfires = hashfires,
                                         hashRTM_Land = hashRTM_Land
                                         ) |>
    Cache(omitArgs = c('rast', "layer"), .functionName = 'makeTimeSinceFire')
  
  ##### HARVEST -----
  harvNTEMS[harvNTEMS==0]<-NA
  
  # get before 1985 harvest from CanLaD -- get year only for harv
  disturbCanLadOldHarvYearFine <- terra::mask(x = disturbCanLadOldYear, 
                                              mask = terra::match(disturbCanLadOldType, 3)) |>
    Cache(.cacheExtra = list(hashDisturbCanLadOldYear = hashDisturbCanLadOldYear,
                             hashDisturbCanLadOldType = hashDisturbCanLadOldType), 
          .functionName = 'makeCanLadHarvOldFine', omitArgs = c("x", "mask"))

  disturbCanLadOldHarvYear <- reproducible::postProcess(x = disturbCanLadOldHarvYearFine, 
                                                        to = rasterToMatch,
                                                        hashRTM_Land = hashRTM_Land)|>
    Cache(.functionName = 'makeCanLadHarvOld',
          omitArgs = c('to', 'x'))
  
  # message('gathered before 1985 harvest from CanLaD')

  # add recent harvest after NTEMS
  maxHarvNTEMS <- (max(terra::values(harvNTEMS), na.rm = T)+1)
  newYears <-  seq(maxHarvNTEMS, max(histLandYears))

  newHarvRast <- make_CanLad_cumulative(yrs = newYears, disturbTypeCode = 2,
                                        dPath = dPath, rtm = rasterToMatch) |>
    Cache(omitArgs = 'rtm', .functionName = 'makeNewHarvRast')
  
  message('Adding recent harvest after NTEMS...')
  
  # combine all types together
  harvNTEMScoarse <- reproducible::postProcess(x = harvNTEMS, 
                                               to = rasterToMatch,
                                               hashRTM_Land = hashRTM_Land,
                                               hashHarvNTEMS = hashHarvNTEMS)|>
    Cache(.functionName = 'makeHarvNTEMScoarse', omitArgs = c('to', "x"))

  harvs <- c(disturbCanLadOldHarvYear, harvNTEMScoarse, newHarvRast)
  names(harvs) <- c('CanLadOld', 'NTEMS', 'CanLadNew')

  timeSinceHarvest <- Map(nn = paste0('year', histLandYears), yr = histLandYears,
                          rtm = rasterToMatch, background = backgroundYr,
                          hashRTM_Land = hashRTM_Land,
                          f = function(nn, yr, rtm, background, hashRTM_Land){
                            harvsYrPos <- clamp(harvs, upper = yr, value = F)
                            maxRast <- max(harvsYrPos, na.rm = T)
                            timeSince <- yr - maxRast
                            backgrnd <- yr - background
                            timeSinceFill <- mask(ifel(is.na(timeSince), backgrnd, timeSince), rtm)
                            names(timeSinceFill) <- 'timeSinceHarvest'
                            return(timeSinceFill)
                          })|>
    Cache(omitArgs = 'rtm', .functionName = 'prep_timeSinceHarv')

  message('Combined all harvest data!')

  # 1. individual Prop Window
  indPropWindow <- function(eachHistLandYear, dPath, rtmFuns, rtm, rtmname, buff) {
    URL <- paste0("https://opendata.nfis.org/downloads/",
                  "forest_change/CA_forest_VLCE2_", 
                  eachHistLandYear, ".zip")
    
    exists <- checkLinkExists(URL, response = "warning")
    
    if (exists){
      propWindow <- Cache(reproducible::prepInputs, 
                          url = URL,
                          targetFile = paste0("CA_forest_VLCE2_", 
                                              eachHistLandYear, ".tif"),
                          destinationPath = dPath, 
                          buff = buff,
                          rtmFuns = rtmFuns,
                          make_landforest_prop = make_landforest_prop,
                          fun = eval(parse(text = rtmFuns)), 
                          rtm = rtm, 
                          .functionName = paste0(rtmname, eachHistLandYear, '_propLand'))
      return(propWindow)
    } else {
      return(NULL)
    }
  }

  # 2. All Prop Windows (Iterates over Years for a specific RTM)
  allPropWindows <- function(rtm, rtmname, rtmFuns, dPath, buff) {
    yearsList <- Map(f = indPropWindow, 
                      eachHistLandYear = histLandYears, # This iterates
                      rtm = rtm,         # This is constant for this loop
                      rtmname = rtmname, # This is constant for this loop
                      rtmFuns = rtmFuns,   # This is constant for this loop
                      dPath = dPath,
                     buff = buff)     # This is constant
    names(yearsList) <- paste0("year", histLandYears)
    return(yearsList)
  }
  
  # 3. For all rtm functions and RTMs (Iterates over the RTMs)
  histLand <- Map(f = allPropWindows,
                  rtm = rtms,
                  rtmname = names(rtms),
                  MoreArgs = list(rtmFuns = rtmFuns,
                                  dPath = dPath, 
                                  buff = bufferRTM)) |> # If this becomes a list of functions, they need to come out of the moreArgs argument! 
    Cache(.functionName = 'make_landforest_prop',
          omitArgs = 'rtm')

  histLand <- lapply(histLand, function(win) {
    if (is.list(win)) {
      Filter(Negate(is.null), win)
    } else {
      win
    }
  })
  
  landscapeYearly <- lapply(names(rtms), function(nRtms){
    nHistLand <- names(histLand[[nRtms]])
    windowly <- lapply(nHistLand, function(eachHistLand) {
      yearly <- c(histLand[[nRtms]][[eachHistLand]],
                  timeSinceHarvest[[eachHistLand]], 
                  histFire[[eachHistLand]])
      return(yearly)
    }) |> setNames(nHistLand)  
  }) |> setNames(names(rtms))
  
  return(list(harvNTEMS = harvNTEMS, landscapeYearly = landscapeYearly))
}
