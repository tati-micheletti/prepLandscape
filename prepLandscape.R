## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "prepLandscape",
  description = "This module prepares landscape layers for habitat selection analyses",
  keywords = "",
  authors = structure(list(list(given = c("Julie", "W"), family = "Turner", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(prepLandscape = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "prepLandscape.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.5.9002)", "ggplot2", "terra", "sf", 'stringr', 'data.table'),
  parameters = bindrows(
    defineParameter("histLandYears", "integer", 2010:2023, NA, NA,
                    paste0("This is the year range we use past (not simulated) landscape layers.")),
    defineParameter("backgroundYr", "integer", 1900, NA, NA,
                    paste0("This is the year we fill fire and harvest rasters for time since calculations.")),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter("nbacURL", "character", 'https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/NBAC_1972to2024_20250506_shp.zip', NA, NA,
                    "URL for NBAC (National Burn Area Composite) fire database"),
    defineParameter("nfdbURL", "character", 'https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip', NA, NA,
                    "URL for National Fire Data Base (NFDB) for back filling NBAC"),
    defineParameter("rtmFuns", "character", 
                    paste0("make_landforest_prop(targetFile = targetFile, trast = rtm, buff = buff, where2save = NULL)"), 
                    NA, NA,
                    paste0("List functions to apply to rasters for moving windows or not.",
                           " Currently only applies to landcover"))
  ),
  inputObjects = bindrows(
    expectsInput(objectName = 'studyArea', objectClass = 'SpatVector',
                 desc = 'Study area of telemetry data + herd areas',
                 sourceURL = 'https://drive.google.com/file/d/1iq1f53pAtZFIFoN1RFhlXEghSbvI3Dnf/view?usp=share_link'),
    expectsInput(objectName = 'studyArea_extendedLandscape', objectClass = 'SpatVector',
                 desc = 'Extended study area for prepping landscape layers'),
    expectsInput(objectName = 'buffer', objectClass = 'numeric',
                 desc = 'Buffer for moving windows'),
    expectsInput(objectName = 'harvNTEMS', objectClass = 'SpatRaster',
                 desc = 'harvest history',
                 sourceURL = "https://opendata.nfis.org/downloads/forest_change/CA_Forest_Harvest_1985-2020.zip"),
    expectsInput(objectName = 'disturbCanLadOldType', objectClass = 'SpatRaster',
                 desc = 'CanLad disturbance type data before 1985',
                 sourceURL = 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada_disturbances_1965to1984/v1/canlad_1965_1984_disturbanceType.tif'),
    expectsInput(objectName = 'disturbCanLadOldYear', objectClass = 'SpatRaster',
                 desc = 'CanLad disturbance year data before 1985',
                 sourceURL = 'https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/canada_disturbances_1965to1984/v1/canlad_1965_1984_disturbanceYear.tif'),
    expectsInput(objectName = 'fires', objectClass = 'SpatVector',
                 desc = paste0('SpatVector of fire polygons (combined NBAC and NFDB).')),
    expectsInput(objectName = 'anthroDisturb', objectClass = 'SpatVector',
                 desc = paste0('SpatVector collection of anthropogenic disturbances excluding harvest',
                               ' This includes paved/unpaved linear features and other polygonal disturbances.')),
    expectsInput("rasterToMatch_extendedLandscapeFine", "SpatRaster",
                 desc = paste("A raster to match of the study area plus larger buffer at finest resolution.")),
    expectsInput("rasterToMatch_extendedLandscape", "SpatRaster",
                 desc = paste("A raster to match of the study area plus larger buffer at target forecasting resolution.")),
    expectsInput("rasterToMatch_extendedLandscapeCoarse", "SpatRaster",
                 desc = paste("A coarser raster to match of the study area plus large bufferto caluculate proportions of landcover.")),
    expectsInput("rtms", "list",
                 desc = paste0("List of template rasters. Only 1 if working at one resolution."))
  ),
  outputObjects = bindrows(
    createsOutput(objectName = 'landscapeYearly', objectClass = 'spatRaster',
                  desc = 'spatRaster stack of the yearly landscape layers'),
    createsOutput(objectName = 'landscape5Yearly', objectClass = 'spatRaster',
                  desc = 'spatRaster stack of the 5 yearly landscape layers'),
    createsOutput(objectName = 'studyArea', objectClass = 'SpatVector',
                 desc = 'Study area of telemetry data + herd areas'),
    createsOutput(objectName = 'studyArea_extendedLandscape', objectClass = 'SpatVector',
                 desc = 'Extended study area for prepping landscape layers'),
    createsOutput(objectName = 'buffer', objectClass = 'numeric',
                 desc = 'Buffer for moving windows'),
    createsOutput(objectName = 'harvNTEMS', objectClass = 'SpatRaster',
                 desc = 'harvest history'),
    createsOutput(objectName = 'disturbCanLadOldType', objectClass = 'SpatRaster',
                 desc = 'CanLad disturbance type data before 1985'),
    createsOutput(objectName = 'disturbCanLadOldYear', objectClass = 'SpatRaster',
                 desc = 'CanLad disturbance year data before 1985'),
    createsOutput(objectName = 'fires', objectClass = 'SpatVector',
                 desc = paste0('SpatVector of fire polygons (combined NBAC and NFDB).')),
    createsOutput(objectName = 'anthroDisturb', objectClass = 'SpatVector',
                 desc = paste0('SpatVector collection of anthropogenic disturbances excluding harvest',
                               ' This includes paved/unpaved linear features and other polygonal disturbances.')),
    createsOutput("rasterToMatch_extendedLandscapeFine", "SpatRaster",
                 desc = paste("A raster to match of the study area plus larger buffer at finest resolution.")),
    createsOutput("rasterToMatch_extendedLandscape", "SpatRaster",
                 desc = paste("A raster to match of the study area plus larger buffer at target forecasting resolution.")),
    createsOutput("rasterToMatch_extendedLandscapeCoarse", "SpatRaster",
                 desc = paste("A coarser raster to match of the study area plus large bufferto caluculate proportions of landcover.")),
    createsOutput("rtms", "list",
                 desc = paste0("List of template rasters. Only 1 if working at one resolution."))
  )
))

doEvent.prepLandscape = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "prepLandscape", "createLayers")
    },
    createLayers = {
      
      dPath <- inputPath(sim)
      
      if (is.null(sim$studyArea_extendedLandscape)){
        message("Creating studyArea_extendedLandscape...")
        sim$studyArea_extendedLandscape <- terra::buffer(sim$studyArea, 50000) |>
          Cache(.functionName = 'prep_studyArea_extendedLandscape')
      }
      # Here we digest studyArea_extendedLandscape, which is used below
      hashSA_Land <- digest::digest(sim$studyArea_extendedLandscape)
      
      if (is.null(sim$harvNTEMS)){
        sim$harvNTEMS <- createHarvNTEMS(harvNTEMSurl = extractURL("harvNTEMS"),
                                         studyArea_extendedLandscape = sim$studyArea_extendedLandscape,
                                         dPath = dPath, 
                                         hashSA_Land = hashSA_Land)
      }
      
      if (is.null(sim$rasterToMatch_extendedLandscapeFine)){
        sim$rasterToMatch_extendedLandscapeFine <- createFineRTM(studyArea_extendedLandscape = 
                                                                   sim$studyArea_extendedLandscape,
                                                                 RTM = sim$harvNTEMS)
      }
      # Here we digest rasterToMatch_extendedLandscapeFine, which is used widely below
      hashRTM_LandFine <- digest::digest(sim$rasterToMatch_extendedLandscapeFine)
      
      if (is.null(sim$rasterToMatch_extendedLandscape)){
        sim$rasterToMatch_extendedLandscape <- createExtendedRTM(rasterToMatch_extendedLandscapeFine =
                                                                   sim$rasterToMatch_extendedLandscapeFine,
                                                                 hashRTM_LandFine = hashRTM_LandFine)
      }
      
      if (is.null(sim$rasterToMatch_extendedLandscapeCoarse)){
        sim$rasterToMatch_extendedLandscapeCoarse <- createCoarseRTM(rasterToMatch_extendedLandscapeFine =
                                                                       sim$rasterToMatch_extendedLandscapeFine,
                                                                     hashRTM_LandFine = hashRTM_LandFine)
      }
      
      if (is.null(sim$rtms)){
        message("Creating rtms...")
        sim$rtms <- list(sim$rasterToMatch_extendedLandscape)
        names(sim$rtms) <- c('window240')
      }
      
      if (is.null(sim$buffer)){
        sim$buffer <- 720
      }

      if (is.null(sim$disturbCanLadOldType)){
        sim$disturbCanLadOldType <- Cache(createDisturbCanLadOldType, 
                                   disturbCanLadOldTypeURL = extractURL('disturbCanLadOldType'),
                                   rasterToMatch_extendedLandscapeFine = 
                                     sim$rasterToMatch_extendedLandscapeFine,
                                   hashRTM_LandFine = hashRTM_LandFine, 
                                   dPath = dPath,
                                   omitArgs = 'rasterToMatch_extendedLandscapeFine', 
                                   .functionName = 'load_disturbCanLadOldType_Outter')
      }
      
      if (is.null(sim$disturbCanLadOldYear)){
        sim$disturbCanLadOldYear <- Cache(createdisturbCanLadOldYear, 
          disturbCanLadOldYearURL = extractURL('disturbCanLadOldYear'),
          rasterToMatch_extendedLandscapeFine = 
            sim$rasterToMatch_extendedLandscapeFine,
          hashRTM_LandFine = hashRTM_LandFine, 
          dPath = dPath,
          omitArgs = 'rasterToMatch_extendedLandscapeFine',
          .functionName = 'load_disturbCanLadOldYear_Outter')
      }

      if (is.null(sim$fires)){
        sim$fires <- createFires(hashSA_Land = hashSA_Land,
                                 nbacURL = P(sim)$nbacURL,
                                 nfdbURL = P(sim)$nfdbURL,
                                 dPath = dPath,
                                 studyArea_extendedLandscape = sim$studyArea_extendedLandscape)
      }
      
      if (is.null(sim$anthroDisturb)){
        sim$anthroDisturb <- createAnthroDisturbance(dPath = dPath, 
                                                     dataPath = dataPath(sim), 
                                                     studyArea_extendedLandscape = 
                                                       sim$studyArea_extendedLandscape, 
                                                     hashSA_Land = hashSA_Land)
      }
      message("Digest: rasterToMatch")
      hashRTM_Land <- digest::digest(sim$rasterToMatch)
      message("Digest: fires")
      hashfires <- digest::digest(sim$fires)
      message("Digest: harvNTEMS")
      hashHarvNTEMS <- digest::digest(sim$harvNTEMS)
      message("Digest: disturbCanLadOldYear")
      hashDisturbCanLadOldYear <- digest::digest(sim$disturbCanLadOldYear)
      message("Digest: disturbCanLadOldType")
      hashDisturbCanLadOldType <- digest::digest(sim$disturbCanLadOldType)

      sim$landscapeYearly <- prep_everything(# Small Inputs 
                                             histLandYears = Par$histLandYears, 
                                             backgroundYr = Par$backgroundYr,
                                             rtmFuns = Par$rtmFuns,
                                             bufferRTM = sim$buffer,
                                             dPath = dPath,
                                             # Large inputs (should be omitted)
                                             fires = sim$fires, 
                                             rasterToMatch = sim$rasterToMatch_extendedLandscape,
                                             rtms = sim$rtms,
                                             harvNTEMS = sim$harvNTEMS, 
                                             disturbCanLadOldYear = sim$disturbCanLadOldYear,
                                             disturbCanLadOldType = sim$disturbCanLadOldType, 
                                             # Hashes
                                             hashRTM_Land = hashRTM_Land,
                                             hashfires = hashfires,
                                             hashHarvNTEMS = hashHarvNTEMS,
                                             hashDisturbCanLadOldYear = hashDisturbCanLadOldYear,
                                             hashDisturbCanLadOldType = hashDisturbCanLadOldType)
      # |> Cache(.functionName = 'prep_yearly', 
      #         omitArgs = c('rasterToMatch', 'rtms', "fires", "harvNTEMS", 
      #                      "disturbCanLadOldYear", "disturbCanLadOldType"))
      #                      FOR SOME REASON NOT WORKING, ERROR ON path?!

      sim$landscape5Yearly <- sim$anthroDisturb
      
      # print("SAVE OUTPUTS!")
      # writeRaster(x = sim$landscapeYearly[[1]], 
      #             filename = file.path(outputPath(sim), "Covariates/harvNTEMS.tif"))
      # 
      # lapply(names(sim$landscapeYearly[[2]][[1]]), function(RAS){
      #   writeRaster(x = sim$landscapeYearly[[2]][[1]][[RAS]], 
      #               filename = file.path(outputPath(sim), paste0("Covariates/", RAS, ".tif")))
      #   print(paste0("SAVED: ", RAS))
      # })
      
    },

    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", inputPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  # TODO give a smaller area
  if (!suppliedElsewhere("studyArea", sim)){
    message("StudyArea not supplied. Using BC-NWT example.")
    sim$studyArea <- reproducible::prepInputs(url = extractURL("studyArea"),
                                              destinationPath = dataPath(sim),
                                              targetFile = "studyArea_bcnwt_4sims.shp",
                                              alsoExtract = "similar", fun = "terra::vect") |>
      Cache()
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


