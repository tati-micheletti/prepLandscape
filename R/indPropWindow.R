indPropWindow <- function(eachHistLandYear, dPath, rtmFun, rtm){
  propWindow <- Cache(reproducible::prepInputs, 
                      url = paste0("https://opendata.nfis.org/downloads/",
                                   "forest_change/CA_forest_VLCE2_", eachHistLandYear, 
                                   ".zip"),
                      destinationPath = dPath, # end pre process
                      fun = eval(parse(text = rtmFun)), # end process
                      rtm = rtm)
  return(propWindow)
}
