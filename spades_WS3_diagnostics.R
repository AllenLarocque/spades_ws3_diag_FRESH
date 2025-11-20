defineModule(sim, list(
  name = "spades_ws3_diagnostics",
  description = "Module to plot fire and harvest time series after simulation completes",
  keywords = c("plotting", "visualization", "fire", "harvest"),
  authors = c(person("Allen", "Larocque", email = NA, role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5.9000", fireHarvestPlots = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list("ggplot2", "data.table", "SpaDES.core"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter("resInHA", "numeric", NULL, NA, NA, "Resolution in hectares. If NULL, will be calculated from rasterToMatch")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "burnSummary", objectClass = "data.table", desc = "Fire burn summary data table"),
    expectsInput(objectName = "harvestStats", objectClass = "data.frame", desc = "Harvest statistics data frame"),
    expectsInput(objectName = "rasterToMatch", objectClass = "SpatRaster", desc = "Template raster for resolution calculation")
  ),
  outputObjects = bind_rows(
  )
))

## event types

doEvent.fireHarvestPlots = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      # Schedule plot event at the end of simulation to run after all other modules
      # Using low priority to ensure it runs after other events at the same time
      sim <- scheduleEvent(sim, end(sim), "fireHarvestPlots", "plot", eventPriority = -10)
    },
    plot = {
      sim <- Plot(sim)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions

Init <- function(sim) {
  # Source the plotting function
  plotScriptPath <- file.path(modulePath(sim), "..", "R", "simplePlot.R")
  if (file.exists(plotScriptPath)) {
    source(plotScriptPath)
  } else {
    # Try alternative path
    plotScriptPath <- file.path(getwd(), "R", "simplePlot.R")
    if (file.exists(plotScriptPath)) {
      source(plotScriptPath)
    } else {
      warning("Could not find simplePlot.R script. Plotting may fail.")
    }
  }

  return(invisible(sim))
}

Plot <- function(sim) {
  # Check that required objects exist
  if (is.null(sim$burnSummary)) {
    warning("burnSummary not found. Cannot create plot.")
    return(invisible(sim))
  }
  if (is.null(sim$harvestStats)) {
    warning("harvestStats not found. Cannot create plot.")
    return(invisible(sim))
  }
  if (is.null(sim$rasterToMatch)) {
    warning("rasterToMatch not found. Cannot create plot.")
    return(invisible(sim))
  }

  # Call the plotting function
  plotFireWithHarvest(sim, resInHA = P(sim)$resInHA)

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  return(invisible(sim))
}

