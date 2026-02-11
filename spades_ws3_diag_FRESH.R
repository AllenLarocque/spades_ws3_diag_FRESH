## Description:
# It outputs diagnostic plots for the FRESH datalad pipeline.

defineModule(sim, list(
  name = "spades_ws3_diag_FRESH",
  description = "Module to plot harvest diagnostic plots at the end of simulation",
  keywords = c("plotting", "visualization", "WS3", "harvest"),
  authors = c(person("Allen", "Larocque", email = NA, role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5.9000", spades_ws3_diagnostics = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list(),
  documentation = list(),
  reqdPkgs = list("ggplot2", "data.table", "SpaDES.core", "terra"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter("resInHA", "numeric", NULL, NA, NA, "Resolution in hectares. If NULL, will be calculated from rasterToMatch")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "burnSummary", objectClass = "data.table", desc = "Fire burn summary data table"),
    expectsInput(objectName = "harvestStats", objectClass = "data.frame", desc = "Harvest statistics data frame"),
    expectsInput(objectName = "rasterToMatch", objectClass = "SpatRaster", desc = "Template raster for resolution calculation"),
    expectsInput(objectName = "landscape", objectClass = "SpatRaster", desc = "Landscape SpatRaster with age layer")
  ),
  outputObjects = bind_rows(
  )
))

## event types

doEvent.spades_ws3_diag_FRESH = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      # Schedule plot event every year starting from plotInitialTime
      if (!is.na(P(sim)$.plotInitialTime)) {
        sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "spades_ws3_diag_FRESH", "plot")
      }
      # Also schedule at end for final plots
      sim <- scheduleEvent(sim, end(sim), "spades_ws3_diag_FRESH", "plot", eventPriority = .last())
    },
    plot = {
      sim <- Plot(sim)
      # Schedule next plot event if interval is specified
      if (!is.na(P(sim)$.plotInterval) && time(sim) < end(sim)) {
        sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "spades_ws3_diag_FRESH", "plot")
      }
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions

Init <- function(sim) {
  return(invisible(sim))
}


# Plot harvest time series:
plotHarvest <- function(sim, resInHA = NULL) {
  if (is.null(resInHA)) {
    resInHA = as.integer(prod(res(sim$rasterToMatch))/1e4)
  }

  harvest <- data.table::copy(as.data.table(sim$harvestStats))  # Bring in the harvest stats data
  harvest[, area := ws3_harvestArea_pixels * resInHA]           # Calculate the area harvested by multiplying the pixels by the res
  harvest[, source := "harvest"]                                # Add a 'source' column
  plotData <- harvest

  # Plot it:
  p1 <- ggplot2::ggplot(plotData, ggplot2::aes(x = year, y = area, col = source)) +
    ggplot2::geom_line() +
    ggplot2::geom_smooth(method = "lm", se = FALSE) +
    ggplot2::scale_color_manual(values = c("darkgreen")) +
    ggplot2::labs(
      y = "area disturbed (ha)",
      title = paste(
        "Basenames:",
        paste(unlist(sim@params$spades_ws3$basenames), collapse = ", "),
        "; planning_period_freq = ",
        sim@params$spades_ws3$planning_period_freq
      )
    )

  SpaDES.core::Plots(data = p1,
                     types = c("png"),
                     filename = file.path(figurePath(sim), "TimeSeries_HarvestByArea"))
}

Plot <- function(sim) {
  # Plot age class structure every year
  plotAgeClassStructureByArea(sim, resInHA = P(sim)$resInHA)

  # Plot age class structure by volume every year (period will be calculated automatically)
  plotAgeClassStructureByVolume(sim, period = NULL, yname = "totvol")

  # Plot harvest time series (only at end to avoid redundant plots)
  if (time(sim) >= end(sim)) {
    if (is.null(sim$harvestStats)) {
      warning("harvestStats not found. Cannot create harvest plot.")
    } else if (is.null(sim$rasterToMatch)) {
      warning("rasterToMatch not found. Cannot create harvest plot.")
    } else {
      plotHarvest(sim, resInHA = P(sim)$resInHA)
    }
  }

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  return(invisible(sim))
}

