defineModule(sim, list(
  name = "spades_ws3_diagnostics",
  description = "Module to plot fire and harvest time series after simulation completes",
  keywords = c("plotting", "visualization", "fire", "harvest"),
  authors = c(person("Allen", "Larocque", email = NA, role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5.9000", spades_ws3_diagnostics = "0.0.1"),
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

doEvent.spades_ws3_diagnostics = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)
      # Schedule plot event at the end of simulation to run after all other modules
      sim <- scheduleEvent(sim, end(sim), "spades_ws3_diagnostics", "plot", eventPriority = .last())
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
  return(invisible(sim))
}

# Add summary tables to the simList




## Plotting functions:

# Plot age class structure:
# At start, at end, at each year, as an animation through time, as a 3-D manifold


# Plot fire and harvest time series:
plotFireWithHarvest <- function(sim, resInHA = NULL) {
  if (is.null(resInHA)) {
    resInHA = as.integer(prod(res(sim$rasterToMatch))/1e4)
  }
  fire <- sim$burnSummary                                       # Bring in the fire data
  fireByYear <- fire[, .(area = sum(areaBurned)), .(year)]      # Aggregate by taking the sum for every year across the landscape
  fireByYear[, source := "fire"]                                # add a 'source' column

  harvest <- data.table::copy(as.data.table(sim$harvestStats))  # Bring in the harvest stats data
  harvest[, area := ws3_harvestArea_pixels * resInHA]           # Calculate the area harvested by multiplying the pixels by the res
  harvest[, source := "harvest"]                                # Add a 'source' column
  plotData <- rbind(harvest, fireByYear, fill = TRUE)           # Combine the two and make a new df.

  # Plot it:
  p1 <- ggplot2::ggplot(plotData, ggplot2::aes(x = year, y = area, col = source)) +
    ggplot2::geom_line() +
    ggplot2::geom_smooth(method = "lm", se = FALSE) +
    ggplot2::scale_color_manual(values = c("red", "darkgreen")) +
    ggplot2::labs(
      y = "area disturbed (ha)",
      title = paste(
        "Basenames:",
        paste(unlist(sim@params$spades_ws3$basenames), collapse = ", "),
        "; planning_period_freq = ",
        sim@params$spades_ws3$planning_period_freq
      )
    )

  # Use outputPath from sim instead of hardcoded path
  #outputDir <- file.path(outputPath(sim), "figures")
  #if (!dir.exists(outputDir)) {
  #  dir.create(outputDir, recursive = TRUE)
  #}

  SpaDES.core::Plots(data = p1,
                     types = c("png"),
                     filename = file.path(figurePath(sim), "TimeSeries-Fire_Harvest"))
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

