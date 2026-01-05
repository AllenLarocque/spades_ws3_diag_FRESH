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

# Add summary tables to the simList




## Plotting functions:

# Access inventory: (something like:)
# Plot <- function(sim) {
#   # Access the forest model
#   fm <- sim$fm
#
#   # Call Python methods via reticulate
#   total_area <- py$fm$inventory(0)
#   periods <- py$fm$periods
#
#   # Or use the R object (reticulate handles conversion)
#   # ...
# }


# Plot age class structure:
plotAgeClassStructureByArea <- function(sim, resInHA = NULL) {
  if (is.null(sim$landscape)) {
    warning("landscape not found. Cannot create age class structure plot.")
    return(invisible(NULL))
  }

  if (!"age" %in% names(sim$landscape)) {
    warning("landscape does not have an 'age' layer. Cannot create age class structure plot.")
    return(invisible(NULL))
  }

  # Get age values from the landscape raster
  ageRaster <- sim$landscape[["age"]]
  ageValues <- terra::values(ageRaster)
  ageValues <- ageValues[!is.na(ageValues)]  # Remove NA values

  if (length(ageValues) == 0) {
    warning("No valid age values found in landscape. Cannot create age class structure plot.")
    return(invisible(NULL))
  }

  # Calculate resolution in hectares if not provided
  if (is.null(resInHA)) {
    if (!is.null(sim$rasterToMatch)) {
      resInHA <- as.numeric(prod(terra::res(sim$rasterToMatch)) / 1e4)  # This is the resolution in hectares
    } else {
      resInHA <- as.numeric(prod(terra::res(ageRaster)) / 1e4)
    }
  }


  # Bin ages into 10-year bins
  maxAge   <- max(ageValues, na.rm = TRUE)
  ageBreaks <- seq(0, ceiling(maxAge / 10) * 10, by = 10)  # Create bins that end in a multiple of 10

  # Determine padding width: e.g., 2 for up to 99, 3 for 100+, etc.
  padWidth <- nchar(max(ageBreaks) - 1)

  # Build labels with zero-padding, e.g., "00-09", "10-19", "100-109"
  ageStarts <- ageBreaks[-length(ageBreaks)]
  ageEnds   <- ageBreaks[-1] - 1
  ageLabels <- sprintf("%0*d-%0*d", padWidth, ageStarts, padWidth, ageEnds)

  # Create age bins (left-closed, right-open): [start, end+1)
  ageBinned <- cut(
    ageValues,
    breaks = ageBreaks,
    labels = ageLabels,
    include.lowest = TRUE,
    right = FALSE
  )

  # Count pixels in each bin and convert to area (hectares)
  ageCounts <- table(ageBinned)
  ageArea   <- as.numeric(ageCounts) * resInHA

  # Create data frame for plotting (preserve factor order)
  plotData <- data.frame(
    ageClass = factor(names(ageCounts), levels = ageLabels),  # keep bins in correct order  ageClass = factor(names(ageCounts), levels = ageLabels),  # keep bins in correct order
    area_ha  = ageArea
  )

  # Get current year for title
  currentYear <- time(sim)

  # Create the plot
  p1 <- ggplot2::ggplot(plotData, ggplot2::aes(x = ageClass, y = area_ha)) +
    ggplot2::geom_bar(stat = "identity",
                      fill = "darkgreen",
                      alpha = 0.7) +
    ggplot2::labs(
      x = "Age Class (years)",
      y = "Area (hectares)",
      title = paste("Forest Age Class Structure By Area - Year", currentYear),
      subtitle = paste("Study Area(s):", paste(
        unlist(sim@params$spades_ws3$basenames), collapse = ", "))
    )+
  ggplot2::scale_y_continuous(labels = scales::label_number(accuracy = 1, big.mark = ",")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 10),
      panel.grid = element_blank(),
      axis.line = element_line(color = "grey30"),
      axis.ticks = element_line(color = "grey")
    )

  # Save the plot to figures directory
  figDir <- figurePath(sim)

  filename <- paste0("AgeClassStructureByArea_Year", sprintf("%04d", currentYear))
  SpaDES.core::Plots(data = p1,
                     types = c("png"),
                     filename = file.path(figDir, filename))

  return(invisible(p1))
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

