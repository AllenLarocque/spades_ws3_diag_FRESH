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
  
  # Use memory-efficient approach: use freq() instead of loading all values
  # This avoids loading potentially millions of values into memory
  freq_table <- tryCatch({
    terra::freq(ageRaster, value = TRUE)
  }, error = function(e) {
    warning(paste("Error reading age frequencies from raster:", e$message))
    return(NULL)
  })
  
  if (is.null(freq_table) || nrow(freq_table) == 0) {
    warning("No valid age values found in landscape. Cannot create age class structure plot.")
    return(invisible(NULL))
  }
  
  # Remove NA values
  freq_table <- freq_table[!is.na(freq_table$value), ]
  
  if (nrow(freq_table) == 0) {
    warning("No valid age values found in landscape. Cannot create age class structure plot.")
    return(invisible(NULL))
  }
  
  # Extract unique age values and their counts
  ageValues <- freq_table$value

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

  # Count pixels in each bin using frequency table (more memory efficient)
  # Instead of using table(), we'll aggregate the counts from freq_table
  freq_table$ageBinned <- ageBinned
  ageCounts <- aggregate(count ~ ageBinned, data = freq_table, FUN = sum)
  
  # Convert to named vector for compatibility with existing code
  ageCountsVec <- ageCounts$count
  names(ageCountsVec) <- ageCounts$ageBinned
  
  # Convert to area (hectares)
  ageArea <- as.numeric(ageCountsVec) * resInHA

  # Create data frame for plotting (preserve factor order)
  plotData <- data.frame(
    ageClass = factor(names(ageCountsVec), levels = ageLabels),  # keep bins in correct order
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

