# Plot age class structure by volume:
plotAgeClassStructureByVolume <- function(sim, period = NULL, yname = "totvol") {
  if (is.null(sim$fm)) {
    warning("Forest model (sim$fm) not found. Cannot create age class structure by volume plot.")
    return(invisible(NULL))
  }

  # Access forest model via reticulate
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("reticulate package is required for volume plotting")
  }
  
  fm <- sim$fm
  
  # Calculate period from simulation time if not provided
  # Period 0 is initial state, subsequent periods increment with planning_period_freq
  if (is.null(period)) {
    if (time(sim) == start(sim)) {
      period <- 0L  # Initial state
    } else {
      # Calculate period based on planning period frequency
      planning_freq <- ifelse(is.null(sim@params$spades_ws3$planning_period_freq), 
                              1L, 
                              as.integer(sim@params$spades_ws3$planning_period_freq))
      periods_elapsed <- as.integer((time(sim) - start(sim)) / planning_freq)
      period <- periods_elapsed
    }
  }
  
  # Get age class distribution (dict of age: area)
  age_dist <- tryCatch({
    fm$age_class_distribution(period = as.integer(period))
  }, error = function(e) {
    warning(paste("Error getting age class distribution for period", period, ":", e$message))
    return(NULL)
  })
  
  if (is.null(age_dist) || length(age_dist) == 0) {
    warning("No age classes found. Cannot create age class structure by volume plot.")
    return(invisible(NULL))
  }
  
  # Extract ages and calculate volume for each age
  ages <- as.numeric(names(age_dist))
  volumes <- numeric(length(ages))
  
  # Get volume for each age
  for (i in seq_along(ages)) {
    age <- ages[i]
    # Get volume inventory for this age
    vol <- tryCatch({
      fm$inventory(period = period, yname = yname, age = as.integer(age))
    }, error = function(e) {
      warning(paste("Error getting volume for age", age, ":", e$message))
      return(0)
    })
    volumes[i] <- ifelse(is.null(vol) || is.na(vol), 0, as.numeric(vol))
  }
  
  # Create data frame with age and volume
  volData <- data.frame(
    age = ages,
    volume = volumes
  )
  
  # Bin ages into 10-year bins
  maxAge <- max(ages, na.rm = TRUE)
  ageBreaks <- seq(0, ceiling(maxAge / 10) * 10, by = 10)
  
  # Determine padding width
  padWidth <- nchar(max(ageBreaks) - 1)
  
  # Build labels with zero-padding
  ageStarts <- ageBreaks[-length(ageBreaks)]
  ageEnds <- ageBreaks[-1] - 1
  ageLabels <- sprintf("%0*d-%0*d", padWidth, ageStarts, padWidth, ageEnds)
  
  # Create age bins (left-closed, right-open): [start, end+1)
  volData$ageBinned <- cut(
    volData$age,
    breaks = ageBreaks,
    labels = ageLabels,
    include.lowest = TRUE,
    right = FALSE
  )
  
  # Sum volumes by age class
  plotData <- aggregate(volume ~ ageBinned, data = volData, FUN = sum)
  names(plotData) <- c("ageClass", "volume")
  
  # Create factor with correct order
  plotData$ageClass <- factor(plotData$ageClass, levels = ageLabels)
  
  # Get current year for title
  currentYear <- time(sim)
  
  # Create the plot
  p1 <- ggplot2::ggplot(plotData, ggplot2::aes(x = ageClass, y = volume)) +
    ggplot2::geom_bar(stat = "identity",
                      fill = "steelblue",
                      alpha = 0.7) +
    ggplot2::labs(
      x = "Age Class (years)",
      y = paste("Volume (", yname, ")", sep = ""),
      title = paste("Forest Age Class Structure By Volume - Year", currentYear),
      subtitle = paste("Study Area(s):", paste(
        unlist(sim@params$spades_ws3$basenames), collapse = ", "))
    ) +
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
  
  filename <- paste0("AgeClassStructureByVolume_Year", sprintf("%04d", currentYear))
  SpaDES.core::Plots(data = p1,
                     types = c("png"),
                     filename = file.path(figDir, filename))
  
  return(invisible(p1))
}

