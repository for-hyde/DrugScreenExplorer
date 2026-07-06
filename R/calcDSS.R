calcDSS <- function(value, conc, type = "DSS2", t = 10, scale_100 = FALSE) {
  # 1. Clean, aggregate, and sort the concentration data
  valueConc <- data.frame(viab = value, conc = conc) %>%
    dplyr::filter(!is.na(conc) & conc > 0 & !is.na(viab)) %>%
    dplyr::group_by(conc) %>%
    dplyr::summarise(viab = mean(viab)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(conc)

  if (nrow(valueConc) < 2) return(0)

  # 2. Convert Viability to Inhibition (Assuming viability is out of 100)
  # If your input viability is scaled 0 to 1, change 100 to 1, and t=10 to t=0.10
  if (!scale_100) {
    valueConc$inhib <- 1 - valueConc$viab
    t_scaled <- t / 100
    max_val <- 1
  } else {
    valueConc$inhib <- 100 - valueConc$viab
    t_scaled <- t
    max_val <- 100
  }

  # 3. Apply the DSS noise threshold (t)
  # If inhibition is less than the noise threshold, treat it as 0
  valueConc$inhib_cleansed <- valueConc$inhib - t_scaled
  valueConc$inhib_cleansed[valueConc$inhib_cleansed < 0] <- 0

  # 4. Integrate using Logarithmic Trapezoidal Rule (matching your AUC style)
  areaTotal <- 0
  maxPossibleArea <- 0

  for (i in seq(nrow(valueConc) - 1)) {
    # Width of the concentration step in log space
    log_width <- log(valueConc$conc[i + 1] / valueConc$conc[i])

    # Empirical area of inhibition for this step
    areaEach  <- (valueConc$inhib_cleansed[i] + valueConc$inhib_cleansed[i + 1]) * log_width * 0.5
    areaTotal <- areaTotal + areaEach

    # Max possible area if the drug had achieved 100% active inhibition (max_val - t) across the step
    maxEach   <- ((max_val - t_scaled) + (max_val - t_scaled)) * log_width * 0.5
    maxPossibleArea <- maxPossibleArea + maxEach
  }

  if (maxPossibleArea <= 0) return(0)

  # 5. Compute the specific DSS metrics empirically
  dss1 <- (areaTotal / maxPossibleArea) * 100

  # Calculate observed maximum inhibition achieved in the screen for DSS3 scaling
  obs_max_inhib <- max(valueConc$inhib, na.rm = TRUE)

  if (type == "DSS1") {
    return(dss1)
  } else if (type == "DSS2") {
    # Standard normalization based on log-range scale parameter
    dss2 <- dss1 / log10(100)
    return(max(0, dss2))
  } else if (type == "DSS3") {
    # Scale based on how close the maximum observed effect was to 100%
    scaling_factor <- (obs_max_inhib - t_scaled) / (max_val - t_scaled)
    scaling_factor <- max(0, min(1, scaling_factor)) # bound between 0 and 1

    dss3 <- (dss1 / log10(100)) * scaling_factor * 2
    return(max(0, dss3))
  } else {
    stop("Invalid DSS type. Choose 'DSS1', 'DSS2', or 'DSS3'")
  }
}
