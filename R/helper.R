# Some helper functions
## Function to read screen data from one plate


## Function to make pdf for a list of ggplot objects
makepdf <- function(x, name, ncol = 3, nrow = 2, width = 20, height = 12) {

  if (length(x) == 0)
    return(NULL)

  pdf(name, width = width, height = height)
  for (i in seq(1, length(x), by = ncol * nrow)) {
    # print(names(x)[i])
    j <- min(i + ncol * nrow - 1, length(x))
    do.call(gridExtra::grid.arrange, c(x[seq(i, j)], ncol = ncol, nrow = nrow))
  }

  dev.off() %>%
    invisible

}


## Function to generate an ordered row IDs based on the number of rows
genRowIDs <- function(x) {
  if (x > 702)
    stop("Rows exceeds maximal supported number (n=702)")

  allID <- c(paste0(LETTERS, 0),
             paste0(rep(LETTERS, each = 26), rep(LETTERS, times = 26)))

  return(allID[seq(1, x)])
}

## Function to generate an ordered column IDs based on the number of columns
genColIDs <- function(x) {
  formatC(seq(x), width = 2, format = "d", flag = "0")
}

## Function for fitting edge effect using loess local regression method
fitOneLoess <- function(plateData,
                        useNeg,
                        useLowConcentrations,
                        lowConcTab,
                        span) {

  # select 'de facto' negative controll wells for surface fitting
  if (!useNeg) {
    #do not use positive controls
    fitIn <- plateData[!plateData$wellType %in% "pos", ]
  } else {

    if (!"wellType" %in% colnames(plateData))
      stop("No well type information found")

    if (useLowConcentrations > 0) {

      # lowest concentrations are also used for fitting
      lowConc <- paste0(lowConcTab$drug, "_", lowConcTab$concentration)
      lowConcWell <-
        plateData[paste0(plateData$drug, "_", plateData$concentration)
                  %in% lowConc, ]$wellID

    } else lowConcWell <- c()

    fitIn <- plateData[plateData$wellType %in% "neg" |
                         plateData$wellID %in% lowConcWell, ]
  }

  # fit the surface using loess model
  lo <- loess(normVal ~ numRowID + numColID, data = fitIn,
              control = loess.control(surface = "direct"), span = span)
  plateData$edgeFactor <- predict(lo, newdata = plateData)
  plateData
}

# Get initial values based on the values on the plate

getIniParm <- function(fitIn) {

  # estimate initial values based on the values on the plate
  dr1 <- median(fitIn[fitIn$numRowID == min(fitIn$numRowID), ]$normVal)
  cr2 <- 1 - median(fitIn[fitIn$numRowID == max(fitIn$numRowID), ]$normVal)
  dc1 <- median(fitIn[fitIn$numColID == min(fitIn$numColID), ]$normVal)
  cc2 <- 1 - median(fitIn[fitIn$numColID == max(fitIn$numColID), ]$normVal)

  er1 <- max(fitIn$numRowID) * 0.1
  er2 <- max(fitIn$numRowID) + 1 - er1
  ec1 <- max(fitIn$numColID) * 0.1
  ec2 <- max(fitIn$numColID) + 1 - ec1
  sr1 <- 0.5
  sr2 <- 0.5
  sc1 <- 0.5
  sc2 <- 0.5

  paraIni <- c(dr1 = dr1, cr2 = cr2, dc1 = dc1,
               cc2 = cc2, er1 = er1, er2 = er2,
               ec1 = ec1, ec2 = ec2, sr1 = sr1, sr2 = sr2,
               sc1 = sc1, sc2 = sc2)

  return(paraIni)
}

#function for 2D sigmoid curv-fitting
esFun <- function(data, par) {
  with(data,
       sum(((normVal - (1 + (par[1] - 1)/(1 + exp((numRowID - par[5])/par[9])) -
                          par[2] + par[2]/(1 + exp((numRowID - par[6])/par[10]))) *
               (1 + (par[3] - 1)/(1 + exp((numColID - par[7])/par[11])) -
                  par[4] + par[4]/(1 + exp((numColID - par[8])/par[12])))))^2))
}

#function for predict values based on 2D sigmoid model
predictFun <- function(numRowID, numColID, par) {
  (1 + (par[1] - 1)/(1 + exp((numRowID - par[5])/par[9])) - par[2] +
     par[2]/(1 + exp((numRowID - par[6])/par[10]))) *
    (1 + (par[3] - 1)/(1 + exp((numColID - par[7])/par[11])) - par[4] +
       par[4]/(1 + exp((numColID - par[8])/par[12])))
}

## Function for fitting edge effect using 2D sigmoid model (experimental feature)
fitOneSigmoid <- function(plateData, useNeg, useLowConcentrations, lowConcTab) {

  if (!"wellType" %in% colnames(plateData))
    stop("No well type information found")

  # select 'de facto' negative control wells for surface fitting
  if (!useNeg) {
    #do not use positive controls
    fitIn <- plateData[!plateData$wellType %in% "pos", ]
  } else {
    if (useLowConcentrations > 0) {
      # lowest concentrations are also used for fitting
      lowConc <- paste0(lowConcTab$drug, "_", lowConcTab$concentration)
      lowConcWell <-
        plateData[paste0(plateData$drug, "_",
                         plateData$concentration) %in% lowConc, ]$wellID

    } else lowConcWell <- c()

    fitIn <- plateData[plateData$wellType %in% "neg" |
                         plateData$wellID %in% lowConcWell, ]
  }

  paraIni <- getIniParm(fitIn)

  edgeFactors <- tryCatch({
    sig2D <- optim(par = paraIni, esFun, data = fitIn)
    predictFun(plateData$numRowID, plateData$numColID, sig2D$par)
  }, error = function(err) {
    warning("Edge effect estimate using 2D-sigmoid model failed.
            Set edgeEffect value to 1")
    rep(1, nrow(plateData))
  })

  plateData$edgeFactor <- edgeFactors
  plateData
}


# Function to calculate AUC using linear-log (on X) trapezoidal method
# Use summariseScreen() to calculate AUC.
calcAUC <- function(value, conc, viabCut = NULL) {
  
  valueConc <- data.frame(viab = value, conc = conc) %>%
    dplyr::filter(!is.na(conc) & conc > 0) %>%
    dplyr::group_by(conc) %>%
    dplyr::summarise(viab = mean(viab)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(conc)

  # censor the values based on viability cut-off
  if (!is.null(viabCut)) {
    valueConc <- dplyr::mutate(valueConc, viab = viab + 1 - viabCut) %>%
      dplyr::mutate(viab = ifelse(viab > 1, 1, viab))
  }

  if (nrow(valueConc) %in% c(0, 1)) {
    return(mean(valueConc$viab, na.rm = TRUE))
  } else {
    areaTotal <- 0
    for (i in seq(nrow(valueConc) - 1)) {
      areaEach  <- (valueConc$viab[i] + valueConc$viab[i + 1]) *
        log(valueConc$conc[i + 1]/valueConc$conc[i]) * 0.5
      areaTotal <- areaTotal + areaEach
    }
    aucRatio <- areaTotal / log(valueConc[nrow(valueConc), ]$conc / valueConc[1, ]$conc)
    return(aucRatio)
  }
}

# Function to correct edge effect using a specialized linear model, to fix
# issue of over correcting of bliss model. This is only an experimental function

corEdgeLinear <- function(x, m) {

  inTab <- data.frame(normVal = x, edgeFactor = m)

  s <- apply(inTab, 1, function(eachRow) {
    if (eachRow[1] < 0) {
      eachRow[1]
    } else if (eachRow[1] >= 0 & eachRow[1] <= eachRow[2]) {
      eachRow[1]/eachRow[2]
    } else {
      eachRow[1] + 1 - eachRow[2]
    }
  })
  s
}


# estimate the edge effect on one plate

estimateOnePlate <- function(plateData, nLayer, onlyNeg) {

  if (nLayer < 1) {
    stop("At least only layer needs to be specified as edge")
  } else {
    nCol <- length(unique(plateData$colID))
    nRow <- length(unique(plateData$rowID))
    edgeCol <- genColIDs(nCol)[c(seq(1, nLayer), seq(nCol - nLayer + 1, nCol))]
    edgeRow <- genRowIDs(nRow)[c(seq(1, nLayer), seq(nRow - nLayer + 1, nRow))]
  }

  edgeWell <-
    dplyr::filter(plateData, colID %in% edgeCol, rowID %in% edgeRow) %>%
    dplyr::select(wellID, value, wellType)
  innerWell <- dplyr::filter(plateData, !wellID %in% edgeWell$wellID)

  if (onlyNeg) {
    # only use negative control wells, check whether they are present in edge
    # and innel wells
    if (!"neg" %in% edgeWell$wellType)
      stop("No negative control wells found on the edge.")
    if (!"neg" %in% innerWell$wellType)
      stop("No negative control wells found on the inner plate")
    edgeFac <-
      median(edgeWell$value, na.rm=TRUE) / median(innerWell$value, na.rm = TRUE)
  } else {
    edgeFac <-
      median(edgeWell$value, na.rm=TRUE) / median(innerWell$value, na.rm = TRUE)
  }

  edgeFac
}

# Determin the type of input vector
detectClass <- function(x) {
  # change potential NA records to NA
  x[x %in% c("", "NA")] <- NA
  x <- na.omit(x)
  nx <- length(unique(x))

  if (nx <= 1) {
    return("nd")
  } else if (nx == 2) {
    return("binary")
  } else {
    x.num <- as.numeric(as.character(x))
    if (all(!is.na(x.num))) {
      return("continuous")
    } else {
      if (nx < length(x)) {
        return("categorical")
      } else return("nd")
    }
  }
}


#- plot viability heatmap for each plate
plotViability <- function(matPlate, plateList, limits, ifCorrected) {

  # plot normalized ATP count (viability))
  if (!"normVal" %in% colnames(matPlate))
    stop("No relative viability values found, please normalize the plate first")

  if (is.null(limits)) {
    limits <- c(0, 2)
  }

  #- matPlate should contains normVal
  if (!"normVal" %in% colnames(matPlate))
    stop("Plate should be normalized")

  matPlate <-
    dplyr::mutate(matPlate,
                  normVal = ifelse(
                    normVal < limits[1], limits[1],
                    ifelse(normVal > limits[2], limits[2], normVal)))

  if (ifCorrected) {
    if (!"normVal.cor" %in% colnames(matPlate)) {
      stop("No edge-corrected viability found,
           please run edge effect correction first")
    } else {
      matPlate <- dplyr::mutate(matPlate, normVal = normVal.cor)
    }
  }

  plotList <- lapply(plateList, function(plateName) {

    p <- dplyr::filter(matPlate, fileName == plateName) %>%
      ggplot(aes(x = colID, y = rowID, fill = normVal)) +
      geom_tile(color = "grey80") +
      labs(x = "", y = "", fill = "viability", title = plateName) +
      theme_void() +
      theme(axis.text  = element_text(size = 6),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.margin = margin(5, 5, 5, 5))

    if (!is.null(limits)) {
      p <- p + scale_fill_gradient2(limits = limits, high = "red", low = "blue",
                                    mid = "white", midpoint = 1)
    } else {
      p <- p + scale_fill_gradient2(high = "red", low = "blue", mid = "white",
                                    midpoint = 1)
    }
  })
  return(plotList)
}

#plot zscore heatmap for each plate
plotZscore <- function(matPlate, plateList, limits) {

  # calculate per-plate z-score
  calcZ <- function(x)
    dplyr::mutate(x, zscore = (value - mean(value))/sd(value))

  matPlate <- group_by(matPlate, fileName) %>%
    do(calcZ(.)) %>%
    ungroup()

  if (is.null(limits)) {
    limits <- c(-3, 3)
  }

  # censoring value based on limit
  matPlate <-
    dplyr::mutate(matPlate,
                  zscore = ifelse(zscore < limits[1], limits[1],
                                  ifelse(zscore > limits[2], limits[2], zscore)))

  plotList <- lapply(plateList, function(plateName) {
    p <- dplyr::filter(matPlate, fileName == plateName) %>%
      ggplot(aes(x = colID, y = rowID, fill = zscore)) +
      geom_tile(color = "grey80") +
      labs(x = "", y = "", fill = "Z score", title = plateName) +
      theme_void() +
      theme(axis.text = element_text(size = 6),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.margin = margin(5, 5, 5, 5))

    if (!is.null(limits)) {
      p <- p + scale_fill_gradient2(limits = limits, high = "red", low = "blue",
                                    mid = "white", midpoint = 0)
    } else {
      p <- p + scale_fill_gradient2(high = "red", low = "blue", mid = "white",
                                    midpoint = 0)
    }
  })

  return(plotList)
}


#plot edge effect for each plate
plotEdgeEffect <- function(matPlate, plateList, limits) {

  # only plot estiamted edge effect
  if (!"edgeFactor" %in% colnames(matPlate))
    stop("No edge effect information found: please fit edge effect first")

  if (is.null(limits)) {
    limits <- c(0, 2)
  }

  matPlate <-
    dplyr::mutate(matPlate,
                  normVal =
                    ifelse(normVal < limits[1], limits[1],
                           ifelse(normVal > limits[2], limits[2], normVal)))

  plotList <- lapply(plateList, function(plateName) {

    p <- dplyr::filter(matPlate, fileName == plateName) %>%
      ggplot(aes(x = colID, y = rowID, fill = edgeFactor)) +
      geom_tile(color = "grey80") +
      labs(x = "", y = "", fill = "viability", title = plateName) +
      theme_void() +
      theme(axis.text  = element_text(size = 6),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5),
            plot.margin = margin(5, 5, 5, 5))

    if (!is.null(limits)) {
      p <- p + scale_fill_gradient2(limits = limits, high = "red", low = "blue",
                                    mid = "white", midpoint = 1)
    } else {
      p <- p + scale_fill_gradient2(high = "red", low = "blue", mid = "white",
                                    midpoint = 1)
    }
  })

  return(plotList)
}

#Experimental function for automated parsing of raw datafiles. Takes file path,
# then extracts the well plate information based on regex parsing.
extract_plate_block <- function(file_path, allowNA = FALSE){
    #Read in lines
    raw_lines <- readLines(file_path, warn = FALSE)

    # This regex catches 'A,' or 'A\t' or 'A;' or ',A,'
    start_idx <- grep("^(,)?A(,|\\t|;)", raw_lines)

    # Check if a start has been found
    if (length(start_idx) == 0){
      stop("Could not Automatically locate the start of the plate grid (Row A)")
    }

    target_line <- raw_lines[start_idx[1]]
    delim <- if (grepl("\\t", target_line)) "\t" else if (grepl(";", target_line)) ";" else ","

    #Set header as row directly above A
    header_idx <- start_idx[1] - 1

    #Dynamically determine plate size by counting elements in the header row
    header_elements <- strsplit(raw_lines[header_idx], delim)[[1]]
    # Remove empty elements to get an accurate count of column headers
    num_cols <- length(header_elements[header_elements != ""])

    #set number of rows based on known row-ratios from column numbers.
    rows_lookup <- c(
      `3`  = 2,
      `4`  = 3,
      `6`  = 4,
      `8`  = 6,
      `12` = 8,
      `24` = 16,
      `48` = 32
    )
    row_count <- rows_lookup[as.character(num_cols)]
    end_idx <- header_idx + row_count

    #Extract the text block
    isolated_lines <- raw_lines[header_idx:end_idx]

    #Read text block as matrix
    raw_matrix <- do.call(rbind, strsplit(isolated_lines, delim))

    raw_matrix[1,1] <- "RowLetter"
    # Convert to data frame
    plate_df <- as.data.frame(raw_matrix, stringsAsFactors = FALSE)

    screenData <- convert_plate_to_long(plate_df, allowNA = allowNA)

    return(screenData)
}

# Function to convert plate layout to long format with wellID, rowID, colID, and value columns.
convert_plate_to_long <- function(plate_df, allowNA = FALSE) {
    #Extract column numbers
    idx <- as.character(unlist(plate_df[1, -1]))
    #remove from df
    plate_df <- plate_df[-1, ]
    #Set column names
    names(plate_df) <- c("rowID", idx)

    #Convert to long format
    plate_df <- tidyr::pivot_longer(
      plate_df,
      cols = -rowID,
      names_to = "colID",
      values_to = "values"
    )

    screenData <- plate_df %>%
      mutate(
        wellID = paste0(rowID, str_pad(colID, width = 3, pad = "0")),
        rowID = paste0(rowID, "0"),
        value = as.numeric(values)
      ) %>%
      select(wellID, rowID, colID, value)

    if (anyNA(screenData$value)) {
      if (allowNA) {
        warning("NA values detected in plate")
      } else {
        stop("NA values detected in plate. If NA values should be allowed, please set the 'allowNA = TRUE'")
      }
    }

    return(screenData)

    # temp_file <- tempfile(fileext = ".csv")
    # on.exit(unlink(temp_file), add = TRUE)

    # readr::write_csv(plate_df, file = temp_file)
    # plate_df <- plater::read_plate(temp_file)

    # screenData <- plate_df %>%
    #   extract(Wells, into = c("row_letter", "colID"), regex = "([A-Z]+)(\\d+)", remove = FALSE) %>%
    #   mutate(
    #     rowID = paste0(row_letter, "0"),
    #     wellID = paste0(row_letter, str_pad(colID, width =3, pad = "0")),
    #     value = as.numeric(values)
    #   ) %>%
    #   select(wellID, rowID, colID, value)

    # return(screenData)
}


# Calculate one DSS variant. This is used by the public calcDSS wrapper below.
calcDSS_single <- function(value, conc, dssType = "DSS2", threshold = 10) {
  
  # 1. Clean, aggregate replicates, and sort 
  valueConc <- data.frame(viab = value, conc = conc) %>%
    dplyr::filter(!is.na(conc) & conc > 0) %>%
    dplyr::group_by(conc) %>%
    dplyr::summarise(viab = mean(viab)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(conc)

  # 2. Convert viability (0 to 1 scale) to inhibition percentage (0 to 100 scale)
  # Inhibition = (1 - viability) * 100
  valueConc <- dplyr::mutate(valueConc, inhibition = (1 - viab) * 100)

  # Determine number of points above inhibition threshold
  num_points_above_threshold <- sum(valueConc$inhibition > threshold)
  if (num_points_above_threshold < 2) {
    return(NaN) # Not enough data points above threshold to calculate DSS
  }

  # Handle edge cases where there are insufficient points
  if (nrow(valueConc) %in% c(0, 1)) {
    return(NaN) # Not enough data points to calculate DSS
  } else {
    
    # Total log10 concentration span (max(x) - min(x))
    total_span <- log10(valueConc$conc[nrow(valueConc)] / valueConc$conc[1])
    
    # Effective log10 concentration span (max(x) - min(x)) for points above threshold
    active_conc <- valueConc$conc[valueConc$inhibition > threshold]

    # Check there are enough points for DSS2 and DSS3 calculations
    if (length(active_conc) > 1) {
      active_span <- log10(tail(active_conc, 1) / head(active_conc, 1))
    } else {
      #If length is 1 or 0, active_span is 0. For DSS2 and DSS3, this will result in NaN being returned.
      if (dssType %in% c("DSS2", "DSS3")) {
        return(NaN) # Not enough active points to calculate DSS2 or DSS3
      } else {
        active_span <- 0 # Not required for DSS1 calculation
      }
    }

    # Calculate the inhibition AUC using a vectorized trapezoidal rule on the
    # log10 concentration scale. This remains separate from calcAUC(), which
    # integrates viability rather than inhibition.
    log_conc <- log10(valueConc$conc)
    auc <- sum(
      (valueConc$inhibition[-nrow(valueConc)] +
         valueConc$inhibition[-1]) *
        diff(log_conc) / 2
    )

    # Calculate R_max 
    R_max <- log10(max(valueConc$inhibition))

    #DSS1 calculation
    dss_score <- (auc - threshold * active_span) / ((100 - threshold) * total_span)
    if (dssType == "DSS1") {
      return(dss_score)
    }
    #DSS2 calculation from DSS1
    dss_score <- dss_score / R_max
    if (dssType == "DSS2") {
      return(dss_score)
    }

    dss_score <- dss_score * (active_span / total_span)
    if (dssType == "DSS3") {
      return(dss_score)
    }

    return(NaN) # If dssType is not recognized, return NaN
  }
}

# Calculate one or more DSS variants. A single variant retains the historical
# scalar return value; multiple variants are returned as a named numeric vector.
calcDSS <- function(value, conc, dssType = "DSS2", threshold = 10) {
  valid_types <- c("DSS1", "DSS2", "DSS3")

  if (!is.character(dssType) || length(dssType) == 0 || anyNA(dssType)) {
    stop("dssType must be one or more of 'DSS1', 'DSS2', 'DSS3', or 'all'.")
  }

  if ("all" %in% dssType) {
    dss_types <- valid_types
  } else {
    dss_types <- unique(dssType)
  }

  if (any(!dss_types %in% valid_types)) {
    stop("dssType must be one or more of 'DSS1', 'DSS2', 'DSS3', or 'all'.")
  }

  scores <- vapply(
    dss_types,
    function(type) calcDSS_single(value, conc, dssType = type,
                                   threshold = threshold),
    numeric(1)
  )

  if (length(scores) == 1) {
    unname(scores)
  } else {
    scores
  }
}