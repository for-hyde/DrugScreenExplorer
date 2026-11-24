# helper functions for DrugScreenExplorer
####-----Plotting_Functions-----####
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
      labs(x = "", y = "", fill = "estimated edge effect", title = plateName) +
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

####---Curve_Fitting_Functions---####

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

  fitIn <- fitIn[
    is.finite(fitIn$normVal) &
      is.finite(fitIn$numRowID) &
      is.finite(fitIn$numColID),
    ,
    drop = FALSE
  ]

  # A two-dimensional surface cannot be estimated from controls confined to
  # one row or column. Use finite non-positive-control wells as a fallback.
  if (nrow(fitIn) < 4 ||
      length(unique(fitIn$numRowID)) < 2 ||
      length(unique(fitIn$numColID)) < 2) {
    fallback <- plateData[plateData$wellType != "pos", ,
                          drop = FALSE]
    fallback <- fallback[
      is.finite(fallback$normVal) &
        is.finite(fallback$numRowID) &
        is.finite(fallback$numColID),
      ,
      drop = FALSE
    ]
    if (nrow(fallback) >= 4 &&
        length(unique(fallback$numRowID)) >= 2 &&
        length(unique(fallback$numColID)) >= 2) {
      fitIn <- fallback
    } else {
      stop("Insufficient finite measurements spanning both plate dimensions for edge-effect estimation.")
    }
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

####----Summarise_Screen_Functions--####

# subset for non-control wells, average for multiple values. 
subsetDataFrame <- function(dataframe){
  #Check if normVal.cor column is present
  ifCor <- "normVal.cor" %in% colnames(dataframe)
  #Subset for non-control wells
  subData <- dataframe[dataframe$wellType %in% "sample", ]
  #
  subData <- subData |>
    dplyr::group_by(sampleID, fileName, drug, concentration) |>
    dplyr::summarise(
      normVal = mean(normVal, na.rm = TRUE),
      normVal.cor = if (ifCor) mean(normVal.cor, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
  subData
}


# Calculate one DSS variant. This is used by the public calcDSS wrapper below.
calcDSS <- function(value, conc, dssType = "DSS2", threshold = 10) {
  valid_types <- c("DSS1", "DSS2", "DSS3", "all")
  checkmate::assert_subset(dssType, valid_types)
  
  if ("all" %in% dssType) {
    dss_types <- valid_types[valid_types != "all"]
  } else {
    dss_types <- unique(dssType)
  }
  
  if (any(!dss_types %in% valid_types)) {
    stop("dssType must be one of 'DSS1', 'DSS2', 'DSS3', or 'all'.")
  }
  
  # --- 1. CLEAN AND AGGREGATE (Base R for speed) ---
  # Filter out NA and concentrations <= 0
  valid_idx <- !is.na(conc) & !is.na(value) & conc > 0
  if (!any(valid_idx)) return(if(length(dss_types) == 1) NaN else stats::setNames(rep(NaN, length(dss_types)), dss_types))
  
  conc <- conc[valid_idx]
  value <- value[valid_idx]
  
  # Fast aggregation of replicates using tapply 
  agg_viab <- tapply(value, conc, mean)
  unique_conc <- as.numeric(names(agg_viab))
  
  # Sort by concentration
  sort_idx <- order(unique_conc)
  conc_sorted <- unique_conc[sort_idx]
  viab_sorted <- as.numeric(agg_viab)[sort_idx]
  
  n_points <- length(conc_sorted)
  if (n_points < 2) return(if(length(dss_types) == 1) NaN else stats::setNames(rep(NaN, length(dss_types)), dss_types))
  
  # --- 2. CALCULATE METRICS ONCE ---
  inhibition <- (1 - viab_sorted) * 100
  
  if (sum(inhibition > threshold) < 2) {
    res <- rep(NaN, length(dss_types))
    return(if(length(dss_types) == 1) NaN else stats::setNames(res, dss_types))
  }
  
  log_conc <- log10(conc_sorted)
  total_span <- log_conc[n_points] - log_conc[1]
  
  active_conc <- conc_sorted[inhibition > threshold]
  if (length(active_conc) > 1) {
    active_span <- log10(tail(active_conc, 1) / head(active_conc, 1))
  } else {
    if (any(c("DSS2", "DSS3") %dofin% dss_types)) { # note: check logic below
      # If active_span is insufficient for DSS2/DSS3, handle gracefully
      active_span <- 0
    } else {
      active_span <- 0
    }
  }
  
  # If active_span is 0 or invalid for DSS2/DSS3 requirements
  if (length(active_conc) <= 1 && any(dss_types %in% c("DSS2", "DSS3"))) {
    res <- rep(NaN, length(dss_types))
    return(if(length(dss_types) == 1) NaN else stats::setNames(res, dss_types))
  }
  
  # Trapezoidal AUC
  auc <- sum((inhibition[-n_points] + inhibition[-1]) * diff(log_conc) / 2)
  R_max <- log10(max(inhibition))
  
  # Base DSS1 calculation
  dss1 <- (auc - threshold * active_span) / ((100 - threshold) * total_span)
  
  # Derive other scores mathematically from DSS1 (no re-computation!)
  dss2 <- dss1 / R_max
  dss3 <- dss2 * (active_span / total_span)
  
  # Map results
  score_map <- c("DSS1" = dss1, "DSS2" = dss2, "DSS3" = dss3)
  #just return all, filter in other function. 
  score_map
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

####----readScreen_Functions-----#### 
#Read plate annotation file, check present columns. 
readPlateAnno <- function(filePath, deciMark){
  sampleAnno <- read_delim(
    filePath,
    delim = "\t",
    locale = locale(decimal_mark = deciMark),
    col_types = cols()
  )
  # Ensure columns are present
  if (!all(c("sampleID", "fileName") %in% colnames(sampleAnno))) {
    stop("The plate annotation file must contain the following columns: sampleID and fileName")
  }
  # Ensure all filenames are unique
  raw_filenames <- sampleAnno$fileName
  all_files <- vapply(raw_filenames, basename, "c")
  if (any(duplicated(all_files))) {
    stop("Duplicate file names found in the plate annotation file. Please ensure all file names are unique.")
  }
  return(sampleAnno)
}

readWellAnno <- function(filePath, deciMark){
  wellAnno <- read_delim(
    filePath,
    delim = "\t",
    locale = locale(decimal_mark = deciMark),
    col_types = cols()
  )
  # Ensure columns are present
  if (!all(c("wellID", "drug", "concentration") %in% colnames(wellAnno))) {
    stop("The well annotation file must contain the following columns: wellID, 
         drug, and concentration.")
  }
  return(wellAnno)
}

getWellDims <- function(wellFile){
  wells <- wellFile$wellID
  #Extract number of rows and columns from well IDs
  rowIDs <- unique(substr(wells, 1, 2))
  colIDs <- unique(as.numeric(substr(wells, 3, nchar(wells))))
  #export total size
  plate_size <- c(
    rows = length(rowIDs), 
    cols = length(colIDs))
  return(plate_size)
}

validateRanges <- function(rowRange, colRange){
  if (length(rowRange) != 2 || length(colRange) < 1){
    warning("Rows and columns must be specified. Using automatic parsing.")
    return(FALSE)
  }
  if (any(rowRange < 1) || any(colRange < 1)){
    warning("rowRange and colRange must contain positive integers.
    Using automatic parsing.")
    return(FALSE)
  }
  "valid row and column ranges provided. Using manual parsing."
  return(TRUE)
}

parseManual <- function(fileList, rowRange, colRange, 
                        sep, deciMark, plateSize){
  screenData <- lapply(fileList, function(eachFile){
    #Manual reading from specified row and column ranges
    plateData <- data.table::fread(
      eachFile,
      sep = sep,
      dec = deciMark,
      header = FALSE,
      skip = rowRange[1] - 1,
      nrows = rowRange[2] - rowRange[1] + 1,
      select = if (length(colRange) == 2) {
        seq(colRange[1], colRange[2])
      } else {
        colRange
      },
      data.table = FALSE
    )
    dplyr::mutate(
      plateData,
      fileName = basename(eachFile)
    )
  })
}

parseAutomatic <- function(fileList, decimark, wellDims) {
  screenData <- lapply(fileList, function(eachFile){
    #read in raw lines
    raw_lines <- readLines(eachFile, warn = FALSE)
    # Search for a row labelled A. This supports unquoted exports such as
    # A;value;value and quoted exports such as "34","A",value,value.
    start_idx <- grep("(^|[,;\t])\"?A\"?([,;\t]|$)", raw_lines)
    if (length(start_idx) == 0){
      stop(paste0("Could not Automatically locate the start of the plate grid\n
      Check file formatting or use manual parsing with rowRange and colRange specified."))
    }
    # Use the first candidate line to determine the delimiter and row-label
    # position. Fields before the row label are export metadata.
    target_line <- raw_lines[start_idx[1]]
    delimiters <- c(",", "\t", ";")
    delim <- delimiters[which.max(
      vapply(delimiters, function(x) {
        length(strsplit(target_line, x, fixed = TRUE)[[1]])
      }, integer(1))
    )]
    target_fields <- trimws(gsub('^"|"$', "",
                                 strsplit(target_line, delim, fixed = TRUE)[[1]]))
    row_label_idx <- match("A", target_fields)
    if (is.na(row_label_idx)) {
      stop("Could not identify the row-label column in the plate grid.")
    }

    # Extract the expected number of plate rows and retain measurement columns.
    cols <- wellDims[2]
    rows <- wellDims[1]
    end_idx <- start_idx[1] + rows - 1
    if (end_idx > length(raw_lines)) {
      stop("The detected plate grid does not contain the expected number of rows.")
    }
    isolated_lines <- raw_lines[start_idx[1]:end_idx]
    raw_matrix <- do.call(rbind, lapply(isolated_lines, function(line) {
      fields <- trimws(gsub('^"|"$', "",
                             strsplit(line, delim, fixed = TRUE)[[1]]))
      values <- fields[(row_label_idx + 1):length(fields)]
      if (length(values) < cols) {
        stop("The detected plate grid does not contain the expected number of columns.")
      }
      values[seq_len(cols)]
    }))

    plate_df <- as.data.frame(raw_matrix, stringsAsFactors = FALSE,
                              check.names = FALSE)
    #Add fileName
    plate_df <- dplyr::mutate(
      plate_df, 
      fileName = basename(eachFile))
    file.path()
    
    return(plate_df)
    
  })
  
  return(screenData)
}  

#When manual loading is performed, check if data includes the header and column. 
#Automatically generate row and column IDs in any case
checkManualHeaders <- function(screenData){
  firstPlate <- screenData[[1]]
  if (grepl(firstPlate[2, 1], "A")) {
    no_index <- FALSE
  } else {
    no_index <- TRUE
  }
  #Compare if integer value 
  if (as.integer(firstPlate[1, 2]) != as.integer(1)) {
    no_header <- TRUE
  } else {
    no_header <- FALSE
  }
  screenData_mod <-addHeaderAndIndex(screenData, no_index = no_index, no_header = no_header)
  
  return(screenData_mod)
}

addHeaderAndIndex <- function(screenData, no_index, no_header){
  if (!no_index) {
    #remove first column (row index)
    screenData <- lapply(screenData, function(plate) {
      plate <- plate[, -1]
      return(plate)
    })
  }
  if (!no_header) {
    #remove first row (column header)
    screenData <- lapply(screenData, function(plate) {
      plate <- plate[-1, ]
      return(plate)
    })
  }
  screenData <- lapply(screenData, function(plate) {
    # Generate row and column IDs
    num_rows <- nrow(plate)
    num_cols <- ncol(plate) - 1 #fileName column
    row_ids <- genRowIDs(num_rows)
    col_ids <- genColIDs(num_cols)

    # Add row and column IDs to the plate data
    plate <- cbind(rowID = row_ids, plate)
    names(plate) <- c("rowID", col_ids, "fileName")

    plate
  })

  screenData
}

convertPlateToLong <- function(screenData){
  #Convert to long dfs
  long_plates <- lapply(screenData, function(plate) {
    col_nums <- setdiff(names(plate), c("rowID","fileName"))
    tidyr::pivot_longer(
                        plate,
                        cols = dplyr::all_of(col_nums),
                        names_to = "colID",
                        values_to = "value")
  })
  #Combine dfs into one, add wellID column
  long_plates |>
    dplyr::bind_rows() |>
    dplyr::mutate(wellID = paste0(rowID, colID))
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

# Join Well annotation data and label controls
joinWellAnno <- function(screenData, wellData, negCtrl, posCtrl){
  #join on wellID
  screenData <- dplyr::left_join(
    screenData, wellData, by = "wellID"
  )
  screenData <- screenData %>%
    dplyr::mutate(
      wellType = if ("drug2" %in% names(.)){
        dplyr::case_when(
          drug %in% posCtrl & drug2 %in% posCtrl ~ "pos",
          drug %in% negCtrl & drug2 %in% negCtrl ~ "neg",
          TRUE ~ "sample")
      } else {
        dplyr::case_when(
          drug %in% posCtrl ~ "pos",
          drug %in% negCtrl ~ "neg",
          TRUE ~ "sample"
        )
      }
    )
   return(screenData)
}

####----Normalization_Functions-----####

findDiscardLayers <- function(dataframe, nLayers) {
  #Size of the plate
  nCol <- length(unique(dataframe$colID))
  nRow <- length(unique(dataframe$rowID))

  #Generate the edges based on the number of layers specified
  edgeCol <- genColIDs(nCol)[c(
    seq(1, nLayers), 
    seq(nCol - nLayers + 1, nCol))]
  edgeRow <- genRowIDs(nRow)[c(
    seq(1, nLayers),
    seq(nRow - nLayers + 1, nRow))]

  list(
    edgeCol = edgeCol,
    edgeRow = edgeRow
  )
}

# Function to check if controls still exist after discarding 
# edge layers. If not, will raise an error and return FALSE.
getControlMedians <- function(dataframe, disCol, disRow, method){
  #filter the wells
  filteredData <- dplyr::filter(
    dataframe,
    !colID %in% disCol & !rowID %in% disRow
  )
  #Find negative values
  negValues <- filteredData[filteredData$wellType == "neg", ]$value
  posValues <- filteredData[filteredData$wellType == "pos", ]$value  

  if (length(negValues) == 0) {
    stop("No negative control wells remain after discarding edge layers.")
  } else {
    medianNeg <- median(negValues, na.rm = TRUE)
  }

  if (method == "NPI") {
    if (length(posValues) == 0) {
      stop("No positive control wells remain after discarding edge layers.")
    } else {
      medianPos <- median(posValues, na.rm = TRUE)
    }
  } else {
    medianPos <- NA
  }

  list(
    medNeg = medianNeg,
    medPos = medianPos
  )
  
}
