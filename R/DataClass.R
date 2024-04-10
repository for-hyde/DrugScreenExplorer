# add version information
#-------------------------------------------------------------------------------
# experimentData
#-------------------------------------------------------------------------------

experimentData <-
  setClass("experimentData",
           representation(
             name           = "character",
             lab            = "character",
             contact        = "character",
             title          = "character",
             abstract       = "character",
             url            = "character",
             pubMedIds      = "character",
             samples        = "list",
             hybridizations = "list",
             normControls   = "list",
             preprocessing  = "list",
             other          = "list"
           ),

           # contains = c("MIAxE"),

           prototype = prototype(
             # .Versioned(versions=c(classVersion("MIAxE"), MIAME="1.1.0")),
             name           = "",
             lab            = "",
             contact        = "",
             title          = "",
             abstract       = "",
             url            = "",
             pubMedIds      = "",
             samples        = list(),
             hybridizations = list(),
             normControls   = list(),
             preprocessing  = list(),
             other          = list()
           )
  )

#-------------------------------------------------------------------------------
# plateAnnotation
#-------------------------------------------------------------------------------

plateAnnotation <-
  setClass("plateAnnotation",
           representation(
             fileName       = "character",
             batch          = "character",
             sampleID       = "character"
           ),

           # contains=c("MIAxE"),
           prototype = prototype(
             # Versioned(versions=c(classVersion("MIAxE"), MIAME="1.1.0"))
             fileName       = "",
             batch          = "",
             sampleID       = ""
           )
  )


#-------------------------------------------------------------------------------
# wellAnnotation
#-------------------------------------------------------------------------------

wellAnnotation <-
  setClass("wellAnnotation",
           representation(
             wellID         = "character",
             rowID          = "character",
             colID          = "character"
           ),

           # contains=c("MIAxE"),

           prototype = prototype(
             # Versioned(versions=c(classVersion("MIAxE"), MIAME="1.1.0"))
             wellID          = "",
             rowID           = "",
             colID           = ""
           )
  )

#-------------------------------------------------------------------------------
# screenData
#-------------------------------------------------------------------------------

# screenData can be an array

screenData <-
  setClass("screenData",
           representation(
             fileName = "character",
             sampleID = "character",
             wellID = "character",
             rowID  = "character",
             colID  = "character",
             value  = "numeric"),

           prototype = prototype(
             fileName = "",
             wellID = "",
             rowID  = "",
             colID  = "",
             value  = NULL)
  )



#-------------------------------------------------------------------------------
# screenSet
#-------------------------------------------------------------------------------

#
# screenSet <-
#   setClass("screenSet",
#            representation(screenData = "screenData",
#                           plateAnnotation = "plateAnnotation",
#                           wellAnnotation  = "wellAnnotation",
#                           experimentData  = "experimentData",
#                           "VIRTUAL"),
#
#            # contains = "",
#
#            prototype = prototype(
#              # .VersionedBiobase(versions=c(eSet="1.3.0")),
#              screenData = "matrix",
#              phenoData = .AnnotatedDataFrame(
#                dimLabels =c("sampleNames", "sampleColumns")),
#              featureData = .AnnotatedDataFrame(
#                dimLabels =c("featureNames", "featureColumns")),
#              annotation  = character(),
#              protocolData = .AnnotatedDataFrame(
#                dimLabels=c("sampleNames", "sampleColumns")))
#   )
#


#-------------------------------------------------------------------------------


