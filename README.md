# DrugScreenExplorer

DrugScreenExplorer is an R/Bioconductor package for processing, quality
control, and visualization of high-throughput drug sensitivity screen data.
It supports single-drug screens and two-drug combinations through both an R
function interface and an interactive Shiny application.

## Features

- Import and assemble raw screening data and plate/well annotations
- Normalize plate measurements
- Detect and correct edge and plate effects
- Summarize dose-response relationships with measures such as IC50 and AUC
- Visualize dose-response curves, plate data, drug similarities, and sample
  associations
- Analyze synergy and antagonism in two-drug combinations
- Explore complete workflows interactively in a Shiny application
- Generate analysis reports

## Installation

DrugScreenExplorer requires R >= 4.4.0. Install the released package through
Bioconductor:

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

BiocManager::install("DrugScreenExplorer")
```
alternatively, install the development version from GitHub:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("for-hyde/DrugScreenExplorer")
```

## Quick start

Load the package:

```r
library(DrugScreenExplorer)
```

Launch the interactive application directly:

```r
app <- buildApp()
shiny::runApp(app)
```

Alternatively, create a self-contained application directory containing the
preprocessed data:

```r
rawFolder <- system.file("testData/rawData",
                         package = "DrugScreenExplorer")
plateFile <- system.file("testData/plateAnno/plateAnno.tsv",
                         package = "DrugScreenExplorer")
wellFile <- system.file("testData/wellAnno/wellAnno.tsv",
                        package = "DrugScreenExplorer")

makeShiny(
  rawDir = rawFolder,
  plateAnnotationFile = plateFile,
  wellAnnotationFile = wellFile,
  dir = "."
)
```

The generated `shiny` directory contains `shinyAPP.R` and `ShinyData.zip`.

## Documentation

- Package documentation is available through `help(package = "DrugScreenExplorer")`
  and the individual function help pages.
- The package vignettes describe the R workflow and the Shiny application:

```r
browseVignettes("DrugScreenExplorer")
```

## Reporting issues

Please report bugs or feature requests in the
[GitHub issue tracker](https://github.com/for-hyde/DrugScreenExplorer/issues).

## Authors and contributors

- Forrest Hyde
- Shuo Wang
- Junyan Lu

## License

DrugScreenExplorer is distributed under the Artistic License 2.0.

## Contact

- GitHub: https://github.com/for-hyde/
- Email: `forrest.hyde@stud.uni-heidelberg.de`





