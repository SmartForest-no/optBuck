# optBuck

`optBuck` is an R package for processing production data from single-grip forest harvesters and evaluating bucking outcomes. The package reads StanForD 2010 (`.hpr`) files and provides tools for extracting production data, handling assortments and price matrices, and computing optimal bucking solutions using dynamic programming.

The package was developed for research applications related to harvester data analysis, bucking optimization, productivity assessment, and economic simulation.

## Background

Single-grip harvesters generate large amounts of standardized production data during operation. These data contain detailed information on harvested stems, produced logs, dimensions, volumes, assortments, and harvesting operations. Such data are increasingly used in forest inventory, harvesting research, operational planning, and economic analyses.

Bucking has major influence on timber value and value recovery. A stem can often be processed into multiple alternative log combinations with different dimensions, assortments, and values. `optBuck` provides tools for evaluating such bucking outcomes and computing value-maximizing alternatives.

## Features

The package includes functionality for:

- reading and parsing StanForD 2010 (`.hpr`) files
- extracting stem- and log-level production data
- extracting assortment definitions and price matrices
- evaluating observed bucking outcomes
- computing optimal bucking solutions
- visualizing and comparing bucking outcomes

## Installation

```r
devtools::install_github("SmartForest-no/optBuck")
```

## Example

```r
library(optBuck)

# Example data
hprfile <- system.file(
  "extdata",
  "example.hpr",
  package = "optBuck"
)

# Read HPR file
XMLNode <- getXMLNode(hprfile)

# Extract information
PriceMatrices <- getPriceMatrices(XMLNode)
LengthClasses <- getLengthClasses(XMLNode)
SpeciesGroupDefinition <- getSpeciesGroupDefinition(XMLNode)
ProductData <- getProductData(
  XMLNode,
  SpeciesGroupDefinition
)

PermittedGrades <- getPermittedGrades(XMLNode)
Stems <- getStems(XMLNode)
Logs <- getLogs(XMLNode)

StemProfile <- getStemprofile(
  XMLNode,
  Logs
)

# Observed bucking
Bucking <- getBucking(
  XMLNode,
  PriceMatrices,
  ProductData,
  StemProfile,
  LengthClasses
)

# Optimal bucking
OptimalBucking <- buckHpr(
  XMLNode,
  PriceMatrices,
  ProductData,
  StemProfile,
  PermittedGrades,
  SpeciesGroupDefinition
)
```

## Citation

If you use `optBuck` in scientific work, please cite:

> Noordermeer L (2026). *optBuck: An R package for handling single-grip forest harvester data and bucking optimization*. R package. https://github.com/SmartForest-no/optBuck

## Funding

The package was developed as part of the SmartForest project, funded by the Research Council of Norway (project no. 309671).
