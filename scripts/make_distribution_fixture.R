# ==============================================================================
# scripts/make_distribution_fixture.R
#
# Purpose : Build the small synthetic product files audit 34 reads (Phase 7a):
#             inputs/test_fixtures/distribution_synthetic.hpr  (.hpr, no stems)
#             inputs/test_fixtures/distribution_synthetic.pin  (.pin)
#           Both hold the same two products per species group:
#             "Spruce Sawlog"  with a LengthDistributionDefinition and a target
#                              Distribution per cell (30 / 45 / 25 on three
#                              lengths, 0 on the shortest; one diameter class
#                              has 20 / 50 / 30)
#             "Spruce Pulpwood" without any distribution data
#
# Privacy : everything is invented here from literals; no real file is read.
#
# Usage   : Rscript scripts/make_distribution_fixture.R
# ==============================================================================

root <- local({
  d <- normalizePath(Sys.getenv("APTER_ROOT", getwd()), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "R", "helpers_paths.R")) && dirname(d) != d) d <- dirname(d)
  d
})
dir <- file.path(root, "inst", "extdata", "inputs", "test_fixtures")

classes <- function(x) paste0("<DiameterClass><DiameterClassLowerLimit>", x, "</DiameterClassLowerLimit></DiameterClass>", collapse = "")
lclasses <- function(x) paste0("<LengthClass><LengthClassLowerLimit>", x, "</LengthClassLowerLimit><LengthClassMargin>0</LengthClassMargin></LengthClass>", collapse = "")
cell <- function(d, l, price, dist)
  sprintf(paste0('<ProductMatrixItem diameterClassLowerLimit="%d" lengthClassLowerLimit="%d"><Price>%s</Price>',
                 '%s<Limitation>0</Limitation><BuckingCriteria>No limit</BuckingCriteria></ProductMatrixItem>'),
          d, l, price, if (is.na(dist)) "" else sprintf("<Distribution>%s</Distribution>", dist))

saw_d <- c(180, 250, 320); saw_l <- c(370, 430, 490, 550)
dist_for <- function(d, l) {
  v <- if (d == 250) c(0, 20, 50, 30) else c(0, 30, 45, 25)
  v[match(l, saw_l)]
}
saw_cells <- paste(unlist(lapply(saw_d, function(d) lapply(saw_l, function(l)
  cell(d, l, 400 + d / 10 + l / 10, dist_for(d, l))))), collapse = "")
pulp_d <- c(60, 100); pulp_l <- c(300, 310)
pulp_cells <- paste(unlist(lapply(pulp_d, function(d) lapply(pulp_l, function(l)
  cell(d, l, 250 + l / 100, NA)))), collapse = "")

body <- function(name, grp, d, l, dmax, cells, ld, extra_end) paste0(
  "<ProductName>", name, "</ProductName><ProductGroupName>", grp, "</ProductGroupName>",
  "<DiameterDefinition><DiameterClasses>", classes(d), "<DiameterClassMAX>", dmax, "</DiameterClassMAX></DiameterClasses>",
  "<DiameterMINTop>", min(d), "</DiameterMINTop></DiameterDefinition>",
  "<LengthDefinition>", lclasses(l), "<LengthClassMAX>", max(l) + 9, "</LengthClassMAX></LengthDefinition>",
  '<PriceDefinition><VolumeDiameterAdjustment>Rounded downwards to nearest cm</VolumeDiameterAdjustment>',
  '<VolumeDiameterCategory volumeDiameterTopPosition="0">Top</VolumeDiameterCategory>',
  '<VolumeLengthCategory>Length as defined in LengthClasses</VolumeLengthCategory></PriceDefinition>',
  ld, "<ProductMatrixes>", cells, "</ProductMatrixes>", extra_end)
ld_xml <- paste0('<LengthDistributionDefinition modificationRestricted="false"><DistributionAllowed>true</DistributionAllowed>',
                 "<DistributionCategory>Volume of logs</DistributionCategory><MAXDeviation>4</MAXDeviation>",
                 "<DistributionWithinProduct>true</DistributionWithinProduct></LengthDistributionDefinition>")

# ---- .hpr ----------------------------------------------------------------------------
hpr <- paste0(
  '<?xml version="1.0" encoding="utf-8"?>\n',
  '<HarvestedProduction messageType="hpr" diameterUnit="mm" lengthUnit="cm" version="3.3" ',
  'xmlns="urn:skogforsk:stanford2010"><Machine>',
  "<SpeciesGroupDefinition><SpeciesGroupKey>1</SpeciesGroupKey><SpeciesGroupName>Spruce</SpeciesGroupName></SpeciesGroupDefinition>",
  "<ProductDefinition><ProductKey>101</ProductKey><ClassifiedProductDefinition>",
  body("Spruce Sawlog", "Sawlog", saw_d, saw_l, 600, saw_cells, ld_xml, "<SpeciesGroupKey>1</SpeciesGroupKey>"),
  "</ClassifiedProductDefinition></ProductDefinition>",
  "<ProductDefinition><ProductKey>102</ProductKey><ClassifiedProductDefinition>",
  body("Spruce Pulpwood", "Pulpwood", pulp_d, pulp_l, 300, pulp_cells, "", "<SpeciesGroupKey>1</SpeciesGroupKey>"),
  "</ClassifiedProductDefinition></ProductDefinition>",
  "</Machine></HarvestedProduction>\n")

# ---- .pin ----------------------------------------------------------------------------
pin <- paste0(
  '<?xml version="1.0" encoding="utf-8"?>\n',
  '<ProductInstruction messageType="pin" diameterUnit="mm" lengthUnit="cm" version="3.3" ',
  'xmlns="urn:skogforsk:stanford2010">',
  "<ProductDefinition><ProductUserID>SS1</ProductUserID><ClassifiedProductDefinition>",
  body("Spruce Sawlog", "Sawlog", saw_d, saw_l, 600, saw_cells, ld_xml, "<SpeciesGroupUserID>Spruce</SpeciesGroupUserID>"),
  "</ClassifiedProductDefinition></ProductDefinition>",
  "<ProductDefinition><ProductUserID>SP1</ProductUserID><ClassifiedProductDefinition>",
  body("Spruce Pulpwood", "Pulpwood", pulp_d, pulp_l, 300, pulp_cells, "", "<SpeciesGroupUserID>Spruce</SpeciesGroupUserID>"),
  "</ClassifiedProductDefinition></ProductDefinition>",
  "</ProductInstruction>\n")

for (x in list(list(hpr, "distribution_synthetic.hpr"), list(pin, "distribution_synthetic.pin"))) {
  if (grepl("StemCoordinates|Latitude|Longitude|ObjectName", x[[1]])) stop("coordinate or contact field in fixture")
  writeBin(charToRaw(enc2utf8(x[[1]])), file.path(dir, x[[2]]))
  cat("wrote", x[[2]], "\n")
}
