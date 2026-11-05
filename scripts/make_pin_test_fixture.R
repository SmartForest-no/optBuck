# ==============================================================================
# scripts/make_pin_test_fixture.R
#
# Purpose : Build the synthetic .pin files audit 32 reads (Phase 5b):
#             inputs/test_fixtures/pin_minimal_synthetic.pin  hand-made layout:
#               a header with two dates, four products, no PriceDefinition, no
#               DiameterMINTop, no top position, no grades
#             inputs/test_fixtures/pin_full_synthetic.pin     machine layout:
#               header with sender, Extension blocks, permitted grades,
#               PriceDefinition, DiameterMINTop/MAXButt, top position, cutting
#               window, distribution and limitation, LogColorMarking, and cells
#               of every BuckingCriteria kind
#
# Privacy : everything is invented here from literals. No real .pin or .hpr file
#           is read or copied. The script refuses to write a file that holds a
#           coordinate or contact element (the audit injects invented
#           coordinates into a temp copy to test that they are never read).
#
# Usage   : Rscript scripts/make_pin_test_fixture.R
# ==============================================================================

root <- local({
  d <- normalizePath(Sys.getenv("APTER_ROOT", getwd()), winslash = "/", mustWork = FALSE)
  while (!file.exists(file.path(d, "R", "helpers_paths.R")) && dirname(d) != d) d <- dirname(d)
  d
})
dir <- file.path(root, "inst", "extdata", "inputs", "test_fixtures")

ROOT_OPEN <- paste0('<ProductInstruction xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ',
                    'xmlns:xsd="http://www.w3.org/2001/XMLSchema" messageType="pin" ',
                    'diameterUnit="mm" lengthUnit="cm" version="3.3" versionDate="2017-02-06" ',
                    'xmlns="urn:skogforsk:stanford2010">')

cell <- function(d, l, price, crit, full = FALSE)
  sprintf(paste0('<ProductMatrixItem diameterClassLowerLimit="%d" lengthClassLowerLimit="%d"><Price>%s</Price>',
                 '<Distribution>0</Distribution><Limitation>0</Limitation><BuckingCriteria>%s</BuckingCriteria>%s',
                 '</ProductMatrixItem>'),
          d, l, price, crit,
          if (full) '<LogColorMarking><Color1>false</Color1><Color2>false</Color2><Color3>false</Color3></LogColorMarking>' else "")
classes <- function(x) paste0("<DiameterClass><DiameterClassLowerLimit>", x, "</DiameterClassLowerLimit></DiameterClass>", collapse = "")
lengths_ <- function(x) paste0("<LengthClass><LengthClassLowerLimit>", x, "</LengthClassLowerLimit></LengthClass>", collapse = "")

# ---- minimal layout --------------------------------------------------------------------
mini_product <- function(uid, name, species, d_classes, d_max, l_classes, l_max, prices) {
  cells <- unlist(lapply(seq_along(d_classes), function(i) lapply(seq_along(l_classes), function(j)
    cell(d_classes[i], l_classes[j], prices[i, j], "Price"))))
  paste0('<ProductDefinition><ProductUserID agency="Sample">', uid, "</ProductUserID><ClassifiedProductDefinition>",
         "<ProductName>", name, "</ProductName><ProductInfo>Synthetic</ProductInfo>",
         '<SpeciesGroupUserID agency="Sample">', species, "</SpeciesGroupUserID>",
         "<DiameterDefinition><DiameterClasses>", classes(d_classes), "<DiameterClassMAX>", d_max,
         "</DiameterClassMAX></DiameterClasses></DiameterDefinition>",
         "<LengthDefinition>", lengths_(l_classes), "<LengthClassMAX>", l_max, "</LengthClassMAX></LengthDefinition>",
         "<ProductMatrixes>", paste(cells, collapse = ""), "</ProductMatrixes></ClassifiedProductDefinition></ProductDefinition>")
}
minimal <- paste0(
  '<?xml version="1.0" encoding="utf-8"?>\n', ROOT_OPEN,
  "<ProductInstructionHeader><CreationDate>2024-05-01T12:00:00+03:00</CreationDate>",
  "<ModificationDate>2024-05-01T12:00:00+03:00</ModificationDate></ProductInstructionHeader>\n",
  mini_product("PS1", "Pine Sawlog", "Pine", c(180, 250, 320), 500, c(310, 430), 550,
               matrix(c(62, 66, 68, 72, 70, 74), 3, byrow = TRUE)), "\n",
  mini_product("PP1", "Pine Pulpwood", "Pine", c(60, 100), 250, c(270, 300), 310,
               matrix(c(30, 32, 34, 36), 2, byrow = TRUE)), "\n",
  mini_product("SS1", "Spruce Sawlog", "Spruce", c(180, 250, 320), 500, c(310, 430), 550,
               matrix(c(60, 64, 66, 70, 68, 72), 3, byrow = TRUE)), "\n",
  mini_product("SP1", "Spruce Pulpwood", "Spruce", c(60, 100), 250, c(270, 300), 310,
               matrix(c(28, 30, 32, 34), 2, byrow = TRUE)), "\n",
  "</ProductInstruction>\n")

# ---- full layout -----------------------------------------------------------------------
ext <- paste0("<Extension><BuckingType>Value</BuckingType><SortingPile>NoPile</SortingPile>",
              "<BuckingPriority>Middle</BuckingPriority><LowerCuttingWindow>5</LowerCuttingWindow>",
              "<UpperCuttingWindow>194</UpperCuttingWindow></Extension>")
extras <- paste0("<ManualFreeBuck>true</ManualFreeBuck><TopLogFreeBuck>true</TopLogFreeBuck>",
                 "<ProductCondition>Buck always</ProductCondition>",
                 '<CuttingWindowDefinition modificationRestricted="false"><LowerLengthLimit>1</LowerLengthLimit>',
                 "<UpperLengthLimit>19</UpperLengthLimit></CuttingWindowDefinition>")
tail_defs <- paste0('<LengthDistributionDefinition modificationRestricted="false"><DistributionAllowed>true</DistributionAllowed>',
                    "<DistributionCategory>Volume of logs</DistributionCategory><MAXDeviation>5.00000</MAXDeviation>",
                    "<DistributionWithinProduct>true</DistributionWithinProduct></LengthDistributionDefinition>",
                    '<LimitationDefinition modificationRestricted="false"><LimitationCategory>No limitation</LimitationCategory>',
                    "<LimitationResult>No action</LimitationResult></LimitationDefinition>")
grades <- function(g) paste0('<PermittedGradesDefinition modificationRestricted="false">',
                             paste0("<PermittedGradeNumber>", g, "</PermittedGradeNumber>", collapse = ""),
                             "<GradeToleranceTop>20</GradeToleranceTop><GradeToleranceButt>20</GradeToleranceButt>",
                             "</PermittedGradesDefinition>")
NOLIM <- "No limit"
MAN   <- "Only logs bucked manually allowed"
EXCL  <- "The log is excluded from apportionment bucking,only bucking according to value allowed"
FORB  <- "Forbidden to buck the log even manually"

pulp_cells <- c(
  cell(30, 290, 20, MAN, TRUE),  cell(30, 330, 25, NOLIM, TRUE), cell(30, 360, 30, NOLIM, TRUE),
  cell(50, 290, 40, NOLIM, TRUE), cell(50, 330, 45, NOLIM, TRUE), cell(50, 360, 50, EXCL, TRUE),
  cell(70, 290, 55, NOLIM, TRUE), cell(70, 330, 60, NOLIM, TRUE), cell(70, 360, 65, NOLIM, TRUE))
pulp <- paste0(
  '<ProductDefinition><ProductUserID agency="Other">PULP_Pine</ProductUserID><ClassifiedProductDefinition>',
  "<ProductName>Pine Pulpwood Full</ProductName><ModificationDate>2025-10-22T17:56:46.413+02:00</ModificationDate>",
  '<ProductInfo modificationRestricted="false">100w</ProductInfo><ProductVersion modificationRestricted="false">3</ProductVersion>',
  "<ProductBuyer><Address /><BusinessName>Invented Buyer</BusinessName></ProductBuyer>",
  "<ProductGroupName>MASSAVED</ProductGroupName>", ext, "<ProductPresentationOrder>1</ProductPresentationOrder>",
  '<DiameterDefinition modificationRestricted="false"><DiameterClasses diameterClassCategory="Top">',
  classes(c(30, 50, 70)), "<DiameterClassMAX>400</DiameterClassMAX>",
  "<DiameterClassAdjustment>DiameterClassAdjustment1</DiameterClassAdjustment>",
  "<DiameterUnderBark>true</DiameterUnderBark></DiameterClasses><DiameterTopPosition>10</DiameterTopPosition></DiameterDefinition>",
  '<LengthDefinition modificationRestricted="false"><LengthClassAdjustment>LengthClassAdjustment1</LengthClassAdjustment>',
  paste0("<LengthClass><LengthClassLowerLimit>", c(290, 330, 360),
         "</LengthClassLowerLimit><LengthClassMargin>0</LengthClassMargin></LengthClass>", collapse = ""),
  "<LengthClassMAX>400</LengthClassMAX></LengthDefinition>",
  '<PriceDefinition modificationRestricted="false"><VolumeDiameterAdjustment>Measured diameter in mm</VolumeDiameterAdjustment>',
  '<VolumeDiameterCategory volumeDiameterTopPosition="10">All diameters (solid volume)</VolumeDiameterCategory>',
  "<VolumeLengthCategory>Physical length cm</VolumeLengthCategory><VolumeUnderBark>true</VolumeUnderBark></PriceDefinition>",
  extras, grades(1:7), tail_defs,
  '<ProductMatrixes modificationRestrictedPrice="false">', paste(pulp_cells, collapse = ""), "</ProductMatrixes>",
  '<SpeciesGroupUserID agency="Buyer">SE1_Tall_SoundKnot</SpeciesGroupUserID></ClassifiedProductDefinition></ProductDefinition>')

saw_cells <- c(
  cell(180, 310, 600, NOLIM, TRUE), cell(180, 430, 640, NOLIM, TRUE),
  cell(250, 310, 700, NOLIM, TRUE), cell(250, 430, 740, NOLIM, TRUE),
  cell(320, 310, 760, FORB, TRUE),  cell(320, 430, 800, NOLIM, TRUE))
saw <- paste0(
  '<ProductDefinition><ProductUserID agency="Other">SAW_Spruce</ProductUserID><ClassifiedProductDefinition>',
  "<ProductName>Spruce Sawlog Full</ProductName><ProductGroupName>Tømmer</ProductGroupName>", ext,
  '<DiameterDefinition modificationRestricted="false"><DiameterClasses diameterClassCategory="Top">',
  classes(c(180, 250, 320)), "<DiameterClassMAX>600</DiameterClassMAX>",
  "<DiameterClassAdjustment>DiameterClassAdjustment1</DiameterClassAdjustment>",
  "<DiameterUnderBark>true</DiameterUnderBark></DiameterClasses>",
  "<DiameterMINTop>180</DiameterMINTop><DiameterMAXButt>700</DiameterMAXButt><DiameterTopPosition>0</DiameterTopPosition></DiameterDefinition>",
  '<LengthDefinition modificationRestricted="false"><LengthClassAdjustment>LengthClassAdjustment1</LengthClassAdjustment>',
  paste0("<LengthClass><LengthClassLowerLimit>", c(310, 430),
         "</LengthClassLowerLimit><LengthClassMargin>0</LengthClassMargin></LengthClass>", collapse = ""),
  "<LengthClassMAX>550</LengthClassMAX></LengthDefinition>",
  '<PriceDefinition modificationRestricted="false"><VolumeDiameterAdjustment>', "Measured diameter rounded downwards to cm",
  '</VolumeDiameterAdjustment><VolumeDiameterCategory volumeDiameterTopPosition="0">Top</VolumeDiameterCategory>',
  "<VolumeLengthCategory>Length as defined in LengthClasses</VolumeLengthCategory><VolumeUnderBark>true</VolumeUnderBark></PriceDefinition>",
  extras, grades(1:2), tail_defs,
  '<ProductMatrixes modificationRestrictedPrice="false">', paste(saw_cells, collapse = ""), "</ProductMatrixes>",
  '<SpeciesGroupUserID agency="Buyer">SE1_Gran_Sound</SpeciesGroupUserID></ClassifiedProductDefinition></ProductDefinition>')

full <- paste0(
  '<?xml version="1.0"?>\n', sub("diameterUnit", 'areaUnit="ha" diameterUnit', ROOT_OPEN, fixed = TRUE),
  "<ProductInstructionHeader><SenderApplication>Invented Sender 1.0</SenderApplication>",
  "<CreationDate>2026-03-26T15:18:20.165626+01:00</CreationDate>",
  "<ModificationDate>2026-03-26T15:18:20.165626+01:00</ModificationDate>",
  "<CountryCode>752</CountryCode></ProductInstructionHeader>\n", pulp, "\n", saw, "\n</ProductInstruction>\n")

# ---- write, refusing private elements --------------------------------------------------
forbidden <- "Latitude|Longitude|Altitude|Coordinate|ContactInformation|FirstName|LastName|ObjectName|OperatorDefinition"
for (f in c("pin_minimal_synthetic.pin", "pin_full_synthetic.pin")) {
  txt <- if (f == "pin_minimal_synthetic.pin") minimal else full
  if (grepl(forbidden, txt)) stop("Refusing to write ", f, ": it holds a coordinate or contact element.")
  if (is.null(xml2::read_xml(txt))) stop(f, " is not well-formed.")
  con <- file(file.path(dir, f), "wb"); writeBin(charToRaw(enc2utf8(txt)), con); close(con)
  cat("Wrote", file.path("inputs", "test_fixtures", f), "\n")
}
