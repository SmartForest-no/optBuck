# Apter

Apter is a bucking simulator for Norway spruce and Scots pine. For each stem,
it finds the cutting pattern that gives the highest value under your product
definitions and price matrix. Use it to compare price matrices and to plan
bucking: see how a change in prices or product specifications changes the
logs that are cut and the value recovered.

The name comes from the Norwegian *aptering* (bucking). In the app itself
the simulator appears as **optBuck**.

---

## How it works

1. **Stem profile.** Each tree is described by its diameter along the stem.
   For trees given by diameter and height, the profile comes from the
   Norwegian taper models in [taperNOR](https://github.com/SmartForest-no/taperNOR).
2. **Candidate logs.** Every possible log is generated on a 10 cm grid along
   the stem, from a 10 cm stump upwards, for each product whose length and
   diameter limits it meets.
3. **Pricing.** Each candidate is valued with the price matrix: sawlogs by
   length class and top-diameter class, pulpwood and waste at a flat price
   per m³.
4. **Optimal bucking.** Dynamic programming finds the combination of
   non-overlapping logs with the highest total value for each stem.

---

## Data sources

| Source | Status |
|--------|--------|
| **NFI data.** Spruce and pine from the Norwegian National Forest Inventory. The app takes every plot of the selected maturity class, site class, county and altitude range (metres above sea level, in 50 m steps) that has trees of the selected species, and bucks the trees of that species (spruce, pine or both) with a diameter at breast height of at least 5 cm and a height of at least 4.5 m. Other tree species are not included. Results describe the selected trees only. | Available |
| **Tree lists.** Upload your own trees: tree ID, species and diameter, with heights where available. Missing heights are estimated (see below). | Available |
| **Stem profiles.** Upload measured diameter-along-stem profiles, for example from harvester data, used directly without a taper model. | Planned |
| **Harvester stems (.hpr).** Buck the stems a harvester measured, read from a StanForD 2010 .hpr file (see below). Products and prices can be imported from the same files; see *Assortments and prices*. | Available |

### Tree lists

Choose *Uploaded tree list* in the Simulator and drop a CSV or xlsx file
with one row per tree (a template can be downloaded there):

| Column | Content |
|--------|---------|
| `tree_id` | Required. Unique within its plot (or within the file if there are no plots) |
| `species` | Required. `spruce` or `pine` |
| `dbh` | Required. Diameter at breast height (1.3 m above ground) over bark, in cm |
| `height` | Optional. Tree height in m; leave empty where not measured |
| `plot_id` | Optional. Groups trees for the height model |

The file is checked before a run. Missing columns, non-numeric or missing
values, duplicated tree IDs, unknown species and zero or negative sizes are
errors. Likely unit errors (a dbh that looks like mm, a height that looks
like dm), implausible height/diameter ratios and trees outside the taper
model's range are warnings. Values are never converted between units. Trees
below 5 cm dbh or 4.5 m height are not bucked, as for NFI trees; they are
listed in the check panel.

**Missing heights** are estimated per species, and every tree reports which
method gave its height:

- **10 or more measured heights:** a height-diameter model fitted to your
  trees with [fieldPlot](https://github.com/lennartnoordermeer/fieldPlot)
  (Näslund's model with a plot effect; within a plot that has measured
  heights the plot's curve is used, elsewhere the species curve).
- **1 to 9 measured heights:** the NFI reference height curve (FinnHoyde),
  calibrated to your measured trees: it is multiplied by the ratio of their
  measured heights to the curve's heights at their diameters, as in the NFI
  tariff method. A ratio below 0.7 or above 1.3 is reported.
- **No measured heights:** the NFI reference height curve (FinnHoyde)
  uncalibrated. These are **average curves that do not depend on site**, so
  heights can be biased for your trees; the app shows a warning.

FinnHoyde curves (d in cm, h in m): spruce, d < 15: 1.3 + 0.6·d (Eid & Fitje
1993); d ≥ 15: −14.17 + 20.86·log10(d) (Fitje & Vestjordet 1977). Pine,
d < 10: 1.3 + 0.65456·d (Eid & Fitje 1993); 10 ≤ d < 35: 0.39 + 0.852·d −
0.010644·d² (Vestjordet 1968); d ≥ 35: 13.35 + 0.107·d (Eid & Fitje 1993).

### Harvester stems (.hpr)

Choose *.hpr stems* in the Simulator. If you imported an .hpr file into a
scenario in the same session, that file is used, with the species you
confirmed there; otherwise choose a file (.hpr, or an .hpr in a .zip or .gz,
up to 200 MB). Large files are read stem by stem; a 150 MB file takes about
15 seconds. Each species group in the file is set to spruce, pine or not used.

- **The measured stem is bucked as it is.** Positions are measured from the
  felling cut, so there is no stump. Only the measured part is used: nothing
  above the last measured diameter is simulated, and no taper model is
  involved. Diameters that increase locally are kept.
- **Bark and DBH.** The harvester's diameters are over bark; under-bark
  diameters come from the same bark model as for other trees. DBH is read
  from the profile at 1.3 m above ground: the stump height is taken as 130 cm
  minus the file's DBH height (the height above the felling cut where the
  harvester measures DBH).
- **Stems that are not bucked.** One line shows how many stems were read and
  how many are used (for example "3,176 stems read, 2,676 used."). These
  stems are left out without a warning, as expected:
  - stems of species groups set to not used;
  - multi-tree stems, and stems without a diameter profile;
  - profiles that end below 1.3 m above ground (no DBH can be read).

  These are left out with a warning, because they point to a problem in the
  file (the warning gives the number of stems and the reason):
  - profiles recorded under bark (not supported yet);
  - diameters above 1,500 mm (likely a unit error);
  - a stem key used twice (the later stems are left out);
  - profiles that do not start at the felling cut.
- **What is read.** Only each stem's key, species group and diameter
  profile. Coordinates, dates, operators and names are never read. The logs
  the harvester actually cut are not used yet; comparing them with the
  optimal bucking is planned.

The results are the same as for tree lists, with the stem key as tree ID and
no plots. The height column shows the measured length.

---

## Assortments and prices

The app opens on *Assortments and prices*, with **Scenario A** and
**Scenario B** as two white cards side by side on a light grey page (one
above the other on narrow screens). Each card runs from top to bottom: the
assortment table, the selected assortment with its prices, and the
scenario's own buttons (add, reset, copy from the other scenario, download,
upload, import from .hpr). Each
scenario has its own assortments and its own price matrices. Apter ships
with this base set and the 2021 price matrices for spruce and pine
(diameters under bark); both scenarios start from it:

| Assortment | Min top diameter | Max top diameter | Max butt diameter | Lengths |
|------------|------------------|------------------|-------------------|---------|
| Sawlog, spruce | 12 cm | 60 cm | 65 cm | 3.1–5.5 m in 30 cm steps |
| Sawlog, pine | 12 cm | 50 cm | 60 cm | 3.1–5.5 m in 30 cm steps |
| Pulpwood (spruce, pine) | 5 cm | 70 cm | 70 cm | 3.0–5.5 m in 10 cm steps |
| Waste (spruce, pine) | any | – | – | 0.1–6.0 m |

The pulpwood prices in the base set are temporary flat values, and waste is
priced at 0 NOK/m³. *Reset to defaults* restores Scenario A's base set;
assortments you added stay.

**Assortments.** Each scenario lists its assortments in a compact table
(assortment, species, top diameter, maximum butt diameter, lengths and
volume method). Click a row to select an assortment: the panel below the
table shows its rules with *Edit*, its prices directly underneath, and
*Remove*. *Add assortment* is at the bottom of the scenario. The form also holds the
minimum butt diameter, where the top diameter is read (cm below the cut)
and the pricing: a price matrix, or one flat price over a range of lengths
(every 10 cm). A matrix-priced assortment's lengths are its matrix columns.
Diameter rounding follows the volume method (below). Add assortments such
as poles, large-diameter or small sawlogs and special timber; *Spruce and
pine* adds one of each. Stems are assumed free of defects; no quality
restrictions are applied. Added assortments always count as saleable
volume.

**Two scenarios.** B starts as an exact copy of A; after that the two are
independent and both can be edited. Each scenario lists only its own
assortments. Values that differ from the same assortment in the other
scenario are highlighted on both sides; an assortment that only one
scenario has is not highlighted. Each
assortment's pricing (price matrix or flat price) is set per scenario, so,
for example, spruce pulpwood can have a price matrix in A and a flat price in
B. Selecting an assortment in one scenario selects it in the other too; if
it is missing there, that side shows no details for it. *Make assortment equal to B* (left column) and
*Make assortment equal to A* (right column) copy the selected assortment,
rules and prices, from the other scenario. *Copy from A* (in Scenario B) and
*Copy from B* (in Scenario A) copy the whole other scenario, all assortments
and prices, after a confirmation. Both assortment tables list the
assortments in the same order (by species: sawlog, pulpwood, waste, then
added assortments). A new assortment can be added to both scenarios or to one;
*Remove* asks whether to remove it from this scenario only or from both.

**Prices.** The selected assortment's prices are shown under its rules. A
matrix-priced assortment shows its price matrix in the layout of the source
price lists: top-diameter classes (cm) as rows and length classes (dm) as
columns, in NOK/m³ under bark. A flat-priced assortment shows one price
field. Cells or prices that differ from the other scenario are
highlighted. Each assortment's prices can be edited, reset (base
assortments), uploaded and downloaded on their own.

**Scenario files.** Each scenario can be downloaded and uploaded as one xlsx
file with its assortments (sheet `assortments`) and its prices (one sheet
per price matrix plus `flat_prices`). A file loaded into one scenario
replaces that scenario's assortments and prices.

**Importing from a harvester file (.hpr).** *Import from .hpr* (each
scenario's buttons) reads the products and price matrices of a StanForD 2010
harvester production file and replaces that scenario's assortments and
prices; the other scenario is not changed. The file can be an .hpr, or an
.hpr compressed as .zip or .gz, up to 200 MB. Only the definitions at the
start of the file are read, so even large files load in a few seconds.
Before anything is replaced, a dialog shows what the file contains and what
will be imported:

- **Species groups.** Each of the file's species groups is set to Spruce, Pine
  or Not used. The app suggests a choice from the group's name and ID (for
  example GRAN → Spruce, FURU → Pine, LAUV → Not used); products of groups
  set to Not used are not imported.
- **Products.** Every product is listed with its diameter and length classes.
  In a harvester, some products are only given to logs for which the
  operator sets a particular grade: energy wood for rotten logs, or special
  orders such as poles. The app finds each species group's usual grade (the
  grade most of its products allow; a tie keeps all tied grades). Products
  that do not allow it start unticked, marked *Only for grades set by the
  operator (special orders)*. Otherwise the simulated stems, which have no
  defects, would take them everywhere. Any product can be ticked or
  unticked.

Each imported product becomes a matrix-priced assortment, and a Waste
assortment at 0 NOK/m³ is added for each species:

- The minimum and maximum top diameter and the maximum butt diameter are
  the product's own, under bark, as is the position where the top diameter
  is measured.
- Length classes become whole-dm lengths, rounded up so the log stays
  inside its class (a 365 cm class is cut at 370 cm). A class too narrow for
  a whole-dm length is left out.
- Diameter classes become whole-cm rows, each with the price of the class it
  falls in. Where a class starts between whole centimetres (e.g. 207 mm),
  the price changes at the whole centimetre.
- Price cells marked as forbidden to cut are left empty: such logs are never
  chosen. A diameter class forbidden at every length stays in the matrix as
  an empty row.
- The price volume follows the product's volume method (top, Norwegian mid,
  mid or solid volume). For products priced on the length class, the price
  volume uses the class's lower limit (365 cm), not the cut length.

After the import, one line says how many assortments were imported. A
warning follows only when something is lost or replaced: a product that
could not be imported (for example one without priced cells), a length
class left out, or a volume method the app does not have (solid volume is
used instead). The conversions described above (rounded lengths, whole-cm
diameter rows, empty forbidden cells, the top diameter rounded down to cm
for top and Norwegian-mid volumes) always apply and are not reported, nor
is a duplicate product name, which gets the species group's name added
(e.g. *Massevirke (FURU)*). The file's
bark functions, quality grades, length margins and distribution settings
are not used, and prices are read as NOK/m³ under bark. Only the products,
prices and species groups are read; stems, coordinates, dates and names in
the file are not.

A matrix's columns are the assortment's allowed lengths and its rows its
top-diameter classes. Lengths can be added and removed (a removed length is
no longer cut); diameter classes are added or removed at the smallest and
largest end, so there are no gaps, and never above the assortment's
maximum top diameter (a larger class could not be used). A log with a top
diameter above the largest class is priced at that class (50 cm in the 2021
matrices); its physical diameter is not changed. A line under each matrix
states this, for example "≥ 50 cm: priced as 50 up to max top 60 cm".

**Uploading prices.** Prices can also be loaded on their own. A full price
file dropped on a scenario's *Upload scenario* box sets the prices of all
assortments it contains; a file dropped on the selected assortment's price
upload box sets that assortment's prices only (other assortments in the file
are ignored and listed). Files can be wide (as in the editor; xlsx with one
sheet per price matrix plus `flat_prices`, or a CSV holding one matrix) or
long (CSV, one row per price). The layout is detected automatically. A wide
file replaces only the matrices it contains. A downloaded price file is a
ready-made template. The long layout has these columns:

| Column | Content |
|--------|---------|
| `ProductKey` | Assortment and species, e.g. `spruce_sawlog`, `pine_pulpwood`, `spruce_waste`, or an added assortment such as `spruce_poles` |
| `LengthClass_dm` | Matrix-priced assortments: length class in dm (e.g. 31, 34, …, 55). Empty for flat-priced assortments |
| `PriceDiameterClass_cm` | Matrix-priced assortments: top-diameter class in cm. Empty for flat-priced assortments |
| `PriceNOKperM3` | Price in NOK per m³ |

Uploads are checked before they are used: missing columns, non-numeric or
negative prices, unknown assortments, gaps in the diameter classes,
duplicated cells and assortments without prices are rejected with a message;
diameter classes that start above an assortment's minimum top diameter, and
a largest class that differs from the 2021 matrix, are reported as warnings.
Prices are never converted between units.

---

## Results

In the *Simulator*, choose the trees in the sidebar card and click *Run
optimizer*; the results appear in the card beside it. NFI runs use
all plots of the selected stratum. Under the selection, the app shows the
number of plots and trees per species that will be bucked and an estimated
run time, updated as the selection changes (for a tree list, after the
upload). A progress bar
follows the run from loading the data to the summary. When Scenario B
differs from A, in assortments or prices, the same trees are bucked under
both scenarios. Each scenario's NOK/m³ uses its own commercial volume.

All results describe the selected trees only. The headline figure is
**NOK per m³ of commercial volume under bark**:

- **Commercial volume** is the part of the stem allocated to saleable
  (non-waste) products, measured as 10 cm sections, each using the
  under-bark diameter at its top. Bark, the stump, waste sections and the
  top above the last saleable log are excluded. It does not depend on
  prices or on how a product's sale volume is measured.
- NOK/m³ is reported in total and per species (each species over its own
  commercial volume), with each assortment's contribution and its share of
  the commercial volume.

The results have these tabs:

- **Summary:** run time, plots, trees, trees with logs, logs, commercial
  volume, total value, NOK per m³ and sale (price) volume; after a
  comparison, run time, plots, trees and trees with logs (A / B) once, then
  total value, commercial volume, NOK per m³ and price volume for Scenario A
  and Scenario B side by side
- **Per m³:** NOK/m³ in total, per species and by assortment; **value by
  DBH class**: trees, commercial volume, value and NOK/m³ per species and
  5 cm DBH class (… 5–9.9, 10–14.9 … 55–59.9, ≥ 60 cm); after a comparison,
  value and NOK/m³ for Scenario A and B and the difference (each scenario's
  NOK/m³ on its own commercial volume), with a CSV download and one figure
  (trees per class, and NOK/m³ per class for A and B); and per-tree
  results: the height distribution per species, a preview of the
  first 20 trees of Scenario A (select a tree to
  see its logs: position, length, assortment, top diameter over and under
  bark, price-diameter class, volumes and value) and a CSV download of all
  trees (after a comparison, both scenarios, with a Scenario column)
- **Products:** value and sale volume by assortment; after a comparison,
  for Scenario A and Scenario B side by side, with a figure of the value
  (one bar per assortment, largest first)
- **Selected logs:** the **log tally** (stocknota) of one assortment at a
  time: the number of logs or their commercial volume (m³ under bark) per
  price-diameter class and length class, laid out like the assortment's
  price matrix ("–" where the matrix has no price; logs above the largest
  diameter class count in it, as they are priced). Flat-priced assortments
  are tallied per length. After a comparison it shows Scenario A, B or
  B − A; B − A only for assortments both scenarios have. Waste is not
  tallied. Downloads: an xlsx with one sheet per assortment (logs and
  volume, A, B and, for shared assortments, B − A) and a CSV of all cells. Below
  it, a preview of the first 20 logs of Scenario A, and a CSV download of
  every log cut (after a comparison, both scenarios, with a Scenario column)
- **Scenario comparison** (when B differs from A): the differences in
  value, volume and NOK/m³, NOK/m³ by assortment under A and B, the trees whose cutting pattern
  changed, and example stems bucked both ways. The value change by
  assortment is a bar per changed assortment, largest gain first; a line
  under it counts the assortments whose value did not change. A tree counts as changed only
  if the change alters its value or volume; patterns that differ only
  between equal-value alternatives are not counted. When the scenarios have
  different assortments, value and NOK/m³ are compared by assortment type
  (sawlog, pulpwood, other), and per assortment only for the assortments
  both scenarios have; on the Products tab, "–" marks an assortment a
  scenario does not have.

---

## Assumptions

- **Under bark.** Stem profiles from taperNOR are over bark; under-bark
  diameters subtract the double bark thickness from taperNOR's bark model
  (`barkNOR`) along the whole stem. Product limits, price-diameter classes
  and sale volumes all use the under-bark diameter. Bark thickness is
  floored at zero, so the under-bark diameter never exceeds the over-bark
  diameter; this is provisional while the bark model's behaviour near the
  top of large pines is clarified. The bark model is a replaceable
  component.
- Stump height 10 cm; candidate logs on a 10 cm grid.
- Sale volume is measured per product, with one of five methods:
  - **Top cylinder** (sawlogs in the base set): a cylinder at the
    under-bark top diameter, rounded down to whole cm, over the log length.
  - **Top**: a frustum from the top diameter, widening by 1 cm per metre
    towards the butt.
  - **Solid volume** (pulpwood and waste in the base set): 10 cm sections,
    each at its under-bark top diameter, the same rule as the commercial
    volume.
  - **Calculated Norwegian mid**, as in the Målereglement for skogsvirke
    (Norsk Virkesmåling, 2026, sections 4.1.3–4.1.4): the top diameter
    rounded down to whole cm plus 0.5 cm, plus 1 cm per metre over half the
    log length, V = 3.14/4 · D² · L.
  - **Mid** (StanForD "Mid"): a cylinder at the under-bark diameter
    measured at the middle of the log, rounded down to whole cm,
    V = π/4 · D² · L. The price class still follows the top diameter.
- A log with a top diameter above a price matrix's largest diameter class is
  priced at that class (50 cm in the 2021 sawlog matrices). The physical
  diameter is not changed.
- Each stem is bucked independently. When two cutting patterns are worth
  the same (within 0.000001 NOK), the one with fewer logs is chosen, then
  a matrix-priced product before a flat-priced one and waste last, then the
  longer log, so results do not depend on input order.

---

## Running the app

Requirements: R and these packages:

```r
install.packages(c("shiny", "bslib", "reactable", "rhandsontable", "readxl",
                   "writexl", "arrow", "dplyr", "readr", "xml2"))

# taperNOR and fieldPlot are not on CRAN:
devtools::install_git("https://github.com/SmartForest-no/taperNOR")
remotes::install_github("lennartnoordermeer/fieldPlot")   # height models for tree lists (uses nlme)
```

Start the app from the project folder:

```r
shiny::runApp("app")
```

To start it from another folder, set the environment variable `APTER_ROOT`
to the project folder first (for example in `~/.Renviron`).

The NFI data used by the app are included in the repository in a
sanitized form, without plot locations or original plot IDs. No database
access is needed.

### Demo files

The `demo/` folder has files to try the app with:

| File | Content |
|------|---------|
| `scenario_A.xlsx` | Scenario A: the base assortments and prices (upload under Scenario A) |
| `scenario_B.xlsx` | Scenario B: spruce sawlog with a 14 cm minimum top diameter instead of 12 cm, and spruce sawlog prices 8 % higher from 25 cm top diameter (upload under Scenario B) |
| `prices_A.xlsx`, `prices_B.xlsx` | The same scenarios' price matrices only |
| `tree_list_example.csv` | A tree list: two plots, spruce and pine, with some heights missing |

---

## Development

Repository layout, tests and the data pipeline are described in
[docs/development.md](docs/development.md).

---

## Acknowledgement

Apter is developed by NIBIO within SFI SmartForest (smartforest.no), funded by the
Research Council of Norway (RCN project no. 309671).

Stem profiles come from [taperNOR](https://github.com/SmartForest-no/taperNOR).
Heights of uploaded trees are estimated with
[fieldPlot](https://github.com/lennartnoordermeer/fieldPlot) (Lennart
Noordermeer).
