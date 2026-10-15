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
| **NFI data.** Spruce and pine from the Norwegian National Forest Inventory: all trees on all plots of the selected species, maturity class, site class and county. Results describe the selected trees only. | Available |
| **Tree lists.** Upload your own trees: tree ID, species and diameter, with heights where available. Missing heights are estimated (see below). | Available |
| **Stem profiles.** Upload measured diameter-along-stem profiles, for example from harvester data, used directly without a taper model. | Planned |

### Tree lists

Choose *Uploaded tree list* in the Simulator and drop a CSV or xlsx file
with one row per tree (a template can be downloaded there):

| Column | Content |
|--------|---------|
| `tree_id` | Required. Unique within its plot (or within the file if there are no plots) |
| `species` | Required. `spruce` or `pine` |
| `dbh` | Required. Diameter at breast height over bark, in cm |
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

---

## Assortments and prices

The app opens on *Assortments and prices*, with **Scenario A** on the left and
**Scenario B** on the right (one above the other on narrow screens). Each
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
volume method), with *Add*, *Edit* and *Remove*. The form also holds the
minimum butt diameter, where the top diameter is read (cm below the cut)
and the pricing: a price matrix, or one flat price over a range of lengths
(every 10 cm). A matrix-priced assortment's lengths are its matrix columns.
Diameter rounding follows the volume method (below). Add assortments such
as poles, large-diameter or small sawlogs and special timber; *Spruce and
pine* adds one of each. Stems are assumed free of defects; no quality
restrictions are applied. Added assortments always count as saleable
volume.

**Two scenarios.** B starts as an exact copy of A; after that the two are
independent and both can be edited. Values and rows that differ from the
other scenario are highlighted on both sides, and an assortment that is
missing on one side is listed there as *Not in A* or *Not in B*. Each
assortment's pricing (price matrix or flat price) is set per scenario, so,
for example, spruce pulpwood can have a price matrix in A and a flat price in
B. *Make assortment equal to B* (left column) and *Make assortment equal to
A* (right column) copy one assortment from the other scenario, and *Make
matrix equal to B* or *A* copies one price matrix. To copy a whole
scenario, download it and upload the file to the other side. Both assortment tables list the
assortments in the same order (by species: sawlog, pulpwood, waste, then
added assortments), so the rows line up. A new assortment can be added to both scenarios or to one;
*Remove* asks whether to remove it from this scenario only or from both.

**Price matrices.** Under its assortments, each scenario shows its price
matrices in the layout of the source price lists: top-diameter classes (cm)
as rows and length classes (dm) as columns, in NOK/m³ under bark. The two
scenarios each have their own matrix selector. Cells that differ from the
other scenario are highlighted. Both scenarios can be edited, reset,
uploaded and downloaded.

**Scenario files.** Each scenario can be downloaded and uploaded as one xlsx
file with its assortments (sheet `assortments`) and its prices (one sheet
per price matrix plus `flat_prices`). A file loaded into one scenario
replaces that scenario's assortments and prices.

A matrix's columns are the assortment's allowed lengths and its rows its
top-diameter classes. Lengths can be added and removed (a removed length is
no longer cut); diameter classes are added or removed at the smallest and
largest end, so there are no gaps. A log with a top diameter above the
largest class is priced at that class (50 cm in the 2021 matrices); its
physical diameter is not changed.

**Uploading prices.** Price matrices can also be loaded on their own: drop a
file on a scenario's price-matrix upload box, wide (as in the
editor; xlsx with one sheet per price matrix plus `flat_prices`, or a CSV
holding one matrix) or long (CSV or RDS, one row per price). The layout is
detected automatically. A wide file replaces only the matrices it contains;
in B, uploaded assortments get their own prices. A downloaded price matrix
file is a ready-made template. The long layout has these columns:

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

In the *Simulator*, choose the trees and click *Run optimizer*. NFI runs use
all plots of the selected stratum; for selections above 30,000 trees the
app shows the number of trees and an estimated run time. A progress bar
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
  comparison, total value, commercial volume, NOK per m³ and price volume
  for Scenario A and Scenario B side by side
- **Per m³:** NOK/m³ in total, per species and by assortment, and per-tree
  results; select a tree to see its logs (position, length, assortment, top
  diameter over and under bark, price-diameter class, volumes and value)
- **Products:** value and sale volume by assortment; after a comparison,
  for Scenario A and Scenario B side by side
- **Selected logs:** every log cut, a page at a time, with a CSV download of
  all logs
- **Scenario comparison** (when B differs from A): NOK/m³, value and
  volume under A and B and the differences, the trees whose cutting pattern
  changed, and example stems bucked both ways. A tree counts as changed only
  if the change alters its value or volume; patterns that differ only
  between equal-value alternatives are not counted.

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
- Sale volume is measured per product, with one of four methods:
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
                   "writexl", "arrow", "dplyr", "readr"))

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
