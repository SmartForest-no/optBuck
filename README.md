# Apter

Apter is a bucking simulator for Norway spruce and Scots pine. For each stem,
it finds the cutting pattern that gives the highest value under your product
definitions and price matrix. Use it to compare price matrices and to plan
bucking: see how a change in prices or product specifications changes the
logs that are cut and the value recovered.

The name comes from the Norwegian *aptering* (bucking).

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

## Tree sources

| Source | Status |
|--------|--------|
| **NFI sample trees.** Spruce and pine from the Norwegian National Forest Inventory, selected by species, maturity class, site class and county. Results describe the selected trees only. | Available |
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
listed under Diagnostics.

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

## Products and price matrices

Apter ships with a default product set and the 2021 price matrices for spruce
and pine:

| Product | Top diameter | Lengths |
|---------|--------------|---------|
| Sawlog (spruce, pine) | ≥ 12 cm | 3.1–5.5 m in 30 cm steps |
| Pulpwood (spruce, pine) | ≥ 5 cm | 3.0–5.5 m in 10 cm steps |
| Waste (spruce, pine) | any | 0.1–6.0 m |

The pulpwood prices in the default set are temporary flat values, and waste
is priced at 0 NOK/m³.

**Comparing price matrices.** On the *Prices and assortments* page, each
matrix (spruce sawlog, pine sawlog, any custom assortments, and the flat
prices)
is shown as the default (read-only) next to an editable scenario, in the
layout of the source price lists: top-diameter classes (cm) as rows and
length classes (dm) as columns. Cells that differ from the default are
highlighted, and each matrix can be reset to the default. Tick *Compare with
scenario prices* in the Simulator to run the same trees under both matrices.
The comparison shows NOK/m³ and volume shares by assortment under both
matrices, the trees whose cutting pattern changed, and example stems bucked
both ways. A tree counts as changed only if the change alters its value or
volume; patterns that differ only between equal-value alternatives are not
counted.

**Uploading a scenario.** Drop a file on the upload box: wide (as in the
editor; xlsx with one sheet per price grid plus `flat_prices`, or a CSV
holding one grid) or long (CSV or RDS, one row per price). The layout is
detected automatically, and a wide file replaces only the matrices it
contains. Download the wide xlsx template (or the long CSV template) to
start from the default prices. The long layout has these columns:

| Column | Content |
|--------|---------|
| `ProductKey` | Product, e.g. `spruce_sawlog`, `pine_pulpwood`, `spruce_waste`, or a custom assortment such as `spruce_poles` |
| `LengthClass_dm` | Grid-priced products: length class in dm (e.g. 31, 34, …, 55). Empty for flat-priced products |
| `PriceDiameterClass_cm` | Grid-priced products: top-diameter class in cm. Empty for flat-priced products |
| `PriceNOKperM3` | Price in NOK per m³ |

Uploads are checked before they are used: missing columns, non-numeric or
negative prices, unknown products, length classes that are not defined,
duplicated cells, and missing sawlog prices are rejected with a message;
diameter classes below a product's minimum top diameter, and a largest
class that differs from the default, are reported as warnings.
Prices are never converted between units. A scenario can be downloaded as a
long CSV.

**Custom assortments.** Add assortments such as poles, large-diameter or
small sawlogs and special timber on the *Prices and assortments* page. Each
assortment has:

- species (spruce, pine or both), minimum and maximum top diameter, and
  optionally minimum and maximum butt diameter (cm);
- the allowed log lengths (cm);
- a priority, which decides between equally valued cutting patterns;
- whether diameters are measured under bark, and where the top diameter is
  measured (cm below the cut);
- a sale-volume method (below);
- prices: a grid of length classes by top-diameter classes, like the sawlog
  matrices, or one flat price;
- whether it is used in the default run, the scenario run or both. Using it
  in the scenario only shows what adding the assortment changes.

New assortments appear in the price editor next to the base products, and
their default prices can be edited there. Editing an assortment keeps the
prices of every grid cell that still exists; new cells start at the
starting price. Assortments are shown in the
results as their own class, and always count as saleable volume. Stems are
assumed free of defects; no quality restrictions are applied. Assortments,
with their default and scenario prices, can be downloaded and uploaded as
one xlsx file.

---

## Results

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

The app also shows:

- Value (NOK) and sale (price) volume by product
- Per-tree results on demand, with each tree's logs: position, length,
  product, top diameter over and under bark, price-diameter class, volumes
  and value
- Diagnostics: price-matrix checks, candidates without a price, and stems
  with no saleable logs
- For a price-matrix comparison: NOK/m³, value and volume under both
  matrices, and the differences

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
  - **Top cylinder** (the default for sawlogs): a cylinder at the
    under-bark top diameter, rounded down to whole cm, over the log length.
  - **Top**: a frustum from the top diameter, widening by 1 cm per metre
    towards the butt.
  - **Solid volume** (the default for pulpwood and waste): 10 cm sections,
    each at its under-bark top diameter, the same rule as the commercial
    volume.
  - **Calculated Norwegian mid**, as in the Målereglement for skogsvirke
    (Norsk Virkesmåling, 2026, sections 4.1.3–4.1.4): the top diameter
    rounded down to whole cm plus 0.5 cm, plus 1 cm per metre over half the
    log length, V = 3.14/4 · D² · L.
- A log with a top diameter above a price grid's largest diameter class is
  priced at that class (50 cm in the default sawlog matrices). The physical
  diameter is not changed.
- Each stem is bucked independently. When two cutting patterns are worth
  the same (within 0.000001 NOK), the one with fewer logs is chosen, then
  the higher-priority product, then the longer log, so results do not
  depend on input order.

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

The NFI sample data used by the app are included in the repository in a
sanitized form, without plot locations or original plot IDs. No database
access is needed.

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
