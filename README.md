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
| **Tree lists.** Upload your own trees: tree ID, species and diameter, with heights where available. | Planned |
| **Stem profiles.** Upload measured diameter-along-stem profiles, for example from harvester data, used directly without a taper model. | Planned |

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
matrix (spruce sawlog, pine sawlog, and the flat pulpwood and waste prices)
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
editor; xlsx with one sheet per sawlog matrix plus `flat_prices`, or a CSV
holding one grid) or long (CSV or RDS, one row per price). The layout is
detected automatically, and a wide file replaces only the matrices it
contains. Download the wide xlsx template (or the long CSV template) to
start from the default prices. The long layout has these columns:

| Column | Content |
|--------|---------|
| `ProductKey` | Product, e.g. `spruce_sawlog`, `pine_pulpwood`, `spruce_waste` |
| `LengthClass_dm` | Sawlogs: length class in dm (e.g. 31, 34, …, 55). Empty for pulpwood and waste |
| `PriceDiameterClass_cm` | Sawlogs: top-diameter class in cm. Empty for pulpwood and waste |
| `PriceNOKperM3` | Price in NOK per m³ |

Uploads are checked before they are used: missing columns, non-numeric or
negative prices, unknown products, length classes that are not defined,
duplicated cells, and missing sawlog prices are rejected with a message;
diameter classes that are never used in pricing are reported as warnings.
Prices are never converted between units. A scenario can be downloaded as a
long CSV.

**Planned:**

- Custom assortments, such as poles, large-diameter or small sawlogs and
  special timber, with their own diameter and length limits and price
  matrices.
- A choice of sale-volume method per product.

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
- Sawlog sale volume is a cylinder at the under-bark top diameter (rounded
  down to whole cm) over the log length. Pulpwood and waste sale volume is
  the solid volume under bark: 10 cm sections, each at its under-bark top
  diameter, the same rule as the commercial volume. A choice of volume
  method per product is planned.
- Sawlog top-diameter classes above 50 cm are priced as 50 cm. The physical
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

# taperNOR is not on CRAN:
devtools::install_git("https://github.com/SmartForest-no/taperNOR")
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
Research Council of Norway (grant no. 309671).
