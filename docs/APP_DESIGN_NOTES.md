# nfiBuck — App Design Notes (v1 concept)

**Date:** 2026-06-19  
**Phase:** Planning — no implementation yet

---

## Scope of v1

The first version of the app is a **sampled-plot bucking estimator**, not a full-stratum optimizer. It lets a user configure a stratum, upload prices, and get a quick approximate estimate from a sample of complete plots.

Full-stratum runs (21 000+ trees) are **offline only** in v1. The app clearly labels all estimates as approximate.

---

## User workflow (v1)

```
1. Select stratum
   └── species_group: spruce | pine | both
   └── hogstkl: choose one or more (e.g. 41, 51)
   └── abonitet: choose one or more (e.g. 11, 14, 17)
   └── fylnr (optional): one or more counties
   └── → app shows n_plots and n_trees for the selected combination

2. Set sampling
   └── n_plots: default 10; user may select 25 or 50
   └── → app samples n_plots complete plots (all trees in each plot)
   └── clearly labelled: "Sampled estimate — not ha-expanded"

3. Upload or select price matrix
   └── Option A: upload Excel/CSV file
   └── Option B: use built-in default_2021 matrix (read-only reference)
   └── → app validates the matrix format (see validation rules below)

4. Run bucking optimizer
   └── → taper profiles generated for sampled trees
   └── → candidates generated and valued
   └── → DP optimizer run per tree
   └── → results assembled

5. View results
   └── Product-level: volume, value, n_logs
   └── Plot-level: per-ha estimates (unexpanded), distribution histograms
   └── Zero-log trees: table with DBH and reason
   └── Price-gap report: unpriced candidates by ProductKey + LengthClass_dm
   └── Diameter-cap report: capped candidates and selected logs
   └── Diagnostics: optimizer errors, profile failures
```

---

## Price matrix validation rules

### Format requirements
- Required columns: `ProductKey`, `LengthClass_dm`, `PriceDiameterClass_cm`, `PriceNOKperM3`, `scenario`
- `ProductKey` must contain only values from the active product set
- `PriceDiameterClass_cm` must be integer-valued, minimum 12 cm
- `PriceNOKperM3` must be non-negative

### Completeness checks

| Situation | Action |
|-----------|--------|
| Technically eligible and priced | Included in optimizer |
| Technically eligible but unpriced (some classes) | Excluded from price join; reported as unpriced candidates; run is allowed with warning |
| Technically eligible but unpriced (entire product) | Product is blocked or disabled for this run; user must confirm |
| 31 dm sawlog candidates, no 31 dm row in matrix | Reported as REVIEW (known gap); run is allowed |
| `PriceDiameterClass_cm` > `MAX_PRICE_DIAMETER_CLASS_CM` in uploaded matrix | Warn user; cap is applied for lookup; documented in output |
| Negative `PriceNOKperM3` | Reject matrix; user must correct |
| `PriceNOKperM3` = 0 for a class | Warn; only allowed if user explicitly confirms "treat missing as zero" |

### Missing-price policy (configurable)

- **Default:** missing prices are excluded (candidate unpriced, not selected unless it is the best available product).
- **Override:** user may check "Treat missing prices as zero" for an explicit sensitivity test. This must be clearly labelled in all outputs.
- **Blocked products:** if all price rows for a product are missing or zero, the app blocks that product from the run unless the user disables it explicitly.

### Reporting
- All unpriced candidates are summarised by `ProductKey` + `LengthClass_dm` in a price-gap table.
- Diameter-class capping (`PriceDiameterClass_cm_uncapped > MAX`) is reported separately.
- 31 dm sawlog gap is labelled "KNOWN: sawlog price matrix starts at 34 dm" in the report.

---

## Estimates and labelling

- All volume and value outputs from sampled-plot mode are labelled:
  > **Sampled-plot estimate (n = {n_plots} plots). Not ha-expanded. No NFI factors applied.**
- Simple per-ha values use: `plot_volume / plot_area_m2 × 10 000`
- No NFI expansion factors or plot weights are applied in v1.
- Users may export results but cannot publish them as NFI-representative estimates without further analysis.

---

## App should NOT (v1 constraints)

- Run a 21 000-tree full-stratum interactively — too slow for a web session.
- Apply NFI expansion factors — not yet in scope.
- Import HPR or StanForD files — not in scope for v1.
- Apply bark correction — not implemented.
- Automatically delete or archive any project files.

---

## Technical architecture (proposed)

```
nfiBuck/
  R/
    helpers_*.R          # shared functions (see CLEANUP_PLAN.md)
  app/
    app.R                # Shiny entry point
    ui.R                 # UI definition
    server.R             # server logic
    modules/
      mod_stratum_selector.R
      mod_price_matrix_validator.R
      mod_sampling.R
      mod_optimizer_runner.R
      mod_results_display.R
```

Dependencies for the app: `shiny`, `dplyr`, `readr`, `ggplot2`, `arrow`, `taperNOR`

---

## Questions for user (see project_review_summary.txt "Decisions needed")
