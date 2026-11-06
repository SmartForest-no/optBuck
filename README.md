# optBuck

optBuck simulates bucking: for each stem it finds the cutting pattern with the highest value under a set of
products and prices, and it can compare the bucking a harvester recorded with the best possible bucking.
It reads and writes StanForD 2010 files (`.hpr` production files, `.pin` product instructions) and comes
with a Shiny app.

## Install

```r
install.packages("remotes")
# dependencies = TRUE also installs the app stack and the GitHub-only packages in DESCRIPTION's
# Remotes (taperNOR: Norwegian taper and bark models; fieldPlot: tree-list heights)
remotes::install_github("lennartnoordermeer/Apter@optbuck-merge", dependencies = TRUE)
```

Until the repository is transferred to `SmartForest-no/optBuck`, install from the address above;
after the transfer the line becomes `remotes::install_github("SmartForest-no/optBuck", dependencies = TRUE)`.

## Use

```r
library(optBuck)
run_app()                       # the app: five tabs, Norwegian or English
```

The tabs:

1. **Stems**: choose the stems: the built-in sanitized NFI selection, an uploaded tree list, or the measured stems of a harvester `.hpr` file.
2. **Assortment specifications and prices**: two scenarios, A and B, each with its own assortments and price matrices; import from `.hpr` or `.pin`, export to `.pin`.
3. **Simulator**: the best cutting pattern of every stem under A and under B, and the difference.
4. **Price tuning**: adjust the prices of sawlog assortments toward a wanted length distribution.
5. **Bucking analysis**: how the harvester bucked each stem of an `.hpr` file compared with the best possible bucking under the file's own instruction, with a PDF report.

The same work from R:

```r
f <- system.file("extdata", "inputs", "test_fixtures", "hpr_check_synthetic.hpr", package = "optBuck")
x <- bucking_check(f)                 # recorded against best possible bucking (invented example stems)
x$key_figures
r <- buck_hpr(f)                      # the best bucking of the measured stems under the file's instruction
```

Other functions: `read_hpr()`, `read_hpr_stems()`, `read_hpr_logs()`, `hpr_permitted_grades()`, `hpr_scenario()`,
`buck_stem()`, `write_hpr()`, `write_pin()`, `build_nfi_cache()`. The NFI stem profiles are built on first use
of the built-in NFI source (about 3 minutes, once).

## Data and licences

* Code: MIT, copyright NIBIO (see `LICENSE`).
* The sanitized NFI tables in `inst/extdata/nfi`: CC BY 4.0, attribution "Landsskogtakseringen, NIBIO"
  (`inst/extdata/nfi/LICENSE-data.md`). Tree and plot numbers are random; there are no coordinates.
* Fonts of the app: Inter, SIL Open Font License.
* The test files are invented; no real harvester data is part of the package.

## Citation

See `citation("optBuck")` and `CITATION.cff`.
