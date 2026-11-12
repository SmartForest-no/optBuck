# optBuck 0.3.5

* **Skogforsk 2004 bark function for spruce in .hpr files:** a species group with `Skogforsk 2004, Norway spruce` now uses the published function
  (Hannrup, B. 2004. Funktioner for skattning av barkens tjocklek hos tall och gran vid avverkning med skordare. Skogforsk Arbetsrapport 575:
  double bark = 0.46146 + 0.01386 dbh + 0.03571 d, all in mm, at least 2 mm). It reproduces the recorded under-bark log diameters of two
  files to within rounding (sd 0.29 mm). The pine function (`Skogforsk 2004, Scots pine`) did **not** reproduce them (residual sd 0.8-1.3 mm,
  up to 10 mm), so it is not implemented: pine stays on barkNOR and tab 1 says so once. Effect on file3 / file4 (spruce-dominated):
  price volume of all products 0.976 -> 0.986 and 0.990 -> 0.999 of the harvester's `m3 (price)`; the `Top` products are pine and unchanged.

# optBuck 0.3.4

* **Bark model for .hpr stems:** the under-bark diameters of .hpr stems follow the harvester's own bark function from the file
  (`Swedish Zacco`: double bark = A + B x over-bark diameter; `None`) instead of barkNOR. The function reproduces the under-bark log
  diameters recorded in the files to within 0.3 mm. A file with another function (for example `Skogforsk 2004`) or none falls back
  to barkNOR for those stems, and tab 1 says so once. NFI stems and uploaded tree lists keep barkNOR. Help has a new term, «Barkmodell».
  Effect on a Zacco file (file5): `Top` price volume 0.922 -> 1.002 of the harvester's `m3 (price)`, yield index 93.1 -> 93.2 %.

# optBuck 0.3.3

Fixes after testing 0.3.2.

* **Flat price keeps the product structure.** «Fast pris» on an assortment with a price matrix puts the same price in every priced
  cell of the existing matrix; length classes, diameter classes, empty cells and the demand matrix stay (the .pin export used to get
  length classes every 10 cm and an all-zero demand matrix). A run with B = A with only the price type changed differs only through the price.
* **Products without a demand matrix:** the .pin and .hpr exports write Distribution 0 and DistributionAllowed false (never true with an
  all-zero matrix; one warning if a product had it). Tab 2 shows «Ingen fordelingsmatrise» / "No demand matrix" instead of a table of zeros,
  and the scenario xlsx has no `target_<n>` sheet for such products. Existing demand matrices round-trip through xlsx and .pin.
* **Price volume rounding follows each product's `VolumeDiameterAdjustment`** ("Measured diameter in mm" = not rounded, "... rounded
  downwards to cm" = whole cm), read from the .hpr/.pin, kept in the scenario xlsx and written on export. Calculated Norwegian mid always
  floors and solid volume never rounds. Results of products the file prices on unrounded mm change (price volume 6-8 % higher on `Top`).
* **Downloads:** «Last ned som .hpr» is in tab 2 next to xlsx and .pin (it bucks the stems of tab 1 with the scenario and copies them with
  the simulated logs; greyed out without .hpr stems in tab 1). The .hpr button in tab 3 is gone. The .hpr export no longer scans its own
  output for coordinates (the known coordinate blocks are still removed).
* **Price tuning, matrices:** two new views, «Opprinnelige priser» and «Tilpassede priser» (kr/m³); diameter classes that were not tuned
  (too few logs, no demand matrix) are greyed with a note.
* **Tab 3:** «A og B er like. Endre scenario B for å sammenligne.» when the scenarios are identical.
* **Keys:** æ, ø, å are transliterated (ae, o, a) in assortment keys and sheet names (`spruce_140_sagtommer_gran`, was `sagt_mmer`). Scenario
  files and price uploads with the old keys still load.
* The cleaning thresholds for .hpr stems are described as optBuck's own defaults.

# optBuck 0.3.2

Fixes after testing 0.3.1.

* **Price tuning:** an assortment whose prices differ between the whole-cm rows of one file diameter class (for example
  after editing one cell in tab 2) is listed as not tunable, with the reason «Prisene varierer innenfor en diameterklasse.
  Pristilpasning krever én pris per diameterklasse og lengde.» / "Prices vary within a diameter class. Price tuning needs
  one price per diameter class and length." It used to stop with "value check failed on iteration 1".
* **Tab 2:** the «Change prices» row under the matrix (± NOK/m³, ± %, fill from row prices) is removed; the button
  «Remove» is «Remove assortment» / «Fjern sortiment».
* **Assortment source:** an .hpr loaded in tab 1 gives scenarios A and B its assortments and rules automatically (the
  products the harvester used, at least 1 % of a species' recorded logs), without a dialog. Each scenario starts with
  «Sortimenter fra: <fil> (fane 1)» / «Standardsortimenter» and a button «Bytt kilde» (dialog: the same .hpr as in tab 1 with
  the product selection, an uploaded file, or the default assortments). The upload field and the button «Bruk samme .hpr som
  i fane 1» at the bottom of the scenario, and the buttons «Bruk sortimentene fra denne filen i scenario A/B» in tab 1, are
  removed. Tab 1 no longer using .hpr puts the scenarios that used it back to the default assortments.

# optBuck 0.3.1

* **Reader:** files that declare UTF-8 but have windows-1252 bytes in the header are read (non-UTF-8 lines are converted),
  and the literal text `<U+FEFF>` that some John Deere TimbermaticH files start with is ignored. Valid UTF-8 and real
  byte order marks are untouched; the copy of the header in an exported .hpr is repaired the same way.
* **Bucking analysis:** a product counts as *used* (the best possible bucking may use only used products) when it has at
  least 1 % of a species' recorded logs, not just one log. The recorded logs of the other products are still valued; only
  the optimiser leaves them out. Documented in the help and in the PDF report.
* **Price volume follows the file's own volume method per product.** The file's `Top` is the cylinder at the top diameter
  (it was imported as the frustum); `Calculated Norwegian mid`, `Mid` and `All diameters` keep their rules. A method that is
  not known gets the frustum (`Top`, 1 cm/m taper) and one warning per method. All .hpr results change: the simulator, the
  price tuning and the bucking analysis value recorded and best possible logs alike.
* **Install:** the app stack (shiny, bslib, reactable, ggplot2, ...) and taperNOR and fieldPlot are imports now, so a plain
  install gives a working `run_app()`. Only the audit-only packages stay in Suggests.
* **App:** tab 1 intro «Last inn eller velg stammedata.»; tab 2 has «Bruk samme .hpr som i fane 1» next to the file upload
  (greyed out when tab 1 does not use .hpr stems); max butt diameter, volume method and log position are shown in the edit
  dialog only; prices without ",00"; the type dropdown is wide enough; the empty «Stammer per treslag» panel shows a hint.
* **Terminology:** *Fordelingsmatrise* (Norwegian) and *demand matrix* (English) replace «ønsket fordeling» and "target
  distribution" in the app, help, report, README and messages.
* **Quality warnings fixed:** they said "Kvalitet X dekker Y % ... Stammene og sortimentene kommer trolig fra ulike oppdrag"
  for ordinary files, because the check asked each sawlog product to permit half of the whole stem length; a file with much
  low-quality length (taken by pulpwood products) or special-order products tripped it. The check now compares the stems'
  grade codes with the codes the species' assortments permit: a species is reported only when at least 10 % of its length
  has grades that no assortment permits (a different job's codes). One short line per species in tab 2 with the details
  behind a click; tabs 3 and 4 no longer show these warnings.
* **Price tuning (tab 4):** "Prisene ble tilpasset på et utvalg på N stammer. Tallene under gjelder alle M stammene."; the
  tuned assortments are listed first, the others in one line with the reason, and left out of the detailed results.
* **Audits:** 36 check 14 passed a folder where `nfi_profiles_ready()` takes the root (not a taperNOR problem); audit 15
  needs the prototype's own output files, which are not in the repository, and reports REVIEW where they are absent.

# optBuck (development version)

* New example file `system.file("extdata", "example.hpr", package = "optBuck")`: 376 whole stems and their recorded logs
  (measurements unchanged) from a real harvester file, with seven products, prices multiplied by 1.15 and rounded to the
  nearest 5 NOK, and every name, id, date (shifted, year kept) and coordinate removed or replaced. It is a valid StanForD 2010
  `.hpr` (v3.1) and works in all five tabs; `scripts/make_example_hpr.R` shows how it was made.

# optBuck 0.3.0

First release of the package (the repository, called optBuck until now, is the package).

* Bucking simulator for harvester files: .hpr stems, NFI trees (sanitized data included) and tree lists, two scenarios of
  assortments and prices compared on the same stems.
* StanForD 2010 import and export of .hpr and .pin files; price tuning to a demand matrix (the sawmill's wanted length distribution).
* Bucking analysis (tab 5): the harvester's recorded bucking against the best possible bucking under the file's own
  instruction, with a PDF report.
* Exported functions: `run_app()`, `read_hpr()`, `read_hpr_stems()`, `read_hpr_logs()`, `hpr_permitted_grades()`,
  `hpr_scenario()`, `buck_hpr()`, `buck_stem()`, `bucking_check()`, `write_hpr()`, `write_pin()`, `build_nfi_cache()`.
* Code licensed MIT (copyright NIBIO); the NFI tables under CC BY 4.0 ("Landsskogtakseringen, NIBIO"); fonts OFL.
* NFI tree and plot numbers are random permutations; stem profiles are built on first use.
