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
