# optBuck 0.3.0

First release of the package (the repository, called optBuck until now, is the package).

* Bucking simulator for harvester files: .hpr stems, NFI trees (sanitized data included) and tree lists, two scenarios of
  assortments and prices compared on the same stems.
* StanForD 2010 import and export of .hpr and .pin files; price tuning to a target length distribution.
* Bucking analysis (tab 5): the harvester's recorded bucking against the best possible bucking under the file's own
  instruction, with a PDF report.
* Exported functions: `run_app()`, `read_hpr()`, `read_hpr_stems()`, `read_hpr_logs()`, `hpr_permitted_grades()`,
  `hpr_scenario()`, `buck_hpr()`, `buck_stem()`, `bucking_check()`, `write_hpr()`, `write_pin()`, `build_nfi_cache()`.
* Code licensed MIT (copyright NIBIO); the NFI tables under CC BY 4.0 ("Landsskogtakseringen, NIBIO"); fonts OFL.
* NFI tree and plot numbers are random permutations; stem profiles are built on first use.
