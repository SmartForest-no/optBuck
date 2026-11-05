# Licence of the data in this folder

The two tables `nfi_tree_plot_stratum_ready_github.parquet` and `nfi_plots_github.parquet` are a
sanitised subset of the Norwegian National Forest Inventory (Landsskogtakseringen, NIBIO).

**Licence: Creative Commons Attribution 4.0 International (CC BY 4.0)**,
https://creativecommons.org/licenses/by/4.0/

**Attribution:** "Landsskogtakseringen, NIBIO" (Norwegian Institute of Bioeconomy Research).

What the tables contain: synthetic plot numbers (`plot_id_github`), county code (`fylnr`), elevation
(`hoh`, whole metres), inventory season (`sesong`), maturity class (`hogstkl`), site index
(`abonitet`) and, per tree, a random tree number (`treid`, a permutation without any link to the
NFI database), species group, diameter (cm) and height. There are no coordinates, municipality
codes or original plot or tree identifiers. See `sanitization_manifest.txt` and
`scripts/sanitize_nfi_table.R`.

The software in this repository is licensed separately (MIT, see the LICENSE file at the top level).
