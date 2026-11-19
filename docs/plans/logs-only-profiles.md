# Stem profiles for logs-only .hpr files: investigation (2026-11-19)

Status: investigation and validation only; nothing is implemented in the app. This reopens the 2026-11-16 decision ("no profiles from log diameters") at Lennart's request. Scripts are scratch work outside the repository (they would become `R/helpers_hpr_profile.R` if this goes ahead).

## 1. What taperNOR 0.1.0 offers
- `taperNOR(h, dbh, h_top, sp, with_bark)`: profile from DBH and total height (spruce, pine, birch).
- `hfromd(d, h, sp, output)`: fits height and DBH from diameters `d` (cm) at heights `h` (m) with `taperNOR`. Needs species and at least two diameters above 0.5 m. Unstable with few points (heights of 180-690 m on 1-2-log stems).
- `kublin_nor(Hx, Hm, Dm, mHt, sp, Dx)`: TapeR calibration on several measured diameters (`Hm`, `Dm`) with the **measured tree height `mHt` as required input**; uses the spruce parameters for every species (known issue, PLAN.md). Needs TapeR (already installed with taperNOR).
- Tried: (a) `hfromd` then `taperNOR`; (b) DBH fixed, height by bounded 1-D fit (`optimize`), `taperNOR`; (c) height as in (b), then `kublin_nor`. (c) is best.

## 2. What a logs-only file gives
- Stems with `DBH` (mm) and per log `LogLength`, `Mid ob`, `Top ob` (all John Deere files; `Butt ob` only on Komatsu), `ProductKey`, `StemGrade`, and a `ReferenceDiameter` with its height (not used).
- Position 0 = the felling cut; log start = cumulative `LogLength` (or `StartPos` if present).
- StanForD 2010 (v3.1 XSD): `DBHHeight` is the "height above stump of DBH" (110 cm Norway, 120 Sweden). The file has **no stump height element**, so height above ground = position + (130 - DBHHeight), as already done for real profiles (10 cm).
- Points per stem: (DBHHeight, DBH), (log middle, Mid ob), (log top, Top ob), plus Butt ob where present. No total height is recorded: it is fitted.
- The predicted profile ends 10 cm above the last log (where the John Deere profiles end). Extending to the tree top makes the existing rule "logs cover less than half of the stem" drop most stems.

## 3. Validation (profiles removed, only logs-only content kept)
14 files with real profiles (3 owner files, 6 Komatsu and 5 John Deere archive files; spruce and pine), 4,458 stems for profile error, about 6,000 stems per bucking-check comparison.

| Measure (`kublin_nor` variant) | Result |
|---|---|
| Diameter error, logged part (all points) | bias +1.1 mm, RMSE 6.7 mm (spruce 6.6, pine 9.7) |
| By relative height | RMSE 4-6 mm in 10-80 %; 12-14 mm in the lowest 10 % (butt swell), 9 mm in the top 10 % |
| Above the last log (extrapolation, not used in bucking) | bias -3.4 mm, RMSE 11.8 mm |
| Stems where prediction failed | 0 (hfromd: 24 %; its RMSE 9.2 mm; fixed-DBH variant: RMSE 9.8 mm) |
| Yield index, predicted vs real (same range) | mean -0.010 (range 0 to -0.019); pooled 0.919 vs 0.928; spruce 0.919 vs 0.927, pine 0.919 vs 0.932 |
| Ranking of the 14 files by yield index | Spearman 0.95, Pearson 0.98 |
| Best possible value | +0.7 % (spruce +0.9 %, pine 0.0 %); 83 % of stems within 5 % |
| Sawlog share, best possible | +0.3 percentage points |
| Recorded value | -0.2 % |

The fixed-DBH variant is clearly worse (yield -0.023, best value +1.9 %, pine -3.8 %). Log diameters come from the same machine profile, so errors at log ends and middles are partly in-sample; between them the test is real. Compared to the full real profile (not cut at the last log) the yield differs by a further 0.005 on average; that is a property of the file, not of the prediction.

## 4. The 23 logs-only files (22 John Deere, 1 Komatsu) and the owner's logs-only file
All 24 work (1,500 stems checked at most per file; sampled for time): yield index 0.86-0.99, median 0.91; 0-376 stems left out per file (largest: 376 of 1,342; the cause was not broken down). About 4 % of the stems get "no best bucking found" (stems too small for any priced product). No failures in reading, DBH or prediction. John Deere files from 2009-2018 (versions below 2.2) give a best-possible sawlog share far above the recorded one in several files (e.g. 0.45 against 0.75); this cannot be verified without real profiles and may come from grades or products, so treat the sawlog numbers for those files with care.

## 5. Recommendation: GO, as an estimate that is labelled
- Accuracy is good enough for the bucking yield index: about 0.01 lower than with a measured profile, and the same ranking of files.
- Use `kublin_nor` (TapeR) with the height from a bounded fit and DBH from the file; add a small `R/` helper and a Suggests entry for TapeR. Estimated effort: 1-2 days with an audit.
- Message in tab 1 and tab 5 (Norwegian first): "Filen har ingen stammeprofiler. Profilene er estimert fra stokkdiametre og BHD (taperNOR/TapeR). Apteringsgraden blir typisk om lag 1 prosentpoeng lavere enn med målte profiler, og er mest egnet til å sammenligne filer. Tabell og figurer for toppen av stammen er usikre." English: "This file has no stem profiles. They are estimated from log diameters and DBH (taperNOR/TapeR). The yield index is typically about 1 percentage point lower than with measured profiles and suits comparison between files; the top of the stem is uncertain."
- Limits to state: spruce parameters are used for pine; no profile above the last log; the stump is assumed 130 cm minus DBHHeight; multi-tree stems and stems without DBH or logs are not used; no ground truth exists for the 24 files, only for files that have profiles; Komatsu-style profiles are 1 m longer than the logged part, which the estimate cannot see.
- Open decision for Lennart: whether to also drop the grade-dependent sawlog numbers for these files.
