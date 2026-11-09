# ==============================================================================
# scripts/diag_loss.R  (roadmap 8, task 1, 2026-11-04)
#
# Purpose : Decompose the value loss (best possible minus recorded) of the bucking check
#           of one of the owner's .hpr files, from the saved bucking_check() results
#           (scripts/diag_save_check.R -> C:/optbuck_private/diag/<file>_rz.rds / _norz.rds).
#           Parts a-i of the task. Output: C:/optbuck_private/diag/<file>_loss.txt (private:
#           stem keys) and <file>_loss_table.csv (aggregates only).
# Usage   : Rscript scripts/diag_loss.R file5 [file1 file4]
# ==============================================================================
root_dir <- local({
  is_root <- function(d) file.exists(file.path(d, "R", "helpers_paths.R"))
  d <- normalizePath(Sys.getenv("OPTBUCK_ROOT", unset = getwd()), winslash = "/", mustWork = FALSE)
  while (!is_root(d) && dirname(d) != d) d <- dirname(d)
  d
})
Sys.setenv(OPTBUCK_ROOT = root_dir)
suppressMessages(pkgload::load_all(root_dir, quiet = TRUE))   # the package (R/, inst/)

dirp <- "C:/optbuck_private/hpr"; dg <- "C:/optbuck_private/diag"
priv_hpr <- function(id) { m <- utils::read.csv("C:/optbuck_private/hpr_files.csv", stringsAsFactors = FALSE, encoding = "UTF-8"); file.path(dirp, m$file[match(id, m$id)]) }   # the real names are private (not in the repository)
cfg <- list(file1 = priv_hpr("file1"), file3 = priv_hpr("file3"),
            file4 = priv_hpr("file4"), file5 = priv_hpr("file5"))
args <- commandArgs(trailingOnly = TRUE)
tag <- Sys.getenv("DIAG_TAG", "rz")      # which saved check: rz (default), used (best possible with the used products only)

# The harvester's own valuation of a recorded log: m3 (price) x the price of the file's cell at the
# file's mm diameter class (recorded top diameter as the product says: under or over bark) and the
# file's length class. NA: no cell (outside the matrix); "criteria" tells forbidden / manual only.
harvester_value <- function(rec, hprp) {
  n <- nrow(rec); price <- rep(NA_real_, n); crit <- rep(NA_character_, n); found <- rep(FALSE, n)
  for (pk in unique(rec$ProductKey[!rec$waste])) {
    p <- hprp[[pk]]; ii <- which(rec$ProductKey == pk & !rec$waste); if (is.null(p) || !length(ii)) next
    ub <- !identical(tolower(p$dia_under_bark), "false")
    top <- if (ub) rec$TopUb[ii] else rec$TopOb[ii]
    cells <- p$cells[!is.na(p$cells$d_mm) & !is.na(p$cells$l_cm), , drop = FALSE]
    D <- sort(unique(cells$d_mm)); L <- sort(unique(cells$l_cm))
    dc <- vapply(top, function(t) { u <- D[D <= t]; if (is.na(t) || !length(u)) NA_real_ else max(u) }, 0)
    lc <- vapply(rec$LogLength[ii], function(l) { u <- L[L <= l]; if (!length(u)) NA_real_ else max(u) }, 0)
    mi <- match(paste(dc, lc), paste(cells$d_mm, cells$l_cm))
    price[ii] <- cells$price[mi]; crit[ii] <- cells$criteria[mi]; found[ii] <- !is.na(mi)
  }
  data.frame(h_price = price, h_crit = crit, h_cell = found, h_value = ifelse(is.na(price), 0, price) * ifelse(is.na(rec$m3_price), 0, rec$m3_price))
}

pc <- function(a, b) sprintf("%.1f %%", 100 * a / b)
kr <- function(x) format(round(x), big.mark = " ")
for (nm in intersect(names(cfg), args)) {
  out <- character(0); say <- function(...) out <<- c(out, sprintf(...))
  tab <- list(); add <- function(part, what, n, kr_val, share_loss, note = "") tab[[length(tab) + 1L]] <<- data.frame(file = nm, part = part, what = what, n = n, kr = kr_val, share_of_loss = share_loss, note = note, stringsAsFactors = FALSE)
  x <- readRDS(file.path(dg, sprintf("%s_%s.rds", nm, tag)))
  xn <- if (file.exists(file.path(dg, sprintf("%s_norz.rds", nm)))) readRDS(file.path(dg, sprintf("%s_norz.rds", nm))) else NULL
  h <- read_hpr(cfg[[nm]], basename(cfg[[nm]]))
  st <- x$stems; rec <- x$logs$recorded; opt <- x$logs$optimal; defs <- x$defs
  u <- st$use & st$value_opt > 0
  V_rec <- sum(st$value_rec[u]); V_opt <- sum(st$value_opt[u]); Lo <- V_opt - V_rec
  say("=== %s: %d stems used; recorded %s, best possible %s, loss %s kr, yield index %s", nm, sum(u), kr(V_rec), kr(V_opt), kr(Lo), pc(V_rec, V_opt))
  add("0", "total", sum(u), Lo, 1, sprintf("yield %s", pc(V_rec, V_opt)))
  ru <- rec[rec$stem_key %in% st$StemKey[u], , drop = FALSE]
  hv <- harvester_value(ru, h$products); ru <- cbind(ru, hv)

  # a. recorded logs valued 0, by reason
  nw <- !ru$waste
  zero <- nw & ru$value_nok == 0
  reason <- rep(NA_character_, nrow(ru))
  reason[zero & ru$unknown_product] <- "product not in the definitions"
  reason[zero & is.na(reason) & is.na(ru$class_dm)] <- "length outside the product's length classes"
  nopr <- zero & is.na(reason)
  reason[nopr & ru$h_crit %in% "Forbidden to buck the log even manually"] <- "forbidden cell"
  reason[nopr & is.na(reason) & ru$h_crit %in% "Only logs bucked manually allowed"] <- "manual-only cell"
  reason[nopr & is.na(reason) & !ru$h_cell] <- "outside the matrix (diameter or length)"
  reason[nopr & is.na(reason)] <- "no price in optBuck's grid (other)"
  say("a. Recorded logs valued 0 (not waste): %d of %d non-waste logs (%s of their harvester price volume)", sum(zero), sum(nw), pc(sum(ru$m3_price[zero], na.rm = TRUE), sum(ru$m3_price[nw], na.rm = TRUE)))
  for (r in sort(unique(reason[!is.na(reason)]))) {
    k <- which(reason == r); pot <- sum(ru$h_value[k])
    say("   %-45s %6d logs, %8.1f m3 (price), harvester-rule value if priced %s kr (%s of the loss)", r, length(k), sum(ru$m3_price[k], na.rm = TRUE), kr(pot), pc(pot, Lo))
    add("a", r, length(k), pot, pot / Lo, "value the logs would have at the file's own price")
  }
  w <- ru$waste
  say("   recorded waste (unclassified product): %d logs, %.1f m3sub (value 0 by definition)", sum(w), sum(ru$m3sub[w], na.rm = TRUE))
  add("a", "recorded waste logs (unclassified product)", sum(w), NA_real_, NA_real_, sprintf("%.1f m3sub", sum(ru$m3sub[w], na.rm = TRUE)))

  # b. recorded logs outside the product limits
  out_l <- nw & ru$outside
  stems_out <- unique(ru$stem_key[out_l]); su <- st[st$StemKey %in% stems_out & u, ]
  say("b. Logs outside the product limits: %d logs in %d stems; their stems lose %s kr (%s of the loss); yield index of the other stems %s", sum(out_l), nrow(su),
      kr(sum(su$value_opt - su$value_rec)), pc(sum(su$value_opt - su$value_rec), Lo),
      pc(sum(st$value_rec[u & !st$StemKey %in% stems_out]), sum(st$value_opt[u & !st$StemKey %in% stems_out])))
  add("b", "stems with a log outside the limits", nrow(su), sum(su$value_opt - su$value_rec), sum(su$value_opt - su$value_rec) / Lo, sprintf("%d logs", sum(out_l)))

  # c. possible broken top
  bt <- u & st$possible_broken_top
  say("c. Possible broken top: %d stems (%s); loss %s kr (%s of the loss). Yield index with them %s, without them %s", sum(bt), pc(sum(bt), sum(u)),
      kr(sum(st$value_opt[bt] - st$value_rec[bt])), pc(sum(st$value_opt[bt] - st$value_rec[bt]), Lo), pc(V_rec, V_opt),
      pc(sum(st$value_rec[u & !bt]), sum(st$value_opt[u & !bt])))
  add("c", "possible broken top", sum(bt), sum(st$value_opt[bt] - st$value_rec[bt]), sum(st$value_opt[bt] - st$value_rec[bt]) / Lo,
      sprintf("yield without them %s", pc(sum(st$value_rec[u & !bt]), sum(st$value_opt[u & !bt]))))

  # d. manual cuts
  mm <- u & st$has_manual
  say("d. Stems with manual cuts: %d (%s); loss %s kr (%s of the loss); yield index with %s, without %s", sum(mm), pc(sum(mm), sum(u)),
      kr(sum(st$value_opt[mm] - st$value_rec[mm])), pc(sum(st$value_opt[mm] - st$value_rec[mm]), Lo),
      pc(sum(st$value_rec[mm]), sum(st$value_opt[mm])), pc(sum(st$value_rec[u & !st$has_manual]), sum(st$value_opt[u & !st$has_manual])))
  add("d", "stems with manual cuts", sum(mm), sum(st$value_opt[mm] - st$value_rec[mm]), sum(st$value_opt[mm] - st$value_rec[mm]) / Lo,
      sprintf("yield with %s, without %s", pc(sum(st$value_rec[mm]), sum(st$value_opt[mm])), pc(sum(st$value_rec[u & !st$has_manual]), sum(st$value_opt[u & !st$has_manual]))))

  # e. DistributionAllowed products
  akeys <- assortment_table_keys(defs); dist_ok <- setNames(defs$DistributionAllowed %in% TRUE, akeys)
  opt_u <- opt[as.character(opt$treid) %in% st$StemKey[u], , drop = FALSE]
  has_dist_opt <- unique(as.character(opt_u$treid[dist_ok[as.character(opt_u$ProductKey)] %in% TRUE]))
  has_dist_rec <- unique(ru$stem_key[!is.na(ru$akey) & dist_ok[ru$akey] %in% TRUE])
  dd <- u & st$StemKey %in% has_dist_opt
  say("e. Stems whose best bucking uses a DistributionAllowed product: %d (%s); loss %s kr (%s); yield index %s; other stems %s", sum(dd), pc(sum(dd), sum(u)),
      kr(sum(st$value_opt[dd] - st$value_rec[dd])), pc(sum(st$value_opt[dd] - st$value_rec[dd]), Lo), pc(sum(st$value_rec[dd]), sum(st$value_opt[dd])),
      pc(sum(st$value_rec[u & !dd]), sum(st$value_opt[u & !dd])))
  add("e", "stems whose best bucking uses a DistributionAllowed product", sum(dd), sum(st$value_opt[dd] - st$value_rec[dd]), sum(st$value_opt[dd] - st$value_rec[dd]) / Lo,
      sprintf("yield %s; others %s", pc(sum(st$value_rec[dd]), sum(st$value_opt[dd])), pc(sum(st$value_rec[u & !dd]), sum(st$value_opt[u & !dd]))))
  pv <- function(d, ok) { k <- names(dist_ok)[dist_ok == ok]; sum(d$value[d$key %in% k]) }
  ar <- x$assortments$recorded; ao <- x$assortments$optimal
  say("   per product value (recorded / best possible), DistributionAllowed TRUE: %s / %s kr; others: %s / %s kr", kr(pv(ar, TRUE)), kr(pv(ao, TRUE)), kr(pv(ar, FALSE)), kr(pv(ao, FALSE)))
  add("e", "products with DistributionAllowed TRUE: recorded vs best possible value", NA_integer_, pv(ao, TRUE) - pv(ar, TRUE), (pv(ao, TRUE) - pv(ar, TRUE)) / Lo, sprintf("recorded %s, best %s", kr(pv(ar, TRUE)), kr(pv(ao, TRUE))))
  add("e", "other products: recorded vs best possible value", NA_integer_, pv(ao, FALSE) - pv(ar, FALSE), (pv(ao, FALSE) - pv(ar, FALSE)) / Lo, sprintf("recorded %s, best %s", kr(pv(ar, FALSE)), kr(pv(ao, FALSE))))

  # f. waste-zone rule on vs off
  if (!is.null(xn)) {
    sn <- xn$stems; un <- sn$use & sn$value_opt > 0
    say("f. Waste-zone rule ON (used for the published figure): yield %s (best %s kr); OFF: yield %s (best %s kr, recorded %s kr)", pc(V_rec, V_opt), kr(V_opt),
        pc(sum(sn$value_rec[un]), sum(sn$value_opt[un])), kr(sum(sn$value_opt[un])), kr(sum(sn$value_rec[un])))
    add("f", "waste-zone rule off: best possible increases by", sum(un), sum(sn$value_opt[un]) - V_opt, NA_real_, sprintf("yield off %s, on %s", pc(sum(sn$value_rec[un]), sum(sn$value_opt[un])), pc(V_rec, V_opt)))
  }

  # g. harvester-rule recorded value
  Vg <- sum(ru$h_value); Vo <- sum(ru$value_nok)
  say("g. Harvester-rule recorded value (m3 (price) x the file's price at its own mm class): %s kr vs our recorded value %s kr (%s); yield index with it %s (ours %s)",
      kr(Vg), kr(Vo), pc(Vg, Vo), pc(Vg, V_opt), pc(V_rec, V_opt))
  both <- nw & ru$value_nok > 0 & ru$h_value > 0
  say("   logs with both values: %d; median ratio ours/harvester %.3f; share within 1 %%: %s; harvester value 0 where ours > 0: %d logs; ours 0 where harvester > 0: %d logs",
      sum(both), stats::median(ru$value_nok[both] / ru$h_value[both]), pc(sum(abs(ru$value_nok[both] / ru$h_value[both] - 1) < 0.01), sum(both)),
      sum(nw & ru$h_value == 0 & ru$value_nok > 0), sum(nw & ru$value_nok == 0 & ru$h_value > 0))
  add("g", "harvester-rule recorded value instead of ours", NA_integer_, Vg - Vo, (Vg - Vo) / Lo, sprintf("yield %s instead of %s", pc(Vg, V_opt), pc(V_rec, V_opt)))

  # h. definitions per ProductKey, modification dates
  px <- .hpr_prefix(cfg[[nm]]); doc <- tryCatch(xml2::read_xml(.hpr_close_tags(px$bytes)), error = function(e) NULL)
  if (!is.null(doc)) {
    xml2::xml_ns_strip(doc)
    pd <- xml2::xml_find_all(doc, "//*[local-name()='ProductDefinition']")
    keys <- vapply(pd, function(n) { k <- xml2::xml_find_first(n, "./ProductKey"); if (inherits(k, "xml_missing")) NA_character_ else xml2::xml_text(k) }, "")
    md <- vapply(pd, function(n) { k <- xml2::xml_find_first(n, ".//ModificationDate"); if (inherits(k, "xml_missing")) NA_character_ else xml2::xml_text(k) }, "")
    dup <- keys[duplicated(keys) & !is.na(keys)]
    say("h. ProductDefinition elements: %d, distinct ProductKey: %d, keys with more than one definition: %d; distinct ModificationDate values: %d (%s)", length(pd), length(unique(keys)), length(unique(dup)),
        length(unique(md[!is.na(md)])), paste(sort(unique(substr(md[!is.na(md)], 1, 10))), collapse = ", "))
    add("h", "keys with more than one definition", length(unique(dup)), NA_real_, NA_real_, sprintf("%d definitions, %d distinct modification dates", length(pd), length(unique(md[!is.na(md)]))))
  } else say("h. could not read the definitions part")

  # i. the 20 stems with the largest loss
  st$loss <- st$value_opt - st$value_rec
  top <- st[u, , drop = FALSE]; top <- top[order(-top$loss), ][seq_len(min(20L, sum(u))), ]
  say("i. The 20 stems with the largest loss (sum %s kr = %s of the loss)", kr(sum(top$loss)), pc(sum(top$loss), Lo))
  pcode <- function(pk, akey_map) pk
  for (i in seq_len(nrow(top))) {
    k <- top$StemKey[i]; r <- ru[ru$stem_key == k, , drop = FALSE]; o <- opt[as.character(opt$treid) == k, , drop = FALSE]
    rs <- paste(sprintf("%s%s:%d/%.0f", ifelse(r$waste, "W", r$ProductKey), ifelse(r$manual %in% TRUE, "m", ""), as.integer(r$LogLength), ifelse(is.na(r$TopUb_calc), 0, r$TopUb_calc / 10)), collapse = " ")
    o <- o[order(o$StartPos), ]
    os <- paste(sprintf("%s:%d", o$ProductKey, as.integer(o$LogLength)), collapse = " ")
    why <- c(if (top$possible_broken_top[i]) "broken top", if (top$has_manual[i]) "manual cuts", if (top$outside[i]) "log outside limits",
             if (any(ru$stem_key == k & nw & ru$value_nok == 0)) "unvalued log", if (any(ru$stem_key == k & ru$waste)) "waste cut by harvester")
    say("   %s %s loss %.0f (rec %.0f / best %.0f) | recorded: %s | best: %s | %s", k, top$species_group[i], top$loss[i], top$value_rec[i], top$value_opt[i], rs, os,
        if (length(why)) paste(why, collapse = ", ") else "distribution/other")
  }
  # j. products in the file's instruction that no recorded log uses (special orders, not in use)
  used_keys <- unique(ru$akey[!is.na(ru$akey)])
  nu <- !names(dist_ok) %in% used_keys & !defs$Type %in% "waste"
  unused_keys <- akeys[nu & !defs$Type %in% "waste"]
  ou <- opt_u[as.character(opt_u$ProductKey) %in% unused_keys, , drop = FALSE]
  st_unused <- unique(as.character(ou$treid))
  say("j. Products with no recorded log: %d of %d (%s). The best possible bucking uses them in %d stems (%s of the stems) for %s kr (%s of the best possible value)",
      length(unused_keys), sum(!defs$Type %in% "waste"), paste(sub("^[a-z]+_", "", unused_keys), collapse = ", "), length(st_unused), pc(length(st_unused), sum(u)), kr(sum(ou$value_nok)), pc(sum(ou$value_nok), V_opt))
  fu <- file.path(dg, sprintf("%s_used.rds", nm))
  if (file.exists(fu)) { xu <- readRDS(fu); su <- xu$stems; uu <- su$use & su$value_opt > 0
    say("   Yield index with the best possible bucking restricted to the products the harvester used: %s (recorded %s, best possible %s kr); loss %s kr", pc(sum(su$value_rec[uu]), sum(su$value_opt[uu])), kr(sum(su$value_rec[uu])), kr(sum(su$value_opt[uu])), kr(sum(su$value_opt[uu]) - sum(su$value_rec[uu])))
    add("j", "restricted to the products the harvester used", sum(uu), sum(su$value_opt[uu]) - sum(su$value_rec[uu]), (sum(su$value_opt[uu]) - sum(su$value_rec[uu])) / Lo, sprintf("yield %s instead of %s", pc(sum(su$value_rec[uu]), sum(su$value_opt[uu])), pc(V_rec, V_opt)))
    # the remaining loss in the restricted run: stems with manual cuts, distribution products, broken tops
    Lu <- sum(su$value_opt[uu]) - sum(su$value_rec[uu]); mu <- uu & su$has_manual; bu <- uu & su$possible_broken_top
    say("   restricted run: loss in stems with manual cuts %s kr (%s of its loss); in stems with a possible broken top %s kr; yield with manual cuts %s, without %s", kr(sum(su$value_opt[mu] - su$value_rec[mu])), pc(sum(su$value_opt[mu] - su$value_rec[mu]), Lu),
        kr(sum(su$value_opt[bu] - su$value_rec[bu])), pc(sum(su$value_rec[mu]), sum(su$value_opt[mu])), pc(sum(su$value_rec[uu & !su$has_manual]), sum(su$value_opt[uu & !su$has_manual])))
    xa <- xu$assortments; dk <- setNames(xu$defs$DistributionAllowed %in% TRUE, assortment_table_keys(xu$defs))
    pv2 <- function(d, ok) sum(d$value[d$key %in% names(dk)[dk == ok]])
    say("   restricted run, per product value (recorded / best possible): DistributionAllowed TRUE %s / %s kr; others %s / %s kr", kr(pv2(xa$recorded, TRUE)), kr(pv2(xa$optimal, TRUE)), kr(pv2(xa$recorded, FALSE)), kr(pv2(xa$optimal, FALSE)))
  }
  add("j", "products with no recorded log used by the best possible bucking: value", length(st_unused), sum(ou$value_nok), sum(ou$value_nok) / Lo, paste(unused_keys, collapse = ";"))
  writeLines(out, file.path(dg, sprintf("%s_loss%s.txt", nm, if (tag == "rz") "" else paste0("_", tag))))
  write.csv(do.call(rbind, tab), file.path(dg, sprintf("%s_loss_table%s.csv", nm, if (tag == "rz") "" else paste0("_", tag))), row.names = FALSE)
  cat(out, sep = "\n")
}
