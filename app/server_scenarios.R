# app/server_scenarios.R
#
# Purpose : The "Assortments and prices" tab (Phase 4d): Scenario A and
#           Scenario B, each with its own assortment table and price matrices.
#           Called from server() in app_server.R; returns the reactives the
#           runs need.
#
# State   : A: defs_a() (assortment table) and pm_a() (prices).
#           B is linked to A per assortment (R/helpers_price_matrix.R):
#           b_asm()[key] is "same" (follows A's row), "own" (B's own row, in
#           b_own()) or "none" (not in B); keys only in B are "own".
#           b_state()[key] is B's price state: "same" (A's prices), "own"
#           (pm_b()) or "none" (not in B). Invariant: b_asm "none" <=>
#           b_state "none"; a key only in B has own prices.
#           Internal names: "default" and "scenario" in the backend mean A and B.

scenario_server <- function(input, output, session) {
  keys_of <- assortment_table_keys
  st0 <- setNames(rep("same", nrow(APP_DEFS)), keys_of(APP_DEFS))

  defs_a  <- reactiveVal(APP_DEFS)
  pm_a    <- reactiveVal(APP_PM_DEFAULT)
  pm_b    <- reactiveVal(APP_PM_DEFAULT)          # B's own prices
  b_state <- reactiveVal(st0)                     # B's price states
  b_asm   <- reactiveVal(st0)                     # B's assortment states
  b_own   <- reactiveVal(APP_DEFS[0, , drop = FALSE])
  pm_tick <- reactiveVal(0L)                      # re-render the grids
  msg_a   <- reactiveVal(NULL)                    # last message per column
  msg_b   <- reactiveVal(NULL)
  redraw  <- function() pm_tick(isolate(pm_tick()) + 1L)
  say     <- function(side, type, text, warnings = NULL)
    (if (side == "A") msg_a else msg_b)(list(type = type, text = text, warnings = warnings))

  defs_b   <- reactive(b_defs(defs_a(), b_own(), b_asm()))
  b_now    <- reactive(b_set(defs_a(), pm_a(), pm_b(), b_state(), b_own(), b_asm()))
  b_differ <- reactive(b_differs(pm_a(), pm_b(), b_state(), defs_a(), defs_b()))
  # Everything that changes a run; a run stores it to detect stale results.
  pm_state <- reactive(list(defs_a(), pm_a(), pm_b(), b_state(), b_asm(), b_own()))

  # Set B's state in one go (all four values stay consistent).
  set_b <- function(asm, st, own, pmb = isolate(pm_b())) {
    keep <- names(asm)
    own <- own[keys_of(own) %in% keep[asm == "own"], , drop = FALSE]
    rownames(own) <- NULL
    st <- setNames(st[keep], keep)
    st[is.na(st)] <- ifelse(asm[is.na(st)] == "none", "none", "same")
    b_asm(asm); b_state(st); b_own(own); pm_b(pm_minimal(pmb))
  }

  # "Spruce sawlog" etc. for keys of either scenario.
  all_defs <- function() {
    a <- isolate(defs_a()); b <- isolate(defs_b())
    rbind(a, b[!keys_of(b) %in% keys_of(a), , drop = FALSE])
  }
  key_label <- function(keys, d = all_defs()) {
    k <- keys_of(d); i <- match(keys, k)
    ifelse(is.na(i), keys, sprintf("%s %s", tools::toTitleCase(d$Species[i]), tolower(d$Name[i])))
  }
  swap_prices <- function(pm, keys, new = NULL)
    pm_minimal(rbind(pm_minimal(pm)[!pm_minimal(pm)$ProductKey %in% keys, , drop = FALSE], new))


  # ---- assortment tables ----------------------------------------------------
  dash <- function(x) ifelse(is.finite(x), format(x, trim = TRUE), "–")
  rng  <- function(lo, hi) ifelse(is.na(lo), "–",
                                  ifelse(is.finite(hi), sprintf("%s–%s", dash(lo), dash(hi)),
                                         sprintf("≥ %s", dash(lo))))
  asm_display <- function(d, pm) {
    p <- products_from_defs(d, pm)$pd
    data.frame(
      Assortment = d$Name, Species = tools::toTitleCase(d$Species),
      `Top Ø (cm)` = rng(d$MinTopDiameter_cm, d$MaxTopDiameter_cm),
      `Max butt Ø (cm)` = dash(d$MaxButtDiameter_cm),
      `Length (cm)` = rng(p$MinLength, p$MaxLength),
      `Volume method` = unname(VOLUME_METHOD_SHORT[d$VolumeDiameterCategory]),
      check.names = FALSE, stringsAsFactors = FALSE)
  }
  HIDDEN_ASM_COLS <- c("MinButtDiameter_cm", "DiameterTopPosition_cm", "PriceType")

  output$tbl_asm_a <- renderReactable(
    apter_reactable(asm_display(defs_a(), pm_a()), page_size = 20L))

  # B: rows equal to A greyed, differing cells highlighted (differences in
  # fields only in the form mark the name), B-only rows highlighted, A's
  # assortments not in B as a muted "Not used" line.
  output$tbl_asm_b <- renderReactable({
    da <- defs_a(); db <- defs_b(); bn <- b_now()
    disp_b <- asm_display(db, bn$pm); disp_a <- asm_display(da, pm_a())
    ka <- keys_of(da); kb <- keys_of(db)
    rd <- b_row_diff(da, db)
    cell <- matrix(FALSE, nrow(disp_b), ncol(disp_b), dimnames = list(NULL, names(disp_b)))
    for (i in seq_len(nrow(disp_b))) {
      j <- match(kb[i], ka)
      if (is.na(j)) next
      cell[i, ] <- unlist(disp_b[i, ]) != unlist(disp_a[j, ])
      if (any(rd$diff[i, HIDDEN_ASM_COLS])) cell[i, "Assortment"] <- TRUE
    }
    cls <- ifelse(is.na(match(kb, ka)), "asm-bonly", ifelse(rowSums(cell) > 0, "asm-changed", "asm-same"))
    gone <- match(rd$a_only, ka)
    if (length(gone)) {
      extra <- disp_a[gone, , drop = FALSE]
      extra[["Volume method"]] <- "Not used"
      disp_b <- rbind(disp_b, extra)
      cell <- rbind(cell, matrix(FALSE, length(gone), ncol(cell)))
      cls <- c(cls, rep("asm-none", length(gone)))
    }
    rownames(disp_b) <- NULL
    cols <- setNames(lapply(names(disp_b), function(n) reactable::colDef(
      class = function(value, index) if (isTRUE(cell[index, n])) "asm-diff")), names(disp_b))
    apter_reactable(disp_b, page_size = 20L, columns = cols,
                    row_class = function(index) cls[index])
  })

  observeEvent(defs_a(), {
    k <- keys_of(defs_a()); sel <- isolate(input$asm_pick_a)
    updateSelectInput(session, "asm_pick_a", choices = setNames(k, key_label(k)),
                      selected = if (isTRUE(sel %in% k)) sel else k[1L])
  })
  b_pick_keys <- reactive({
    kb <- keys_of(defs_b()); ka <- keys_of(defs_a())
    c(kb, setdiff(ka, kb))
  })
  observeEvent(b_pick_keys(), {
    k <- b_pick_keys(); kb <- keys_of(isolate(defs_b())); sel <- isolate(input$asm_pick_b)
    lbl <- paste0(key_label(k), ifelse(k %in% kb, "", " (not used)"))
    updateSelectInput(session, "asm_pick_b", choices = setNames(k, lbl),
                      selected = if (isTRUE(sel %in% k)) sel else k[1L])
  })


  # ---- the assortment form ---------------------------------------------------
  asm_editing  <- reactiveVal(NULL)   # list(side, key (NULL = new))
  asm_form_err <- reactiveVal(NULL)
  output$asm_form_msg <- renderUI({
    e <- asm_form_err(); if (is.null(e)) return(NULL)
    div(class = "pm-msg-error", paste(e, collapse = "\n"))
  })

  asm_modal <- function(side, d = NULL) {
    new <- is.null(d)
    v <- function(n, default) {
      x <- if (new) NULL else d[[n]]
      if (is.null(x) || length(x) == 0L || is.na(x)) default else x
    }
    fin <- function(n) { x <- v(n, NA); if (is.finite(x)) x else NA }
    sp <- c("Spruce" = "spruce", "Pine" = "pine")
    modalDialog(
      title = if (new) sprintf("Add assortment to Scenario %s%s", side,
                               if (side == "A") " (and B)" else "")
              else sprintf("Edit %s – %s (Scenario %s)", d$Name, d$Species, side),
      size = "l", easyClose = FALSE, fade = FALSE,
      layout_columns(col_widths = c(4, 4, 4),
        textInput("asm_name", "Assortment", v("Name", "")),
        selectInput("asm_species", "Species",
                    if (new) c("Spruce and pine" = "both", sp) else sp,
                    v("Species", if (new) "both" else "spruce")),
        radioButtons("asm_price_type", "Pricing",
                     c("Price matrix" = "matrix", "Flat price" = "flat"), v("PriceType", "matrix")),
        numericInput("asm_min_top", "Min top diameter (cm)", v("MinTopDiameter_cm", 0), min = 0),
        numericInput("asm_max_top", "Max top diameter (cm; empty = no limit)",
                     fin("MaxTopDiameter_cm"), min = 0),
        numericInput("asm_dtp", "Top diameter read (cm below the cut)",
                     v("DiameterTopPosition_cm", 0), min = 0, step = 10),
        numericInput("asm_min_butt", "Min butt diameter (cm)", v("MinButtDiameter_cm", 0), min = 0),
        numericInput("asm_max_butt", "Max butt diameter (cm; empty = no limit)",
                     fin("MaxButtDiameter_cm"), min = 0),
        selectInput("asm_method", "Volume method", VOLUME_METHODS,
                    v("VolumeDiameterCategory", "Top cylinder"))
      ),
      # Lengths: a flat price uses min..max in 10 cm steps; a matrix's
      # lengths are its columns (set here only for a new matrix: every 30 cm).
      conditionalPanel(
        if (new || !identical(d$PriceType, "matrix")) "true" else "input.asm_price_type == 'flat'",
        layout_columns(col_widths = c(4, 4, 4),
          numericInput("asm_min_len", "Min length (cm)", v("MinLength_cm", 310), min = 10, step = 10),
          numericInput("asm_max_len", "Max length (cm)", v("MaxLength_cm", 550), min = 10, step = 10),
          conditionalPanel("input.asm_price_type == 'matrix'",
            div(class = "note-muted", "A new price matrix starts with a length column every 30 cm.")))),
      conditionalPanel(
        if (new) "true" else sprintf("input.asm_price_type != '%s'", d$PriceType),
        layout_columns(col_widths = c(4, 4, 4),
          numericInput("asm_price", "Starting price (NOK/m³)", 500, min = 0),
          conditionalPanel("input.asm_price_type == 'matrix'",
            numericInput("asm_max_class", "Largest diameter class (cm; without a max top diameter)",
                         NA, min = 1)))),
      uiOutput("asm_form_msg"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("asm_save", "Save", class = "btn-primary")))
  }

  observeEvent(input$asm_add_a, {
    asm_editing(list(side = "A", key = NULL)); asm_form_err(NULL); showModal(asm_modal("A"))
  })
  observeEvent(input$asm_add_b, {
    asm_editing(list(side = "B", key = NULL)); asm_form_err(NULL); showModal(asm_modal("B"))
  })
  observeEvent(input$asm_edit_a, {
    d <- defs_a(); i <- match(input$asm_pick_a, keys_of(d)); req(!is.na(i))
    asm_editing(list(side = "A", key = input$asm_pick_a)); asm_form_err(NULL)
    showModal(asm_modal("A", d[i, , drop = FALSE]))
  })
  observeEvent(input$asm_edit_b, {
    d <- defs_b(); i <- match(input$asm_pick_b, keys_of(d))
    if (is.na(i)) return(say("B", "error", "Not used in Scenario B: choose “Make equal to A” to use it again."))
    asm_editing(list(side = "B", key = input$asm_pick_b)); asm_form_err(NULL)
    showModal(asm_modal("B", d[i, , drop = FALSE]))
  })

  # Form -> table rows (one per species), and the starting prices when the
  # pricing is new. Returns list(rows, ptype, start, errors).
  form_rows <- function(old_row) {
    num <- function(x) if (is.null(x) || length(x) == 0L || is.na(x)) NA_real_ else as.numeric(x)
    or  <- function(x, y) if (is.na(x)) y else x
    name <- trimws(or_default(input$asm_name, ""))
    species <- if (identical(input$asm_species, "both")) PRODUCT_SPECIES else input$asm_species
    ptype <- input$asm_price_type
    rows <- .type_assortment_table(data.frame(
      Name = name,
      Type = if (is.null(old_row)) custom_type(name) else old_row$Type,   # hidden; kept on edits
      Species = species,
      MinTopDiameter_cm = or(num(input$asm_min_top), 0),
      MaxTopDiameter_cm = or(num(input$asm_max_top), Inf),
      MinButtDiameter_cm = or(num(input$asm_min_butt), 0),
      MaxButtDiameter_cm = or(num(input$asm_max_butt), Inf),
      MinLength_cm = num(input$asm_min_len), MaxLength_cm = num(input$asm_max_len),
      DiameterTopPosition_cm = or(num(input$asm_dtp), 0),
      VolumeDiameterCategory = input$asm_method, PriceType = ptype,
      stringsAsFactors = FALSE))
    same_pricing <- !is.null(old_row) && identical(old_row$PriceType, ptype)
    if (same_pricing && ptype == "matrix") {   # lengths stay the matrix columns
      rows$MinLength_cm <- old_row$MinLength_cm; rows$MaxLength_cm <- old_row$MaxLength_cm
    }
    errs <- character(0); start <- NULL
    if (!same_pricing) {
      price <- num(input$asm_price)
      if (is.na(price) || price < 0) errs <- c(errs, "Give a starting price of 0 NOK/m³ or more.")
      lo <- rows$MinLength_cm[1]; hi_len <- rows$MaxLength_cm[1]
      lens <- if (ptype == "matrix" && !anyNA(c(lo, hi_len)) && hi_len >= lo)
        unique(c(seq(lo, hi_len, by = 30), hi_len)) else numeric(0)
      hi <- if (is.finite(rows$MaxTopDiameter_cm[1])) floor(rows$MaxTopDiameter_cm[1]) else num(input$asm_max_class)
      if (length(errs) == 0L)
        start <- tryCatch(do.call(rbind, lapply(keys_of(rows), function(k)
          pm_start_prices(k, ptype, price, max(1, floor(rows$MinTopDiameter_cm[1])), hi, lens))),
          error = function(e) { errs <<- c(errs, conditionMessage(e)); NULL })
    }
    list(rows = rows, ptype = ptype, same_pricing = same_pricing, start = start, errors = errs)
  }

  # Check and commit new A and B values; FALSE (with the form error) if
  # either scenario fails its checks.
  commit <- function(d_a, p_a, asm, st, own, p_b) {
    v <- check_price_set(d_a, p_a)
    if (!v$ok) { asm_form_err(paste("Scenario A:", v$errors)); return(FALSE) }
    bb <- b_set(d_a, p_a, p_b, st, own, asm)
    if (nrow(bb$defs) == 0L) { asm_form_err("Scenario B needs at least one assortment."); return(FALSE) }
    vb <- check_price_set(bb$defs, bb$pm)
    if (!vb$ok) { asm_form_err(paste("Scenario B:", vb$errors)); return(FALSE) }
    defs_a(d_a); pm_a(p_a); set_b(asm, st, own, p_b)
    redraw()
    TRUE
  }

  observeEvent(input$asm_save, {
    ed <- asm_editing(); side <- ed$side; old <- ed$key
    da <- defs_a(); db <- defs_b(); ka <- keys_of(da); kb <- keys_of(db)
    a <- pm_a(); b <- pm_b(); st <- b_state(); asm <- b_asm(); own <- b_own()
    tab <- if (side == "A") da else db; ktab <- keys_of(tab)
    old_row <- if (is.null(old)) NULL else tab[match(old, ktab), , drop = FALSE]
    f <- form_rows(old_row)
    new_keys <- keys_of(f$rows)
    taken <- intersect(new_keys, setdiff(if (side == "A") union(ka, kb) else kb, old))
    if (length(taken) > 0L)
      f$errors <- c(sprintf("%s already exists.", paste(key_label(taken), collapse = " and ")), f$errors)
    if (length(f$errors) > 0L) { asm_form_err(f$errors); return() }

    if (side == "A") {
      if (is.null(old)) {                      # add: to A and, linked, to B
        da <- rbind(da, f$rows)
        a <- swap_prices(a, new_keys, f$start); b <- swap_prices(b, new_keys, f$start)
        asm[new_keys] <- "same"; st[new_keys] <- "same"
      } else {
        k1 <- new_keys; i <- match(old, ka)
        linked <- identical(unname(or_default(asm[old], "same")), "same")
        da[i, ] <- f$rows
        # B rows of their own keep A's old prices when A's pricing changes.
        if (!linked && !f$same_pricing && identical(unname(st[old]), "same")) {
          b <- pm_b_copy_a(a, b, old); st[old] <- "own"
        }
        a <- if (f$same_pricing) pm_rename_key(swap_prices(a, setdiff(k1, old)), old, k1)
             else swap_prices(a, c(old, k1), f$start)
        if (k1 != old) {
          if (linked) {
            b <- pm_rename_key(swap_prices(b, setdiff(k1, old)), old, k1)
            names(asm)[names(asm) == old] <- k1; names(st)[names(st) == old] <- k1
          } else {
            # B keeps its own row under the old key (now only in B); a
            # renamed A row is not in B.
            if (identical(unname(asm[old]), "own") && identical(unname(st[old]), "same")) {
              b <- pm_b_copy_a(pm_a(), b, old); st[old] <- "own"
            }
            if (identical(unname(asm[old]), "none")) { asm <- asm[names(asm) != old]; st <- st[names(st) != old] }
            asm[k1] <- "none"; st[k1] <- "none"
          }
        }
        if (linked && !f$same_pricing) { b <- swap_prices(b, k1, f$start); st[k1] <- "same" }
      }
      ok <- commit(da, a, asm, st, own, b)
    } else {
      eff <- isolate(b_now())$pm
      if (is.null(old)) {                      # add: to B only
        own <- rbind(own[!keys_of(own) %in% new_keys, , drop = FALSE], f$rows)
        asm[new_keys] <- "own"; st[new_keys] <- "own"
        b <- swap_prices(b, new_keys, f$start)
      } else {
        k1 <- new_keys
        newp <- if (f$same_pricing) pm_rename_key(eff[eff$ProductKey == old, , drop = FALSE], old, k1)
                else f$start
        own <- rbind(own[!keys_of(own) %in% c(old, k1), , drop = FALSE], f$rows)
        if (k1 != old) {
          if (old %in% ka) { asm[old] <- "none"; st[old] <- "none" }
          else { asm <- asm[names(asm) != old]; st <- st[names(st) != old] }
        }
        asm[k1] <- "own"
        keep_state <- f$same_pricing && k1 == old
        if (!keep_state) { b <- swap_prices(b, k1, newp); st[k1] <- "own" }
      }
      ok <- commit(da, a, asm, st, own, b)
    }
    if (!isTRUE(ok)) return()
    removeModal()
    say(side, "ok", sprintf("%s %s.", if (is.null(old)) "Added" else "Saved",
                            paste(key_label(new_keys), collapse = " and ")))
  })


  # ---- remove, link, reset -----------------------------------------------------
  asm_removing <- reactiveVal(NULL)    # list(side, key)
  remove_modal <- function(side, key) {
    other <- if (side == "A") "B" else "A"
    in_other <- if (side == "A") key %in% keys_of(defs_b()) else key %in% keys_of(defs_a())
    modalDialog(
      title = sprintf("Remove %s", key_label(key)), easyClose = TRUE, fade = FALSE,
      if (in_other) sprintf("Remove it from Scenario %s only, or from both scenarios?", side)
      else sprintf("It is only in Scenario %s.", side),
      footer = tagList(modalButton("Cancel"),
        actionButton("asm_rm_one", if (in_other) sprintf("From %s only", side) else "Remove",
                     class = "btn-secondary"),
        if (in_other) actionButton("asm_rm_both", "From both", class = "btn-primary")))
  }
  observeEvent(input$asm_remove_a, {
    k <- input$asm_pick_a; req(k %in% keys_of(defs_a()))
    asm_removing(list(side = "A", key = k)); showModal(remove_modal("A", k))
  })
  observeEvent(input$asm_remove_b, {
    k <- input$asm_pick_b
    if (!k %in% keys_of(defs_b())) return(say("B", "error", sprintf("%s is not used in Scenario B.", key_label(k))))
    asm_removing(list(side = "B", key = k)); showModal(remove_modal("B", k))
  })

  do_remove <- function(both) {
    r <- asm_removing(); req(r); side <- r$side; k <- r$key; lbl <- key_label(k)
    da <- defs_a(); ka <- keys_of(da)
    a <- pm_a(); b <- pm_b(); st <- b_state(); asm <- b_asm(); own <- b_own()
    from_a <- (side == "A" || both) && k %in% ka
    from_b <- side == "B" || both
    if (from_a && nrow(da) == 1L) { removeModal(); return(say(side, "error", "Scenario A needs at least one assortment.")) }
    if (from_b) {
      if (k %in% ka && !from_a) { asm[k] <- "none"; st[k] <- "none" }
      else { asm <- asm[names(asm) != k]; st <- st[names(st) != k]; b <- swap_prices(b, k) }
      own <- own[keys_of(own) != k, , drop = FALSE]
    } else if (identical(unname(asm[k]), "same")) {
      # Removed from A only: B keeps a copy of its own.
      own <- rbind(own, da[ka == k, , drop = FALSE]); asm[k] <- "own"
      if (identical(unname(st[k]), "same")) { b <- pm_b_copy_a(a, b, k); st[k] <- "own" }
    } else if (identical(unname(asm[k]), "none")) {
      asm <- asm[names(asm) != k]; st <- st[names(st) != k]
    }
    if (from_a) { da <- da[ka != k, , drop = FALSE]; rownames(da) <- NULL; a <- swap_prices(a, k) }
    if (nrow(b_defs(da, own, asm)) == 0L) {
      removeModal(); return(say(side, "error", "Scenario B needs at least one assortment."))
    }
    defs_a(da); pm_a(a); set_b(asm, st, own, b); redraw(); removeModal()
    say(side, "ok", sprintf("Removed %s from %s.", lbl,
                            if (both) "both scenarios" else paste("Scenario", side)))
  }
  observeEvent(input$asm_rm_one,  do_remove(FALSE))
  observeEvent(input$asm_rm_both, do_remove(TRUE))

  # Make one B assortment (definition and prices) follow A again.
  observeEvent(input$asm_same_b, {
    k <- input$asm_pick_b; req(k)
    if (!k %in% keys_of(defs_a()))
      return(say("B", "error", sprintf("%s is only in Scenario B; remove it instead.", key_label(k))))
    asm <- b_asm(); st <- b_state(); asm[k] <- "same"; st[k] <- "same"
    set_b(asm, st, b_own()); redraw()
    say("B", "ok", sprintf("%s now follows Scenario A.", key_label(k)))
  })
  observeEvent(input$asm_all_same, {
    k <- keys_of(defs_a()); s <- setNames(rep("same", length(k)), k)
    set_b(s, s, b_own()[0, , drop = FALSE]); redraw()
    say("B", "ok", "Scenario B is equal to Scenario A.")
  })

  # A's base assortments back to their defaults (definitions and prices);
  # custom ones stay. B's linked rows follow.
  observeEvent(input$asm_reset, {
    base <- keys_of(APP_DEFS)
    d <- defs_a(); custom <- d[!keys_of(d) %in% base, , drop = FALSE]
    d2 <- rbind(APP_DEFS, custom)
    pm_a(swap_prices(pm_a(), base, APP_PM_DEFAULT[APP_PM_DEFAULT$ProductKey %in% base, , drop = FALSE]))
    asm <- b_asm(); st <- b_state()
    new <- setdiff(base, names(asm)); asm[new] <- "same"; st[new] <- "same"
    defs_a(d2); set_b(asm, st, b_own()); redraw()
    say("A", "ok", "Base assortments reset to the defaults; custom assortments kept.")
  })


  # ---- scenario files (assortments + prices, one xlsx per scenario) ----------
  output$dl_scen_a <- downloadHandler(
    filename = function() "optBuck_scenario_A.xlsx",
    content  = function(file) scenario_export(defs_a(), pm_a(), file))
  output$dl_scen_b <- downloadHandler(
    filename = function() "optBuck_scenario_B.xlsx",
    content  = function(file) { b <- b_now(); scenario_export(b$defs, b$pm, file) })

  observeEvent(input$scen_a_upload, {
    f <- input$scen_a_upload; req(f)
    res <- scenario_import(f$datapath)
    if (!isTRUE(res$ok))
      return(say("A", "error", paste(c(sprintf("%s was not loaded:", f$name), res$errors), collapse = "\n")))
    # B's linked rows follow the new A; B's own rows stay.
    k <- keys_of(res$defs); asm <- b_asm(); st <- b_state(); own <- b_own()
    keep <- names(asm)[asm == "own" & names(asm) %in% keys_of(own)]
    asm2 <- setNames(rep("same", length(k)), k); st2 <- asm2
    asm2[intersect(keep, k)] <- "own"
    st2[intersect(names(st), k)] <- ifelse(st[intersect(names(st), k)] == "own", "own", "same")
    st2[intersect(keep, k)] <- st[intersect(keep, k)]
    b_only <- setdiff(keep, k)
    asm2[b_only] <- "own"; st2[b_only] <- "own"
    b <- pm_b()
    for (key in b_only) if (identical(unname(st[key]), "same")) b <- pm_b_copy_a(pm_a(), b, key)
    bb <- b_set(res$defs, res$pm, b, st2, own, asm2)
    vb <- check_price_set(bb$defs, bb$pm)
    if (!vb$ok) return(say("A", "error", paste(c(sprintf("%s was not loaded (Scenario B would fail):", f$name),
                                                vb$errors), collapse = "\n")))
    defs_a(res$defs); pm_a(res$pm); set_b(asm2, st2, own, b); redraw()
    say("A", "ok", sprintf("Loaded %s: %d assortment(s) with prices.", f$name, nrow(res$defs)), res$warnings)
  })

  observeEvent(input$scen_b_upload, {
    f <- input$scen_b_upload; req(f)
    res <- scenario_import(f$datapath)
    if (!isTRUE(res$ok))
      return(say("B", "error", paste(c(sprintf("%s was not loaded:", f$name), res$errors), collapse = "\n")))
    s <- b_states_from(defs_a(), pm_a(), res$defs, res$pm)
    set_b(s$b_asm, s$state, s$defs_b_own, swap_prices(pm_b(), keys_of(res$defs), res$pm))
    redraw()
    say("B", "ok", sprintf("Loaded %s: %d assortment(s) with prices.", f$name, nrow(res$defs)), res$warnings)
  })

  msg_ui <- function(m) {
    if (is.null(m)) return(NULL)
    tagList(
      div(class = if (identical(m$type, "error")) "pm-msg-error" else "pm-msg-ok", m$text),
      if (length(m$warnings) > 0L)
        div(class = "pm-msg-warn", paste("Warning:", m$warnings, collapse = "\n")))
  }
  output$msg_a <- renderUI(msg_ui(msg_a()))
  output$msg_b <- renderUI(msg_ui(msg_b()))

  output$pm_status <- renderUI({
    div(class = "pm-status", if (b_differ()) tags$strong("Scenario B differs from A: runs compare both scenarios.")
                             else "Scenario B equals A: runs use Scenario A only.")
  })


  # ---- price matrices -----------------------------------------------------------
  is_flat <- function(id) identical(id, PM_FLAT_ID)
  # A: its matrix-priced assortments, then the flat table.
  pm_ids_a <- reactive({
    d <- defs_a(); k <- keys_of(d)
    c(k[d$PriceType == "matrix"], if (any(d$PriceType == "flat")) PM_FLAT_ID)
  })
  flat_keys_a <- reactive({ d <- defs_a(); keys_of(d)[d$PriceType == "flat"] })
  # B: B's assortments and A's assortments not used in B (to use them again).
  b_rows <- reactive({
    db <- defs_b(); da <- defs_a()
    rbind(db, da[keys_of(da) %in% names(b_asm())[b_asm() == "none"], , drop = FALSE])
  })
  pm_ids_b <- reactive({
    d <- b_rows(); k <- keys_of(d)
    c(k[d$PriceType == "matrix"], if (any(d$PriceType == "flat")) PM_FLAT_ID)
  })
  flat_keys_b <- reactive({ d <- b_rows(); keys_of(d)[d$PriceType == "flat"] })
  b_only_keys <- reactive(setdiff(keys_of(defs_b()), keys_of(defs_a())))

  id_label <- function(ids) ifelse(ids == PM_FLAT_ID, "Flat prices", key_label(ids))
  observeEvent(pm_ids_a(), {
    ids <- pm_ids_a(); sel <- isolate(input$pm_id_a)
    updateSelectInput(session, "pm_id_a", choices = setNames(ids, id_label(ids)),
                      selected = if (isTRUE(sel %in% ids)) sel else ids[1L])
  })
  observeEvent(pm_ids_b(), {
    ids <- pm_ids_b(); sel <- isolate(input$pm_id_b)
    updateSelectInput(session, "pm_id_b", choices = setNames(ids, id_label(ids)),
                      selected = if (isTRUE(sel %in% ids)) sel else ids[1L])
  })
  # The two selectors follow each other where the matrix exists on both sides.
  observeEvent(input$pm_id_a, {
    id <- input$pm_id_a
    if (isTRUE(id %in% isolate(pm_ids_b())) && !identical(id, isolate(input$pm_id_b)))
      updateSelectInput(session, "pm_id_b", selected = id)
  }, ignoreInit = TRUE)
  observeEvent(input$pm_id_b, {
    id <- input$pm_id_b
    if (isTRUE(id %in% isolate(pm_ids_a())) && !identical(id, isolate(input$pm_id_a)))
      updateSelectInput(session, "pm_id_a", selected = id)
  }, ignoreInit = TRUE)

  # The flat table is as tall as its rows, with readable product names.
  grid_height <- function(id, n) if (is_flat(id)) 34 + 24 * n else 480
  pm_widget <- function(grid, id, read_only, renderer = NULL, def = NULL) {
    w <- rhandsontable(grid, rowHeaders = if (is_flat(id)) key_label(rownames(grid)) else rownames(grid),
                       readOnly = read_only,
                       colHeaders = if (is_flat(id)) "NOK/m³" else colnames(grid),
                       rowHeaderWidth = if (is_flat(id)) 150 else 50,
                       height = grid_height(id, nrow(grid)), stretchH = "all",
                       matrix_id = id, def = def)
    w <- hot_cols(w, renderer = renderer, columnSorting = FALSE)
    w <- hot_context_menu(w, allowRowEdit = FALSE, allowColEdit = FALSE)
    if (!read_only) w <- hot_validate_numeric(w, cols = seq_len(ncol(grid)), min = 0)
    w
  }

  output$pm_a <- renderRHandsontable({
    pm_tick(); id <- input$pm_id_a; req(id, id %in% pm_ids_a())
    a <- isolate(pm_a())
    g <- if (is_flat(id)) {
      f <- pm_to_grid(a, PM_FLAT_ID); f[isolate(flat_keys_a()), , drop = FALSE]
    } else pm_to_grid(a, id)
    pm_widget(g, id, FALSE, PM_PLAIN_RENDERER)
  })

  # B: "Same as A" shows A greyed and read-only, "Own prices" B's own prices
  # (cells that differ from A highlighted), "Not used" no grid. The flat
  # table carries the state per product in its first column.
  output$pm_b <- renderRHandsontable({
    pm_tick(); id <- input$pm_id_b; req(id, id %in% pm_ids_b())
    a <- isolate(pm_a()); b <- isolate(pm_b()); st <- isolate(b_state())
    if (is_flat(id)) {
      k <- isolate(flat_keys_b())
      pa <- pm_to_grid(a, PM_FLAT_ID); pb <- pm_to_grid(b, PM_FLAT_ID)
      s  <- st[k]; s[is.na(s)] <- "same"
      pa_k <- pa[k, 1]; pb_k <- pb[k, 1]
      price <- ifelse(s == "own", pb_k, ifelse(s == "same", pa_k, NA_real_))
      g <- data.frame(B = unname(B_STATES[s]), PriceNOKperM3 = price, row.names = k,
                      stringsAsFactors = FALSE)
      w <- rhandsontable(g, rowHeaders = key_label(k), colHeaders = c("Scenario B", "NOK/m³"),
                         rowHeaderWidth = 150, height = grid_height(id, length(k)),
                         stretchH = "all", matrix_id = id,
                         def = unname(cbind(NA, ifelse(s == "none", NA, ifelse(is.na(pa_k), price, pa_k)))))
      w <- hot_col(w, "Scenario B", type = "dropdown", source = unname(B_STATES), strict = TRUE,
                   allowInvalid = FALSE)
      w <- hot_col(w, "NOK/m³", renderer = PM_DIFF_RENDERER)
      w <- hot_context_menu(w, allowRowEdit = FALSE, allowColEdit = FALSE)
      for (i in which(s != "own")) w <- hot_cell(w, i, 2, readOnly = TRUE)
      return(w)
    }
    s <- or_default(st[id], "same")
    if (s == "none") return(NULL)                     # no grid
    if (s == "same") return(pm_widget(pm_to_grid(a, id), id, TRUE, PM_SAME_RENDERER))
    pm_widget(pm_to_grid(b, id), id, FALSE, PM_DIFF_RENDERER,
              def = unname(pm_default_aligned(a, b, id)))
  })

  # The B grid is hidden when the shown matrix is not used in B.
  output$pm_b_shown <- reactive({
    id <- input$pm_id_b
    is.null(id) || is_flat(id) || !identical(unname(b_state()[id]), "none")
  })
  outputOptions(output, "pm_b_shown", suspendWhenHidden = FALSE)

  output$pm_b_state_ui <- renderUI({
    pm_tick(); id <- input$pm_id_b; req(id, !is_flat(id))   # follows uploads and resets
    ch <- if (id %in% isolate(b_only_keys())) B_STATES["own"] else B_STATES
    selectInput("pm_b_state", "Scenario B", setNames(names(ch), ch),
                selected = or_default(isolate(b_state())[id], "same"), width = "200px")
  })

  # A product's price state in B; "Not used" takes the assortment out of B,
  # and "Same as A" / "Own prices" bring it back (following A).
  set_b_price_state <- function(k, new) {
    st <- isolate(b_state()); asm <- isolate(b_asm()); b <- isolate(pm_b()); a <- isolate(pm_a())
    da <- isolate(defs_a()); own <- isolate(b_own())
    if (identical(unname(st[k]), new)) return(TRUE)
    in_a <- k %in% keys_of(da)
    if (!in_a && new != "own") {
      say("B", "error", sprintf("%s is only in Scenario B: it keeps its own prices (remove the assortment instead).",
                                key_label(k)))
      return(FALSE)
    }
    if (new == "none") {
      asm[k] <- "none"; st[k] <- "none"; own <- own[keys_of(own) != k, , drop = FALSE]
      if (nrow(b_defs(da, own, asm)) == 0L) {
        say("B", "error", "Scenario B needs at least one assortment."); return(FALSE)
      }
    } else {
      if (identical(unname(asm[k]), "none")) asm[k] <- "same"
      if (new == "same" && identical(unname(asm[k]), "own")) {
        ob <- own[keys_of(own) == k, , drop = FALSE]
        if (!identical(ob$PriceType, da$PriceType[keys_of(da) == k])) {
          say("B", "error", sprintf("%s is priced differently in Scenario B: it keeps its own prices.",
                                    key_label(k)))
          return(FALSE)
        }
      }
      if (new == "own" && !identical(unname(st[k]), "own")) b <- pm_b_copy_a(a, b, k)
      st[k] <- new
    }
    set_b(asm, st, own, b)
    TRUE
  }
  observeEvent(input$pm_b_state, {
    id <- input$pm_id_b; req(id, !is_flat(id))
    if (!set_b_price_state(id, input$pm_b_state)) {}
    redraw()
  }, ignoreInit = TRUE)
  output$pm_b_note <- renderUI({
    id <- input$pm_id_b; req(id, !is_flat(id))
    if (identical(unname(b_state()[id]), "none")) div(class = "note-muted", "Not used in Scenario B.")
  })

  # Grid edits. The matrix id travels with the widget, so a stale grid of
  # the previously shown matrix is never written elsewhere.
  apply_grid_edit <- function(x, pmv, side, flat_keys) {
    if (is.null(x$changes$changes)) return()            # load, not an edit
    id <- or_default(x$params$matrix_id, isolate(input[[paste0("pm_id_", tolower(side))]]))
    g  <- hot_to_r(x)
    ref <- pm_to_grid(isolate(pmv()), id)
    if (is_flat(id)) ref <- ref[flat_keys, , drop = FALSE]   # the grid's row order
    if (!identical(dim(g), dim(ref))) return()
    rownames(g) <- rownames(ref); colnames(g) <- colnames(ref)
    if (any(as.matrix(g) < 0, na.rm = TRUE)) {
      say(side, "error", "Negative prices are not allowed; the edit was undone.")
      redraw(); return()
    }
    pmv(pm_from_grid(g, id, isolate(pmv())))
  }
  observeEvent(input$pm_a, apply_grid_edit(input$pm_a, pm_a, "A", isolate(flat_keys_a())))
  observeEvent(input$pm_b, {
    x <- input$pm_b
    if (is.null(x$changes$changes)) return()
    id <- or_default(x$params$matrix_id, isolate(input$pm_id_b))
    if (!is_flat(id)) {
      if (identical(unname(isolate(b_state())[id]), "own")) apply_grid_edit(x, pm_b, "B", character(0))
      return()
    }
    g <- hot_to_r(x); k <- isolate(flat_keys_b())
    if (nrow(g) != length(k)) return()
    st <- isolate(b_state())
    new_st <- names(B_STATES)[match(g$B, B_STATES)]
    for (i in seq_along(k)) {
      if (is.na(new_st[i])) next
      if (!identical(unname(or_default(st[k[i]], "same")), new_st[i])) {
        set_b_price_state(k[i], new_st[i]); redraw(); return()
      }
      if (new_st[i] == "own") {
        p <- as.numeric(g$PriceNOKperM3[i])
        if (!is.na(p) && p < 0) {
          say("B", "error", "Negative prices are not allowed; the edit was undone.")
          redraw(); return()
        }
        b <- isolate(pm_b())
        j <- b$ProductKey == k[i] & is.na(b$LengthClass_dm)
        if (!is.na(p) && any(j) && abs(b$PriceNOKperM3[j] - p) > 1e-9) {
          b$PriceNOKperM3[j] <- p; pm_b(b)
        }
      }
    }
  })

  # Rows (diameter classes, ends only) and columns (lengths) of a matrix.
  axes_ui <- function(s, id, pm) {
    lens <- sort(unique(pm$LengthClass_dm[pm$ProductKey == id & !is.na(pm$LengthClass_dm)]))
    b <- function(n, l) actionButton(paste0(s, n), l, class = "btn-secondary btn-sm")
    div(class = "pm-axes",
      div(class = "pm-axes-row",
          span(class = "pm-axes-lbl", "Diameter classes"),
          b("_cls_add_lo", "+ smaller"), b("_cls_rm_lo", "− smallest"),
          b("_cls_add_hi", "+ larger"), b("_cls_rm_hi", "− largest")),
      div(class = "pm-axes-row",
          span(class = "pm-axes-lbl", "Lengths (dm)"),
          numericInput(paste0(s, "_len_new"), NULL, NA, min = 1, step = 1, width = "90px"),
          b("_len_add", "Add"),
          selectInput(paste0(s, "_len_pick"), NULL, lens, width = "90px"),
          b("_len_rm", "Remove")))
  }
  output$pm_a_axes <- renderUI({
    pm_tick(); id <- input$pm_id_a; req(id, !is_flat(id), id %in% pm_ids_a())
    axes_ui("pm_a", id, isolate(pm_a()))
  })
  output$pm_b_axes <- renderUI({
    pm_tick(); id <- input$pm_id_b; req(id, !is_flat(id), id %in% pm_ids_b())
    req(identical(unname(isolate(b_state())[id]), "own"))
    axes_ui("pm_b", id, isolate(pm_b()))
  })
  axes_observers <- function(s, pmv, side) {
    edit <- function(f) {
      id <- isolate(input[[paste0("pm_id_", tolower(side))]]); req(id, !is_flat(id))
      res <- tryCatch(f(isolate(pmv()), id), error = function(e) e)
      if (inherits(res, "error")) return(say(side, "error", conditionMessage(res)))
      pmv(res); (if (side == "A") msg_a else msg_b)(NULL); redraw()
    }
    observeEvent(input[[paste0(s, "_cls_add_lo")]], edit(function(pm, id) pm_add_class(pm, id, "low")))
    observeEvent(input[[paste0(s, "_cls_add_hi")]], edit(function(pm, id) pm_add_class(pm, id, "high")))
    observeEvent(input[[paste0(s, "_cls_rm_lo")]],  edit(function(pm, id) pm_remove_class(pm, id, "low")))
    observeEvent(input[[paste0(s, "_cls_rm_hi")]],  edit(function(pm, id) pm_remove_class(pm, id, "high")))
    observeEvent(input[[paste0(s, "_len_add")]],
                 edit(function(pm, id) pm_add_length(pm, id, input[[paste0(s, "_len_new")]])))
    observeEvent(input[[paste0(s, "_len_rm")]],
                 edit(function(pm, id) pm_remove_length(pm, id, input[[paste0(s, "_len_pick")]])))
  }
  axes_observers("pm_a", pm_a, "A")
  axes_observers("pm_b", pm_b, "B")

  # Reset to default prices: the base products' default rows (classes and
  # lengths too), where the product is priced as by default. In B, a base
  # product equal to A goes back to "Same as A", otherwise it gets the
  # default prices as its own.
  base_reset_keys <- function(d) {
    k <- keys_of(d); bk <- keys_of(APP_DEFS)
    k[k %in% bk & d$PriceType == APP_DEFS$PriceType[match(k, bk)]]
  }
  observeEvent(input$pm_a_reset, {
    k <- base_reset_keys(defs_a())
    pm_a(swap_prices(pm_a(), k, APP_PM_DEFAULT[APP_PM_DEFAULT$ProductKey %in% k, , drop = FALSE]))
    redraw()
    say("A", "ok", "Prices of the base assortments reset.")
  })
  observeEvent(input$pm_b_reset, {
    k <- base_reset_keys(defs_b()); st <- b_state(); a <- pm_minimal(pm_a())
    def <- APP_PM_DEFAULT[APP_PM_DEFAULT$ProductKey %in% k, , drop = FALSE]
    for (key in k) {
      same <- isTRUE(all.equal(a[a$ProductKey == key, ], pm_minimal(def[def$ProductKey == key, ]),
                               check.attributes = FALSE)) &&
              key %in% keys_of(defs_a()) &&
              (!identical(unname(b_asm()[key]), "own") ||
               identical(b_own()$PriceType[keys_of(b_own()) == key],
                         defs_a()$PriceType[keys_of(defs_a()) == key]))
      st[key] <- if (same) "same" else "own"
    }
    set_b(b_asm(), st, b_own(), swap_prices(pm_b(), k, def)); redraw()
    say("B", "ok", "Prices of the base assortments reset.")
  })

  # Price matrix uploads (helpers_upload.R).
  do_upload <- function(side, f) {
    res <- upload_price_set(f$datapath, f$name, side, defs_a(), pm_a(), pm_b(), b_state(),
                            b_own(), b_asm())
    if (isTRUE(res$ok)) {
      if (side == "A") pm_a(res$pm) else set_b(res$b_asm, res$state, b_own(), res$pm)
      redraw()
      say(side, "ok", sprintf("Loaded %s: %s.", f$name,
                              paste(id_label(res$ids), collapse = ", ")), res$warnings)
    } else {
      say(side, "error", paste(c(sprintf("%s was not loaded:", f$name), res$errors), collapse = "\n"))
    }
  }
  observeEvent(input$pm_a_upload, { req(input$pm_a_upload); do_upload("A", input$pm_a_upload) })
  observeEvent(input$pm_b_upload, { req(input$pm_b_upload); do_upload("B", input$pm_b_upload) })

  side_pm <- function(side) if (side == "A") pm_a() else b_now()$pm
  output$dl_a_wide <- downloadHandler(filename = function() "optBuck_prices_A.xlsx",
                                      content = function(file) pm_template_wide(side_pm("A"), file))
  output$dl_a_long <- downloadHandler(filename = function() "optBuck_prices_A.csv",
                                      content = function(file) pm_export(side_pm("A"), file))
  output$dl_b_wide <- downloadHandler(filename = function() "optBuck_prices_B.xlsx",
                                      content = function(file) pm_template_wide(side_pm("B"), file))
  output$dl_b_long <- downloadHandler(filename = function() "optBuck_prices_B.csv",
                                      content = function(file) pm_export(side_pm("B"), file))

  list(defs_a = defs_a, pm_a = pm_a, b_now = b_now, b_differ = b_differ,
       pm_state = pm_state, key_label = key_label)
}
