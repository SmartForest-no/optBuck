# app/server_scenarios.R
#
# Purpose : The "Assortments and prices" tab (Phase 4d): Scenario A and
#           Scenario B, each with its own assortment table and price matrices.
#           Called from server() in app_server.R; returns the reactives the
#           runs need.
#
# State   : Two independent scenarios, sc$A and sc$B, each list(defs = the
#           assortment table, pm = its prices). B starts as an exact copy of
#           A; nothing is linked. Both sides are always editable; cells and
#           rows that differ from the other scenario are highlighted on both
#           sides. "Make A equal to B" / "Make B equal to A" copy a whole
#           scenario, one assortment or prices (R/helpers_price_matrix.R:
#           copy_assortment(), copy_prices()). A run compares A and B when
#           scenarios_differ().
#           Internal names: "default" and "scenario" in the backend mean A and B.

scenario_server <- function(input, output, session) {
  keys_of <- assortment_table_keys
  sc <- list(A = list(defs = reactiveVal(APP_DEFS), pm = reactiveVal(APP_PM_DEFAULT)),
             B = list(defs = reactiveVal(APP_DEFS), pm = reactiveVal(APP_PM_DEFAULT)))
  other  <- function(side) if (side == "A") "B" else "A"
  low    <- function(side) tolower(side)
  defs   <- function(side) sc[[side]]$defs()
  prices <- function(side) sc[[side]]$pm()

  pm_tick <- reactiveVal(0L)                      # re-render the grids
  redraw  <- function() pm_tick(isolate(pm_tick()) + 1L)
  msgs    <- list(A = reactiveVal(NULL), B = reactiveVal(NULL))
  say     <- function(side, type, text, warnings = NULL)
    msgs[[side]](list(type = type, text = text, warnings = warnings))

  differ   <- reactive(scenarios_differ(defs("A"), prices("A"), defs("B"), prices("B")))
  # Everything that changes a run; a run stores it to detect stale results.
  pm_state <- reactive(list(defs("A"), prices("A"), defs("B"), prices("B")))

  # Check a scenario and store it; FALSE (with the message) if it fails.
  commit <- function(side, d, pm, err = function(e) say(side, "error", e)) {
    if (nrow(d) == 0L) { err(sprintf("Scenario %s needs at least one assortment.", side)); return(FALSE) }
    v <- check_price_set(d, pm)
    if (!v$ok) { err(paste(sprintf("Scenario %s:", side), v$errors)); return(FALSE) }
    sc[[side]]$defs(d); sc[[side]]$pm(v$pm)
    TRUE
  }

  # "Spruce sawlog" etc. for keys of either scenario.
  key_label <- function(keys) {
    a <- isolate(defs("A")); b <- isolate(defs("B"))
    d <- rbind(a, b[!keys_of(b) %in% keys_of(a), , drop = FALSE])
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
      Pricing = ifelse(d$PriceType == "matrix", "Matrix", "Flat"),
      check.names = FALSE, stringsAsFactors = FALSE)
  }
  HIDDEN_ASM_COLS <- c("MinButtDiameter_cm", "DiameterTopPosition_cm")

  # One scenario's table: cells that differ from the other scenario are
  # highlighted (differences in fields only in the form mark the name), rows
  # only in this scenario are highlighted, and the other scenario's
  # assortments missing here are listed as a muted "Not in A/B" line.
  render_asm <- function(side) renderReactable({
    d <- defs(side); o <- defs(other(side))
    disp <- asm_display(d, prices(side)); disp_o <- asm_display(o, prices(other(side)))
    k <- keys_of(d); ko <- keys_of(o)
    rd <- assortment_diff(d, o)
    cell <- matrix(FALSE, nrow(disp), ncol(disp), dimnames = list(NULL, names(disp)))
    for (i in seq_len(nrow(disp))) {
      j <- match(k[i], ko)
      if (is.na(j)) next
      cell[i, ] <- unlist(disp[i, ]) != unlist(disp_o[j, ])
      if (any(rd$diff[i, HIDDEN_ASM_COLS])) cell[i, "Assortment"] <- TRUE
    }
    cls <- ifelse(rd$status == "only", "asm-only", "")
    gone <- match(rd$other_only, ko)
    if (length(gone)) {
      extra <- disp_o[gone, , drop = FALSE]
      extra$Pricing <- paste("Not in", side)
      disp <- rbind(disp, extra)
      cell <- rbind(cell, matrix(FALSE, length(gone), ncol(cell)))
      cls <- c(cls, rep("asm-none", length(gone)))
    }
    rownames(disp) <- NULL
    cols <- setNames(lapply(names(disp), function(n) reactable::colDef(
      class = function(value, index) if (isTRUE(cell[index, n])) "asm-diff")), names(disp))
    apter_reactable(disp, page_size = 20L, columns = cols,
                    row_class = function(index) cls[index])
  })
  output$tbl_asm_a <- render_asm("A")
  output$tbl_asm_b <- render_asm("B")

  # Pickers: this scenario's assortments, then the other's missing here (to
  # copy them over).
  for (side in c("A", "B")) local({
    side <- side
    observe({
      k <- keys_of(defs(side)); ko <- setdiff(keys_of(defs(other(side))), k)
      ids <- c(k, ko); sel <- isolate(input[[paste0("asm_pick_", low(side))]])
      updateSelectInput(session, paste0("asm_pick_", low(side)),
                        choices = setNames(ids, paste0(key_label(ids),
                                                       ifelse(ids %in% k, "", paste0(" (only in ", other(side), ")")))),
                        selected = if (isTRUE(sel %in% ids)) sel else ids[1L])
    })
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
      title = if (new) sprintf("Add assortment to Scenario %s", side)
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
      if (new) checkboxInput("asm_both", "Add to both scenarios", TRUE),
      uiOutput("asm_form_msg"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("asm_save", "Save", class = "btn-primary")))
  }

  for (side in c("A", "B")) local({
    side <- side; s <- low(side)
    observeEvent(input[[paste0("asm_add_", s)]], {
      asm_editing(list(side = side, key = NULL)); asm_form_err(NULL); showModal(asm_modal(side))
    })
    observeEvent(input[[paste0("asm_edit_", s)]], {
      d <- defs(side); k <- input[[paste0("asm_pick_", s)]]; i <- match(k, keys_of(d))
      if (is.na(i)) return(say(side, "error", sprintf("%s is not in Scenario %s.", key_label(k), side)))
      asm_editing(list(side = side, key = k)); asm_form_err(NULL)
      showModal(asm_modal(side, d[i, , drop = FALSE]))
    })
  })

  # Form -> table rows (one per species), and the starting prices when the
  # pricing is new. Returns list(rows, ptype, same_pricing, start, errors).
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

  observeEvent(input$asm_save, {
    ed <- asm_editing(); side <- ed$side; old <- ed$key
    both <- is.null(old) && isTRUE(input$asm_both)
    d <- defs(side); k <- keys_of(d); pm <- prices(side)
    old_row <- if (is.null(old)) NULL else d[match(old, k), , drop = FALSE]
    f <- form_rows(old_row)
    new_keys <- keys_of(f$rows)
    taken <- intersect(new_keys, setdiff(k, old))
    if (both) taken <- union(taken, intersect(new_keys, keys_of(defs(other(side)))))
    if (length(taken) > 0L)
      f$errors <- c(sprintf("%s already exists.", paste(key_label(taken), collapse = " and ")), f$errors)
    if (length(f$errors) > 0L) { asm_form_err(f$errors); return() }

    if (is.null(old)) {
      d2 <- rbind(d, f$rows); pm2 <- swap_prices(pm, new_keys, f$start)
    } else {
      i <- match(old, k); d2 <- d; d2[i, ] <- f$rows
      pm2 <- if (f$same_pricing) pm_rename_key(swap_prices(pm, setdiff(new_keys, old)), old, new_keys)
             else swap_prices(pm, c(old, new_keys), f$start)
    }
    err <- function(e) asm_form_err(e)
    v <- check_price_set(d2, pm2)
    if (!v$ok) return(err(paste(sprintf("Scenario %s:", side), v$errors)))
    if (both) {
      o <- other(side)
      d3 <- rbind(defs(o), f$rows); pm3 <- swap_prices(prices(o), new_keys, f$start)
      if (!commit(o, d3, pm3, err)) return()
    }
    commit(side, d2, v$pm, err)
    redraw(); removeModal()
    say(side, "ok", sprintf("%s %s%s.", if (is.null(old)) "Added" else "Saved",
                            paste(key_label(new_keys), collapse = " and "),
                            if (both) " to both scenarios" else ""))
  })


  # ---- remove, copy, reset -----------------------------------------------------
  asm_removing <- reactiveVal(NULL)    # list(side, key)
  for (side in c("A", "B")) local({
    side <- side; s <- low(side)
    observeEvent(input[[paste0("asm_remove_", s)]], {
      k <- input[[paste0("asm_pick_", s)]]
      if (!isTRUE(k %in% keys_of(defs(side))))
        return(say(side, "error", sprintf("%s is not in Scenario %s.", key_label(k), side)))
      in_other <- k %in% keys_of(defs(other(side)))
      asm_removing(list(side = side, key = k))
      showModal(modalDialog(
        title = sprintf("Remove %s", key_label(k)), easyClose = TRUE, fade = FALSE,
        if (in_other) sprintf("Remove it from Scenario %s only, or from both scenarios?", side)
        else sprintf("It is only in Scenario %s.", side),
        footer = tagList(modalButton("Cancel"),
          actionButton("asm_rm_one", if (in_other) sprintf("From %s only", side) else "Remove",
                       class = "btn-secondary"),
          if (in_other) actionButton("asm_rm_both", "From both", class = "btn-primary"))))
    })

    # The whole scenario equal to the other one (assortments and prices).
    observeEvent(input[[paste0("asm_copy_all_", s)]], {
      o <- other(side)
      sc[[side]]$defs(defs(o)); sc[[side]]$pm(prices(o)); redraw()
      say(side, "ok", sprintf("Scenario %s is equal to Scenario %s.", side, o))
    })
    # One assortment (definition and prices) equal to the other scenario's.
    observeEvent(input[[paste0("asm_copy_one_", s)]], {
      o <- other(side); k <- input[[paste0("asm_pick_", s)]]
      if (!isTRUE(k %in% keys_of(defs(o))))
        return(say(side, "error", sprintf("%s is not in Scenario %s.", key_label(k), o)))
      r <- copy_assortment(defs(o), prices(o), defs(side), prices(side), k)
      if (commit(side, r$defs, r$pm)) {
        redraw(); say(side, "ok", sprintf("%s is equal to Scenario %s.", key_label(k), o))
      }
    })

    # Base assortments back to their defaults (definitions and prices);
    # custom ones stay.
    observeEvent(input[[paste0("asm_reset_", s)]], {
      base <- keys_of(APP_DEFS)
      d <- defs(side); custom <- d[!keys_of(d) %in% base, , drop = FALSE]
      pm <- swap_prices(prices(side), base, APP_PM_DEFAULT[APP_PM_DEFAULT$ProductKey %in% base, , drop = FALSE])
      if (commit(side, rbind(APP_DEFS, custom), pm)) {
        redraw(); say(side, "ok", "Base assortments reset to the defaults; custom assortments kept.")
      }
    })
  })

  do_remove <- function(both) {
    r <- asm_removing(); req(r); side <- r$side; k <- r$key; lbl <- key_label(k)
    sides <- c(side, if (both) other(side))
    new <- lapply(setNames(sides, sides), function(x) {
      d <- defs(x); keep <- keys_of(d) != k
      list(defs = d[keep, , drop = FALSE], pm = swap_prices(prices(x), k))
    })
    for (x in sides) if (nrow(new[[x]]$defs) == 0L) {
      removeModal(); return(say(side, "error", sprintf("Scenario %s needs at least one assortment.", x)))
    }
    for (x in sides) { rownames(new[[x]]$defs) <- NULL; sc[[x]]$defs(new[[x]]$defs); sc[[x]]$pm(new[[x]]$pm) }
    redraw(); removeModal()
    say(side, "ok", sprintf("Removed %s from %s.", lbl,
                            if (both) "both scenarios" else paste("Scenario", side)))
  }
  observeEvent(input$asm_rm_one,  do_remove(FALSE))
  observeEvent(input$asm_rm_both, do_remove(TRUE))


  # ---- scenario files (assortments + prices, one xlsx per scenario) ----------
  for (side in c("A", "B")) local({
    side <- side; s <- low(side)
    output[[paste0("dl_scen_", s)]] <- downloadHandler(
      filename = function() sprintf("optBuck_scenario_%s.xlsx", side),
      content  = function(file) scenario_export(defs(side), prices(side), file))
    observeEvent(input[[paste0("scen_", s, "_upload")]], {
      f <- input[[paste0("scen_", s, "_upload")]]; req(f)
      res <- scenario_import(f$datapath)
      if (!isTRUE(res$ok))
        return(say(side, "error", paste(c(sprintf("%s was not loaded:", f$name), res$errors), collapse = "\n")))
      sc[[side]]$defs(res$defs); sc[[side]]$pm(res$pm); redraw()
      say(side, "ok", sprintf("Loaded %s: %d assortment(s) with prices.", f$name, nrow(res$defs)), res$warnings)
    })
  })

  msg_ui <- function(m) {
    if (is.null(m)) return(NULL)
    tagList(
      div(class = if (identical(m$type, "error")) "pm-msg-error" else "pm-msg-ok", m$text),
      if (length(m$warnings) > 0L)
        div(class = "pm-msg-warn", paste("Warning:", m$warnings, collapse = "\n")))
  }
  output$msg_a <- renderUI(msg_ui(msgs$A()))
  output$msg_b <- renderUI(msg_ui(msgs$B()))


  # ---- price matrices -----------------------------------------------------------
  is_flat <- function(id) identical(id, PM_FLAT_ID)
  # A scenario's matrices: its matrix-priced assortments, then the flat table.
  pm_ids <- function(side) {
    d <- defs(side); k <- keys_of(d)
    c(k[d$PriceType == "matrix"], if (any(d$PriceType == "flat")) PM_FLAT_ID)
  }
  flat_keys <- function(side) { d <- defs(side); keys_of(d)[d$PriceType == "flat"] }
  id_label <- function(ids) ifelse(ids == PM_FLAT_ID, "Flat prices", key_label(ids))

  # The two matrix selectors are independent.
  for (side in c("A", "B")) local({
    side <- side; s <- low(side)
    observe({
      ids <- pm_ids(side); sel <- isolate(input[[paste0("pm_id_", s)]])
      updateSelectInput(session, paste0("pm_id_", s), choices = setNames(ids, id_label(ids)),
                        selected = if (isTRUE(sel %in% ids)) sel else ids[1L])
    })
  })

  # The flat table is as tall as its rows, with readable product names.
  grid_height <- function(id, n) if (is_flat(id)) 34 + 24 * n else 480
  pm_widget <- function(grid, id, def) {
    w <- rhandsontable(grid, rowHeaders = if (is_flat(id)) key_label(rownames(grid)) else rownames(grid),
                       colHeaders = if (is_flat(id)) "NOK/m³" else colnames(grid),
                       rowHeaderWidth = if (is_flat(id)) 150 else 50,
                       height = grid_height(id, nrow(grid)), stretchH = "all",
                       matrix_id = id, def = def)
    w <- hot_cols(w, renderer = PM_DIFF_RENDERER, columnSorting = FALSE)
    w <- hot_context_menu(w, allowRowEdit = FALSE, allowColEdit = FALSE)
    hot_validate_numeric(w, cols = seq_len(ncol(grid)), min = 0)
  }

  # Both scenarios editable; a cell that differs from the other scenario
  # (or has no counterpart there) is highlighted.
  render_grid <- function(side) renderRHandsontable({
    pm_tick(); s <- low(side); id <- input[[paste0("pm_id_", s)]]
    req(id, id %in% pm_ids(side))
    pm <- isolate(prices(side)); o <- other(side)
    po <- isolate(prices(o)); do <- isolate(defs(o))
    if (is_flat(id)) {
      k <- isolate(flat_keys(side))
      g <- pm_to_grid(pm, PM_FLAT_ID)[k, , drop = FALSE]
      fo <- pm_to_grid(po, PM_FLAT_ID)
      ko <- keys_of(do)[do$PriceType == "flat"]
      def <- ifelse(k %in% ko & k %in% rownames(fo), fo[k, 1], NA_real_)
      return(pm_widget(g, id, unname(cbind(def))))
    }
    g <- pm_to_grid(pm, id)
    has_other <- id %in% keys_of(do)[do$PriceType == "matrix"] &&
                 any(po$ProductKey == id & !is.na(po$LengthClass_dm))
    def <- if (has_other) pm_default_aligned(po, pm, id)
           else matrix(NA_real_, nrow(g), ncol(g))
    pm_widget(g, id, unname(def))
  })
  output$pm_a <- render_grid("A")
  output$pm_b <- render_grid("B")

  # Grid edits. The matrix id travels with the widget, so a stale grid of
  # the previously shown matrix is never written elsewhere.
  apply_grid_edit <- function(x, side) {
    if (is.null(x$changes$changes)) return()            # load, not an edit
    id <- or_default(x$params$matrix_id, isolate(input[[paste0("pm_id_", low(side))]]))
    g  <- hot_to_r(x)
    ref <- pm_to_grid(isolate(prices(side)), id)
    if (is_flat(id)) ref <- ref[isolate(flat_keys(side)), , drop = FALSE]   # the grid's row order
    if (!identical(dim(g), dim(ref))) return()
    rownames(g) <- rownames(ref); colnames(g) <- colnames(ref)
    if (any(as.matrix(g) < 0, na.rm = TRUE)) {
      say(side, "error", "Negative prices are not allowed; the edit was undone.")
      redraw(); return()
    }
    sc[[side]]$pm(pm_from_grid(g, id, isolate(prices(side))))
    redraw()                                          # the other side's highlighting
  }
  observeEvent(input$pm_a, apply_grid_edit(input$pm_a, "A"))
  observeEvent(input$pm_b, apply_grid_edit(input$pm_b, "B"))

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

  # Reset prices: the base products' default rows (classes and lengths
  # too), where the product is priced as by default.
  base_reset_keys <- function(d) {
    k <- keys_of(d); bk <- keys_of(APP_DEFS)
    k[k %in% bk & d$PriceType == APP_DEFS$PriceType[match(k, bk)]]
  }

  for (side in c("A", "B")) local({
    side <- side; s <- low(side); p <- paste0("pm_", s)
    output[[paste0(p, "_axes")]] <- renderUI({
      pm_tick(); id <- input[[paste0("pm_id_", s)]]; req(id, !is_flat(id), id %in% pm_ids(side))
      axes_ui(p, id, isolate(prices(side)))
    })
    edit <- function(f) {
      id <- isolate(input[[paste0("pm_id_", s)]]); req(id, !is_flat(id))
      res <- tryCatch(f(isolate(prices(side)), id), error = function(e) e)
      if (inherits(res, "error")) return(say(side, "error", conditionMessage(res)))
      sc[[side]]$pm(res); msgs[[side]](NULL); redraw()
    }
    observeEvent(input[[paste0(p, "_cls_add_lo")]], edit(function(pm, id) pm_add_class(pm, id, "low")))
    observeEvent(input[[paste0(p, "_cls_add_hi")]], edit(function(pm, id) pm_add_class(pm, id, "high")))
    observeEvent(input[[paste0(p, "_cls_rm_lo")]],  edit(function(pm, id) pm_remove_class(pm, id, "low")))
    observeEvent(input[[paste0(p, "_cls_rm_hi")]],  edit(function(pm, id) pm_remove_class(pm, id, "high")))
    observeEvent(input[[paste0(p, "_len_add")]],
                 edit(function(pm, id) pm_add_length(pm, id, input[[paste0(p, "_len_new")]])))
    observeEvent(input[[paste0(p, "_len_rm")]],
                 edit(function(pm, id) pm_remove_length(pm, id, input[[paste0(p, "_len_pick")]])))

    observeEvent(input[[paste0(p, "_reset")]], {
      k <- base_reset_keys(defs(side))
      if (commit(side, defs(side), swap_prices(prices(side), k,
                                               APP_PM_DEFAULT[APP_PM_DEFAULT$ProductKey %in% k, , drop = FALSE]))) {
        redraw(); say(side, "ok", "Prices of the base assortments reset.")
      }
    })

    # Prices equal to the other scenario's: all of them, or the shown matrix
    # (the flat table: all flat prices). Assortments priced differently in
    # the two scenarios, or missing in one, keep their prices.
    copy <- function(keys) {
      o <- other(side)
      r <- copy_prices(defs(o), prices(o), defs(side), prices(side), keys)
      if (!commit(side, defs(side), r$pm)) return()
      redraw()
      say(side, "ok", paste0(
        if (length(r$copied)) sprintf("Prices equal to Scenario %s: %s.", o,
                                      paste(key_label(r$copied), collapse = ", "))
        else "No prices copied.",
        if (length(r$skipped)) sprintf(" Not copied (not in both scenarios or priced differently): %s.",
                                       paste(key_label(r$skipped), collapse = ", ")) else ""))
    }
    observeEvent(input[[paste0(p, "_copy_all")]], copy(keys_of(defs(side))))
    observeEvent(input[[paste0(p, "_copy_one")]], {
      id <- input[[paste0("pm_id_", s)]]; req(id)
      copy(if (is_flat(id)) flat_keys(side) else id)
    })

    observeEvent(input[[paste0(p, "_upload")]], {
      f <- input[[paste0(p, "_upload")]]; req(f)
      res <- upload_price_set(f$datapath, f$name, defs(side), prices(side))
      if (isTRUE(res$ok)) {
        sc[[side]]$pm(res$pm); redraw()
        say(side, "ok", sprintf("Loaded %s: %s.", f$name, paste(id_label(res$ids), collapse = ", ")),
            res$warnings)
      } else {
        say(side, "error", paste(c(sprintf("%s was not loaded:", f$name), res$errors), collapse = "\n"))
      }
    })
    output[[paste0("dl_", s, "_wide")]] <- downloadHandler(
      filename = function() sprintf("optBuck_prices_%s.xlsx", side),
      content = function(file) pm_template_wide(prices(side), file))
    output[[paste0("dl_", s, "_long")]] <- downloadHandler(
      filename = function() sprintf("optBuck_prices_%s.csv", side),
      content = function(file) pm_export(prices(side), file))
  })

  list(defs_a = sc$A$defs, pm_a = sc$A$pm, defs_b = sc$B$defs, pm_b = sc$B$pm,
       differ = differ, pm_state = pm_state, key_label = key_label)
}
