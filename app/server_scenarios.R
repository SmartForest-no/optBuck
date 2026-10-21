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
#           sides. "Make assortment equal to A/B" and "Make matrix equal to
#           A/B" copy one assortment or one matrix from the other scenario
#           (R/helpers_price_matrix.R: copy_assortment(), copy_prices()); a
#           whole scenario is copied by download and upload. A run compares A and B when
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


  # ---- the selected assortment (shared by A and B) ---------------------------
  # A row click in either table selects that assortment on both sides; a side
  # without it shows no detail panel. If the key leaves both scenarios, the
  # first row of A (or B) is selected.
  first_key <- function() {
    d <- isolate(defs("A")); if (nrow(d) == 0L) d <- isolate(defs("B"))
    keys_of(d)[asm_order(d)][1L]
  }
  selected <- reactiveVal(NULL)
  observe({
    k <- selected()
    if (is.null(k) || !k %in% c(keys_of(defs("A")), keys_of(defs("B")))) selected(first_key())
  })
  observeEvent(input$asm_click, selected(input$asm_click))
  # "matrix", "flat" or "none" (not in this scenario): which parts of the
  # detail panel show.
  sel_state <- function(side) {
    k <- selected(); d <- defs(side); i <- match(k, keys_of(d))
    if (is.null(k) || is.na(i)) "none" else d$PriceType[i]
  }
  sel_row <- function(side) { d <- defs(side); d[match(selected(), keys_of(d)), , drop = FALSE] }


  # ---- assortment tables ----------------------------------------------------
  # Each value formatted on its own ("4", "14.2"; not "4.0" next to 14.2).
  dash <- function(x) ifelse(is.finite(x), vapply(x, function(v) format(v, trim = TRUE), ""), "–")
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
  HIDDEN_ASM_COLS <- c("MinButtDiameter_cm", "DiameterTopPosition_cm", "PriceLengths_cm")

  # Fixed row order, the same in both tables so rows line up: by species
  # (spruce, pine), then sawlog, pulpwood, waste, then custom assortments (by
  # name). Uses the hidden Type, so a renamed base waste row stays a waste row.
  asm_order <- function(d) {
    sp <- match(d$Species, PRODUCT_SPECIES)
    tp <- match(d$Type, c("sawlog", "pulpwood", "waste")); tp[is.na(tp)] <- 4L
    order(sp, tp, ifelse(tp == 4L, tolower(d$Name), ""))
  }

  # One scenario's table: its own assortments only. Cells that differ from
  # the same assortment in the other scenario are highlighted (differences in
  # fields only in the form mark the name). Rows the other scenario lacks
  # are not highlighted. Columns cannot be sorted.
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
    cls <- rep("", nrow(d))   # rows the other scenario lacks are not shaded
    all_rows <- d
    ord <- asm_order(all_rows)
    disp <- disp[ord, , drop = FALSE]; cell <- cell[ord, , drop = FALSE]; cls <- cls[ord]
    rownames(disp) <- NULL
    cols <- setNames(lapply(names(disp), function(n) reactable::colDef(
      class = function(value, index) if (isTRUE(cell[index, n])) "asm-diff")), names(disp))
    # Hidden key column: a row click selects that assortment on both sides.
    disp$.key <- keys_of(all_rows)[ord]
    cls <- paste(cls, ifelse(disp$.key == or_default(selected(), ""), "asm-selected", ""))
    cols$.key <- reactable::colDef(show = FALSE)
    apter_reactable(disp, page_size = 20L, columns = cols, sortable = FALSE,
                    row_class = function(index) cls[index],
                    on_click = reactable::JS(
                      "function(rowInfo) { Shiny.setInputValue('asm_click', rowInfo.original['.key'], {priority: 'event'}); }"))
  })
  output$tbl_asm_a <- render_asm("A")
  output$tbl_asm_b <- render_asm("B")

  # The detail panel's header: the selected assortment's name, its rules in
  # one line (fields that differ from the other scenario highlighted) and
  # Edit. The panel's parts follow output.asm_state_<s>.
  for (side in c("A", "B")) local({
    side <- side; s <- low(side)
    output[[paste0("asm_state_", s)]] <- reactive(sel_state(side))
    outputOptions(output, paste0("asm_state_", s), suspendWhenHidden = FALSE)
    output[[paste0("asm_head_", s)]] <- renderUI({
      k <- selected(); req(k)
      title <- div(class = "asm-detail-title", key_label(k))
      if (sel_state(side) == "none") return(title)
      d <- defs(side); i <- match(k, keys_of(d))
      disp <- asm_display(d, prices(side))[i, ]
      o <- defs(other(side)); j <- match(k, keys_of(o))
      disp_o <- if (!is.na(j)) asm_display(o, prices(other(side)))[j, ]
      hidden_diff <- !is.na(j) && any(assortment_diff(d[i, , drop = FALSE], o)$diff[1L, HIDDEN_ASM_COLS])
      field <- function(n, fmt = "%s") {
        differs <- !is.null(disp_o) && !identical(disp[[n]], disp_o[[n]])
        span(class = paste("asm-rule", if (differs) "asm-diff"), sprintf(fmt, disp[[n]]))
      }
      tagList(
        div(class = "asm-detail-head", title,
            actionButton(paste0("asm_edit_", s), "Edit", class = "btn-secondary btn-sm")),
        div(class = "asm-rules",
          field("Species"), field("Top Ø (cm)", "top Ø %s cm"),
          field("Max butt Ø (cm)", "max butt Ø %s cm"), field("Length (cm)", "length %s cm"),
          field("Volume method"), field("Pricing", "%s price"),
          if (hidden_diff) span(class = "asm-rule asm-diff",
                                sprintf("other rules differ from Scenario %s", other(side)))))
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
      d <- defs(side); k <- selected(); i <- match(k, keys_of(d))
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
      rows$PriceLengths_cm <- old_row$PriceLengths_cm   # hidden (hpr imports); kept on edits
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
    selected(new_keys[1L])
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
      k <- selected()
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

    # One assortment (definition and prices) equal to the other scenario's.
    observeEvent(input[[paste0("asm_copy_one_", s)]], {
      o <- other(side); k <- selected()
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
    # A scenario xlsx (with an "assortments" sheet) replaces the scenario;
    # any other xlsx or CSV is a price file for all its assortments.
    observeEvent(input[[paste0("scen_", s, "_upload")]], {
      f <- input[[paste0("scen_", s, "_upload")]]; req(f)
      sheets <- if (grepl("\\.xlsx$", f$name, ignore.case = TRUE))
        tryCatch(readxl::excel_sheets(f$datapath), error = function(e) character(0))
      if (!"assortments" %in% sheets) {
        res <- upload_price_set(f$datapath, f$name, defs(side), prices(side))
        if (!isTRUE(res$ok))
          return(say(side, "error", paste(c(sprintf("%s was not loaded:", f$name), res$errors), collapse = "\n")))
        sc[[side]]$pm(res$pm); redraw()
        return(say(side, "ok", sprintf("Loaded prices from %s: %s.", f$name,
                                       paste(price_file_label(res$ids), collapse = ", ")), res$warnings))
      }
      res <- scenario_import(f$datapath)
      if (!isTRUE(res$ok))
        return(say(side, "error", paste(c(sprintf("%s was not loaded:", f$name), res$errors), collapse = "\n")))
      sc[[side]]$defs(res$defs); sc[[side]]$pm(res$pm); redraw()
      say(side, "ok", sprintf("Loaded %s: %d assortment(s) with prices.", f$name, nrow(res$defs)), res$warnings)
    })
  })

  # ---- import from .hpr (R/helpers_hpr.R; hpr Phase A) ---------------------------
  # A dialog per scenario: read the file, confirm each species group's
  # species (guessed from its name) and the products to import (those
  # without the best grade start unticked), then the file's products and
  # prices replace this scenario's. The other scenario is not changed.
  hpr <- reactiveVal(NULL)    # list(side, data = read_hpr() result, file)
  hpr_err <- reactiveVal(NULL)
  SP_CHOICES <- c("Spruce" = "spruce", "Pine" = "pine", "Not used" = "none")

  hpr_modal <- function(side) modalDialog(
    title = sprintf("Import from .hpr into Scenario %s", side),
    size = "xl", easyClose = FALSE, fade = FALSE,
    p(class = "note-muted", sprintf(paste(
      "The products and price matrices of a StanForD 2010 .hpr file replace Scenario %s's",
      "assortments and prices. Scenario %s is not changed."), side, other(side))),
    fileInput("hpr_file", sprintf("Harvester production file (.hpr, or .hpr in .zip / .gz; up to %d MB)",
                                  HPR_MAX_UPLOAD_MB),
              accept = c(".hpr", ".zip", ".gz"), width = "100%"),
    uiOutput("hpr_check"), uiOutput("hpr_species"), uiOutput("hpr_products"),
    uiOutput("hpr_msg"),
    footer = tagList(modalButton("Cancel"),
                     actionButton("hpr_apply", sprintf("Replace Scenario %s", side),
                                  class = "btn-primary", disabled = TRUE)))

  for (side in c("A", "B")) local({
    side <- side
    observeEvent(input[[paste0("hpr_import_", low(side))]], {
      hpr(list(side = side)); hpr_err(NULL); showModal(hpr_modal(side))
    })
  })
  # The uploaded file is kept for the session (hpr Phase B bucks its stems
  # later): moved out of Shiny's upload folder into a folder of the session's
  # own, one file at a time (a new upload replaces it). When the session ends
  # the folder and the upload folders are deleted.
  hpr_dir  <- file.path(tempdir(), paste0("apter_hpr_", session$token))
  hpr_kept <- reactiveVal(NULL)   # list(name, path, mb)
  hpr_uploads <- character(0)     # Shiny's upload folders of this session
  session$onSessionEnded(function() unlink(c(hpr_dir, hpr_uploads), recursive = TRUE))
  observeEvent(input$hpr_file, {
    f <- input$hpr_file; req(f, hpr())
    hpr_uploads <<- unique(c(hpr_uploads, dirname(f$datapath)))
    unlink(hpr_dir, recursive = TRUE)                   # the previous upload
    dir.create(hpr_dir, showWarnings = FALSE, recursive = TRUE)
    kept <- file.path(hpr_dir, basename(f$name))
    moved <- isTRUE(file.rename(f$datapath, kept)) ||
      (isTRUE(file.copy(f$datapath, kept, overwrite = TRUE)) && isTRUE(file.remove(f$datapath)))
    if (!moved) kept <- f$datapath
    hpr_kept(list(name = f$name, path = kept, mb = round(file.size(kept) / 1048576, 1)))
    data <- withProgress(message = sprintf("Reading %s...", f$name), value = 0.5,
                         read_hpr(kept, f$name))
    hpr(utils::modifyList(hpr(), list(data = data, file = f$name)))
    hpr_err(NULL)
  })
  hpr_data <- reactive({ h <- hpr(); if (!is.null(h$data)) h$data })
  # Test mode only (shinytest2): where the kept file is, so audit 17 can check
  # that it is deleted when the session ends.
  exportTestValues(hpr_kept_path = hpr_kept()$path)
  observe(updateActionButton(session, "hpr_apply", disabled = !isTRUE(hpr_data()$ok)))

  output$hpr_check <- renderUI({
    d <- hpr_data(); req(d)
    tagList(
      if (length(d$errors)) div(class = "pm-msg-error", paste(c("The file cannot be imported:", d$errors),
                                                              collapse = "\n")),
      if (isTRUE(d$ok)) div(class = "hpr-summary", sprintf("%d species groups · %d products with prices",
                                                           nrow(d$species), length(d$products))),
      if (length(d$warnings)) div(class = "hpr-warn", paste("Warning:", d$warnings, collapse = "\n")))
  })

  # Species group key -> "spruce" / "pine" / "none": the dialog's choice,
  # or the guess before the selects exist.
  hpr_map <- reactive({
    d <- hpr_data(); req(isTRUE(d$ok))
    guess <- guess_hpr_species(d$species$name, d$species$user_id)
    setNames(vapply(seq_len(nrow(d$species)), function(i)
      or_default(input[[paste0("hpr_sp_", d$species$key[i])]], guess[i]), ""), d$species$key)
  })

  output$hpr_species <- renderUI({
    d <- hpr_data(); req(isTRUE(d$ok))
    guess <- guess_hpr_species(d$species$name, d$species$user_id)
    tagList(
      div(class = "section-hdr", "Species groups"),
      tags$table(class = "hpr-table",
        tags$thead(tags$tr(tags$th("Key"), tags$th("Species group"), tags$th("User ID"),
                           tags$th("Products"), tags$th("Apter species"))),
        tags$tbody(lapply(seq_len(nrow(d$species)), function(i) tags$tr(
          tags$td(d$species$key[i]), tags$td(d$species$name[i]),
          tags$td(or_default(d$species$user_id[i], "")), tags$td(d$species$n_products[i]),
          tags$td(selectInput(paste0("hpr_sp_", d$species$key[i]), NULL, SP_CHOICES,
                              isolate(or_default(input[[paste0("hpr_sp_", d$species$key[i])]], guess[i])),
                              width = "130px")))))))
  })

  output$hpr_products <- renderUI({
    d <- hpr_data(); req(isTRUE(d$ok))
    tab <- hpr_product_table(d, hpr_map())
    tagList(
      div(class = "section-hdr", "Products"),
      tags$table(class = "hpr-table",
        tags$thead(tags$tr(tags$th("Import"), tags$th("Product"), tags$th("Species group"),
                           tags$th("Species"), tags$th("Top Ø classes"), tags$th("Length classes (cm)"),
                           tags$th("Forbidden cells"), tags$th(""))),
        tags$tbody(lapply(seq_len(nrow(tab)), function(i) tags$tr(
          class = if (!tab$default[i]) "hpr-off",
          tags$td(class = "hpr-tick", if (tab$species[i] == "none") "–"
                  else checkboxInput(paste0("hpr_pr_", tab$key[i]), NULL, tab$default[i], width = "auto")),
          tags$td(tab$name[i]), tags$td(tab$group[i]),
          tags$td(if (tab$species[i] == "none") "Not used" else tools::toTitleCase(tab$species[i])),
          tags$td(class = "hpr-nowrap", tab$diameters[i]), tags$td(class = "hpr-nowrap", tab$lengths[i]),
          tags$td(tab$forbidden[i]), tags$td(class = "note-muted", tab$reason[i]))))))
  })
  output$hpr_msg <- renderUI({
    e <- hpr_err(); if (!is.null(e)) div(class = "pm-msg-error", paste(e, collapse = "\n"))
  })

  observeEvent(input$hpr_apply, {
    h <- hpr(); d <- h$data; req(isTRUE(d$ok)); side <- h$side
    map <- hpr_map()
    tab <- hpr_product_table(d, map)
    keep <- tab$key[tab$species != "none" &
                    vapply(tab$key, function(k) isTRUE(input[[paste0("hpr_pr_", k)]]), logical(1))]
    if (length(keep) == 0L) return(hpr_err("Tick at least one product to import."))
    res <- hpr_to_scenario(d, map, keep, base_defs = APP_DEFS)
    if (!isTRUE(res$ok)) return(hpr_err(c(sprintf("%s was not imported:", h$file), res$errors)))
    if (!commit(side, res$defs, res$pm, err = function(e) hpr_err(e))) return()
    selected(keys_of(res$defs)[asm_order(res$defs)][1L])
    redraw(); removeModal()
    n <- sum(res$defs$Type != "waste")
    say(side, "ok", sprintf("Imported %d assortment%s from %s into Scenario %s.",
                            n, if (n == 1L) "" else "s", h$file, side), res$warnings)
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


  # ---- prices of the selected assortment ---------------------------------------------
  # In the detail panel, directly under the rules: a matrix-priced assortment
  # shows its editable grid with the row/column buttons, a flat-priced one a
  # single price field. Cells (or the price) that differ from the other
  # scenario, or have no counterpart there, are highlighted.
  price_file_label <- function(ids) ifelse(ids == PM_FLAT_ID, "Flat prices", key_label(ids))
  flat_price <- function(side, k) {
    p <- pm_minimal(prices(side))
    v <- p$PriceNOKperM3[p$ProductKey == k & is.na(p$LengthClass_dm)]
    if (length(v)) as.numeric(v[1L]) else NA_real_
  }

  pm_widget <- function(grid, id, def) {
    w <- rhandsontable(grid, rowHeaders = rownames(grid), colHeaders = colnames(grid),
                       rowHeaderWidth = 50, height = min(480, 34 + 24 * nrow(grid)),
                       stretchH = "all", matrix_id = id, def = def)
    w <- hot_cols(w, renderer = PM_DIFF_RENDERER, columnSorting = FALSE)
    w <- hot_context_menu(w, allowRowEdit = FALSE, allowColEdit = FALSE)
    hot_validate_numeric(w, cols = seq_len(ncol(grid)), min = 0)
  }

  # Both scenarios editable; a cell that differs from the other scenario
  # (or has no counterpart there) is highlighted.
  render_grid <- function(side) renderRHandsontable({
    pm_tick(); id <- selected()
    req(id, sel_state(side) == "matrix")
    pm <- isolate(prices(side)); o <- other(side)
    po <- isolate(prices(o)); do <- isolate(defs(o))
    g <- pm_to_grid(pm, id)
    has_other <- id %in% keys_of(do)[do$PriceType == "matrix"] &&
                 any(po$ProductKey == id & !is.na(po$LengthClass_dm))
    # Compared only with the same assortment in the other scenario: if that
    # scenario lacks it, nothing is highlighted (the grid is its own
    # reference); if it prices it flat, every cell differs.
    def <- if (has_other) pm_default_aligned(po, pm, id)
           else if (id %in% keys_of(do)) matrix(NA_real_, nrow(g), ncol(g))
           else as.matrix(g)
    pm_widget(g, id, unname(def))
  })
  output$pm_a <- render_grid("A")
  output$pm_b <- render_grid("B")

  # Grid edits. The matrix id travels with the widget, so a stale grid of
  # the previously shown assortment is never written elsewhere.
  apply_grid_edit <- function(x, side) {
    if (is.null(x$changes$changes)) return()            # load, not an edit
    id <- or_default(x$params$matrix_id, isolate(selected()))
    d <- isolate(defs(side))
    if (!isTRUE(id %in% keys_of(d)[d$PriceType == "matrix"])) return()
    g  <- hot_to_r(x)
    ref <- pm_to_grid(isolate(prices(side)), id)
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

  # Flat price edits arrive as input$flat_edit = {side, key, value}
  # (www/apter.js, on change of the field), so the key travels with the
  # value, as the matrix id does with a grid.
  observeEvent(input$flat_edit, {
    e <- input$flat_edit; side <- toupper(or_default(e$side, "")); k <- e$key
    if (!side %in% c("A", "B")) return()
    v <- suppressWarnings(as.numeric(e$value))
    d <- defs(side); i <- match(k, keys_of(d))
    if (is.na(i) || d$PriceType[i] != "flat" || length(v) != 1L || is.na(v)) return()
    if (v < 0) {
      say(side, "error", "Negative prices are not allowed; the edit was undone.")
      redraw(); return()
    }
    if (isTRUE(all.equal(v, flat_price(side, k)))) return()
    g <- data.frame(PriceNOKperM3 = v); rownames(g) <- k
    sc[[side]]$pm(pm_from_grid(g, PM_FLAT_ID, prices(side))); msgs[[side]](NULL)
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

  # Reset prices: a base assortment priced as by default gets its default
  # rows back (classes and lengths too).
  base_reset_keys <- function(d) {
    k <- keys_of(d); bk <- keys_of(APP_DEFS)
    k[k %in% bk & d$PriceType == APP_DEFS$PriceType[match(k, bk)]]
  }

  for (side in c("A", "B")) local({
    side <- side; s <- low(side); p <- paste0("pm_", s)
    output[[paste0(p, "_axes")]] <- renderUI({
      pm_tick(); id <- selected(); req(id, sel_state(side) == "matrix")
      axes_ui(p, id, isolate(prices(side)))
    })
    edit <- function(f) {
      id <- isolate(selected()); req(id, isolate(sel_state(side)) == "matrix")
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

    # The flat price field, rendered per assortment (data-key: the key its
    # edits are written to). Re-rendered on selection and resets, not on
    # its own edits, so typing keeps the focus.
    output[[paste0("flat_", s)]] <- renderUI({
      pm_tick(); k <- selected(); req(k, sel_state(side) == "flat")
      div(class = "asm-flat-input", `data-side` = side, `data-key` = k,
          numericInput(paste0("flat_price_", s), "Price (NOK/m³ under bark)",
                       isolate(flat_price(side, k)), min = 0, width = "200px"))
    })
    # The other scenario's flat price, highlighted when it differs.
    output[[paste0("flat_other_", s)]] <- renderUI({
      k <- selected(); req(k, sel_state(side) == "flat")
      o <- other(side); mine <- flat_price(side, k)
      theirs <- if (sel_state(o) == "flat") flat_price(o, k) else NA_real_
      # Only an assortment both scenarios have can differ.
      differs <- sel_state(o) != "none" && !isTRUE(all.equal(mine, theirs))
      session$sendCustomMessage("apter_toggle_class",
                                list(id = paste0("flat_wrap_", s), cls = "is-diff", on = differs))
      span(class = paste("asm-flat-other", if (differs) "asm-diff"),
           if (is.na(theirs)) sprintf("No flat price in Scenario %s", o)
           else sprintf("Scenario %s: %s NOK/m³", o, format(theirs, big.mark = ",")))
    })

    # Reset prices of the selected assortment (base assortments priced as by
    # default only; the button is disabled otherwise).
    observe(updateActionButton(session, paste0(p, "_reset"),
                               disabled = !isTRUE(selected() %in% base_reset_keys(defs(side)))))
    observeEvent(input[[paste0(p, "_reset")]], {
      k <- selected()
      if (!isTRUE(k %in% base_reset_keys(defs(side))))
        return(say(side, "error", sprintf("%s has no default prices.", key_label(k))))
      if (commit(side, defs(side), swap_prices(prices(side), k,
                                               APP_PM_DEFAULT[APP_PM_DEFAULT$ProductKey == k, , drop = FALSE]))) {
        redraw(); say(side, "ok", sprintf("Prices of %s reset.", key_label(k)))
      }
    })

    # Upload prices for the selected assortment only: rows for other
    # assortments in the file are ignored (and reported).
    observeEvent(input[[paste0(p, "_upload")]], {
      f <- input[[paste0(p, "_upload")]]; req(f); k <- selected()
      res <- upload_assortment_prices(f$datapath, f$name, defs(side), prices(side), k)
      if (!isTRUE(res$ok))
        return(say(side, "error", paste(c(sprintf("%s was not loaded:", f$name), res$errors), collapse = "\n")))
      sc[[side]]$pm(res$pm); redraw()
      say(side, "ok", paste0(sprintf("Loaded prices of %s from %s.", key_label(k), f$name),
                             if (length(res$ignored))
                               sprintf(" Ignored (other assortments): %s.",
                                       paste(price_file_label(res$ignored), collapse = ", "))),
          res$warnings)
    })
    # Download the selected assortment's prices.
    sel_prices <- function() { pm <- pm_minimal(prices(side)); pm[pm$ProductKey == selected(), , drop = FALSE] }
    output[[paste0("dl_", s, "_wide")]] <- downloadHandler(
      filename = function() sprintf("optBuck_prices_%s_%s.xlsx", side, selected()),
      content = function(file) pm_template_wide(sel_prices(), file))
    output[[paste0("dl_", s, "_long")]] <- downloadHandler(
      filename = function() sprintf("optBuck_prices_%s_%s.csv", side, selected()),
      content = function(file) pm_export(sel_prices(), file))
  })

  list(defs_a = sc$A$defs, pm_a = sc$A$pm, defs_b = sc$B$defs, pm_b = sc$B$pm,
       differ = differ, pm_state = pm_state, key_label = key_label,
       hpr_file = hpr_kept)   # the session's uploaded .hpr (hpr Phase B)
}
