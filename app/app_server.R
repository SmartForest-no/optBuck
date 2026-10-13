# app/app_server.R
#
# Purpose : Shiny server function (`server`). Sourced by app.R after setup.R
#           and the app helpers (helpers_ui.R, helpers_validation.R,
#           helpers_upload.R, helpers_jobs.R). Styling via CSS classes
#           (www/apter.css) and APTER_CHART colours (theme.R); no inline colours.
#           Not named server.R: Shiny/shinytest2 reject ui.R/server.R next to app.R.
#
# State   : (Phase 4c) one assortment table for all products (asm_defs()),
#           price set A (pm_a()) and B: B's own prices (pm_b()) and a state
#           per product (b_state(): same / own / none; R/helpers_price_matrix.R).
#           Internal names: "default" and "scenario" in the backend mean A and B.

# ---- server -----------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Sidebar: NFI selection ----------------------------------------------

  # Bonitet choices: values available for the selected HKL group and county.
  output$abonitet_ui <- renderUI({
    hkl_raw    <- unlist(HKL_MAP[input$hkl_group])
    fyl        <- selected_fylnr(input$fylnr_sel)
    sub <- filter_stratum_plots(plots_prod, hkl_raw, fyl = fyl)
    abon_avail <- as.character(sort(unique(as.integer(sub$abonitet))))
    if (length(abon_avail) == 0L)
      abon_avail <- as.character(sort(unique(as.integer(plots_prod$abonitet))))
    lbl <- BONITET_LABELS[abon_avail]
    lbl[is.na(lbl)] <- abon_avail[is.na(lbl)]
    choices_named <- setNames(abon_avail, lbl)
    pref <- intersect(c("14", "11", "17"), abon_avail)
    sel  <- if (length(pref) > 0L) pref else abon_avail[1L]
    checkboxGroupInput("abonitet", "Site class (bonitet)",
                       choiceNames  = unname(choices_named),
                       choiceValues = abon_avail,
                       selected     = sel)
  })

  # Only real errors: an empty selection or no matching plots.
  output$stratum_error <- renderUI({
    hkl_raw <- unlist(HKL_MAP[input$hkl_group])
    abon    <- as.integer(input$abonitet)
    if (length(hkl_raw) == 0L || length(abon) == 0L)
      return(div(class = "note-error", "Select at least one maturity class and one site class."))
    if (count_eligible_plots(plots_prod, hkl_raw, abon, selected_fylnr(input$fylnr_sel)) == 0L)
      div(class = "note-error", "No plots match this selection.")
  })

  # ---- Uploaded tree list (Phase 5; R/helpers_tree_list.R) -----------------
  tl_prepared <- reactiveVal(NULL)   # prepare_tree_list() result + $file

  observeEvent(input$tl_upload, {
    f <- input$tl_upload; req(f)
    rd <- read_tree_list(f$datapath, f$name)
    p <- if (length(rd$errors) > 0L)
      list(ok = FALSE, errors = rd$errors, warnings = character(0))
    else withProgress(message = "Checking the tree list and imputing heights...", value = 0.5,
                      prepare_tree_list(rd$data))
    p$file <- f$name
    tl_prepared(p)
  })

  output$tl_validation <- renderUI({
    p <- tl_prepared()
    if (is.null(p)) return(NULL)
    h  <- p$heights
    ex <- if (!is.null(p$trees)) p$trees[!p$trees$in_range, , drop = FALSE]
    tagList(
      div(class = if (isTRUE(p$ok)) "pm-msg-ok" else "pm-msg-error",
          if (isTRUE(p$ok))
            sprintf("%s: %d trees, %d within taperNOR's range.", p$file, nrow(p$trees),
                    sum(p$trees$in_range))
          else paste(c(sprintf("%s cannot be used:", p$file), p$errors), collapse = "\n")),
      if (length(p$warnings) > 0L)
        div(class = "pm-msg-warn", paste("Warning:", p$warnings, collapse = "\n")),
      if (!is.null(ex) && nrow(ex) > 0L)
        tags$details(class = "tl-excluded",
          tags$summary(sprintf("%d tree(s) not bucked", nrow(ex))),
          tags$ul(lapply(seq_len(nrow(ex)), function(i)
            tags$li(sprintf("%s: %s", ex$treid[i], ex$excluded_reason[i]))))),
      if (!is.null(h) && nrow(h) > 0L)
        div(class = "tl-heights",
            div(class = "section-hdr", "Heights"),
            tags$ul(lapply(seq_len(nrow(h)), function(i) tags$li(sprintf(
              "%s: %d trees, %d measured; %s%s", h$species[i], h$n_trees[i], h$n_measured[i],
              if (h$method[i] == "fieldPlot") sprintf("fieldPlot (%s)", h$model[i]) else h$method[i],
              if (!is.na(h$ratio[i])) sprintf(", ratio %.2f", h$ratio[i]) else "")))))
    )
  })

  output$dl_tree_template <- downloadHandler(
    filename = function() "apter_tree_list_template.csv",
    content  = function(file) tree_list_template(file))


  # ---- Assortments and price sets A and B ----------------------------------
  asm_defs <- reactiveVal(APP_DEFS)
  pm_a     <- reactiveVal(APP_PM_DEFAULT)
  pm_b     <- reactiveVal(APP_PM_DEFAULT)          # B's own prices (all products)
  b_state  <- reactiveVal(setNames(rep("same", nrow(APP_DEFS)), assortment_table_keys(APP_DEFS)))
  pm_tick  <- reactiveVal(0L)     # re-render the grids
  pm_msg   <- reactiveVal(NULL)   # last price upload / reset / edit message
  asm_msg  <- reactiveVal(NULL)   # last assortment add / edit / upload message
  redraw   <- function() pm_tick(isolate(pm_tick()) + 1L)

  # Everything that changes a run; a run stores it to detect stale results.
  pm_state  <- reactive(list(asm_defs(), pm_a(), pm_b(), b_state()))
  b_now     <- reactive(b_set(asm_defs(), pm_a(), pm_b(), b_state()))
  b_differ  <- reactive(b_differs(pm_a(), pm_b(), b_state()))

  key_label <- function(keys) {
    d <- asm_defs(); k <- assortment_table_keys(d)
    i <- match(keys, k)
    ifelse(is.na(i), keys, sprintf("%s %s", tools::toTitleCase(d$Species[i]), tolower(d$Name[i])))
  }

  # ---- assortment table ----
  dash <- function(x) ifelse(is.finite(x), format(x, trim = TRUE), "–")
  output$tbl_assortments <- renderReactable({
    d <- asm_defs()
    p <- products_from_defs(d, pm_a())$pd
    apter_reactable(data.frame(
      Assortment = d$Name, Type = d$Type, Species = d$Species,
      `Min top diameter (cm)` = dash(d$MinTopDiameter_cm),
      `Max top diameter (cm)` = dash(d$MaxTopDiameter_cm),
      `Max butt diameter (cm)` = dash(d$MaxButtDiameter_cm),
      `Min length (cm)` = dash(p$MinLength), `Max length (cm)` = dash(p$MaxLength),
      `Length step (cm)` = dash(p$LengthStep),
      `Volume method` = d$VolumeDiameterCategory,
      `Diameter rounding` = ifelse(d$VolumeDiameterAdjustment == VOLUME_ROUND_DOWN_CM,
                                   "Down to cm", "None"),
      check.names = FALSE, stringsAsFactors = FALSE), page_size = 12L)
  })

  observeEvent(asm_defs(), {
    k <- assortment_table_keys(asm_defs()); sel <- isolate(input$asm_pick)
    updateSelectInput(session, "asm_pick", choices = setNames(k, key_label(k)),
                      selected = if (isTRUE(sel %in% k)) sel else k[1L])
  })

  asm_editing  <- reactiveVal(NULL)   # ProductKey being edited; NULL = new
  asm_form_err <- reactiveVal(NULL)
  output$asm_form_msg <- renderUI({
    e <- asm_form_err(); if (is.null(e)) return(NULL)
    div(class = "pm-msg-error", paste(e, collapse = "\n"))
  })

  asm_modal <- function(d = NULL) {
    new <- is.null(d)
    v <- function(n, default) {
      x <- if (new) NULL else d[[n]]
      if (is.null(x) || length(x) == 0L || is.na(x)) default else x
    }
    fin <- function(n) { x <- v(n, NA); if (is.finite(x)) x else NA }
    sp <- c("Spruce" = "spruce", "Pine" = "pine")
    modalDialog(
      title = if (new) "Add assortment" else paste("Edit", d$Name, "–", d$Species),
      size = "l", easyClose = FALSE,
      layout_columns(col_widths = c(4, 4, 4),
        textInput("asm_name", "Assortment", v("Name", "")),
        textInput("asm_type", "Type", v("Type", "")),
        selectInput("asm_species", "Species",
                    if (new) c("Spruce and pine" = "both", sp) else sp,
                    v("Species", if (new) "both" else "spruce")),
        numericInput("asm_min_top", "Min top diameter (cm)", v("MinTopDiameter_cm", 0), min = 0),
        numericInput("asm_max_top", "Max top diameter (cm; empty = no limit)",
                     fin("MaxTopDiameter_cm"), min = 0),
        numericInput("asm_min_butt", "Min butt diameter (cm)", v("MinButtDiameter_cm", 0), min = 0),
        numericInput("asm_max_butt", "Max butt diameter (cm; empty = no limit)",
                     fin("MaxButtDiameter_cm"), min = 0),
        radioButtons("asm_price_type", "Pricing",
                     c("Price matrix" = "matrix", "Flat price" = "flat"), v("PriceType", "matrix")),
        numericInput("asm_dtp", "Top diameter read (cm below the cut)",
                     v("DiameterTopPosition_cm", 0), min = 0, step = 10),
        selectInput("asm_method", "Volume method", VOLUME_METHODS,
                    v("VolumeDiameterCategory", "Top cylinder")),
        selectInput("asm_adjust", "Diameter rounding",
                    c("None" = "None", "Down to cm" = VOLUME_ROUND_DOWN_CM),
                    v("VolumeDiameterAdjustment", "None"))
      ),
      # Lengths: a flat price uses min..max by step; a matrix's lengths are
      # its columns (set here only for a new matrix).
      conditionalPanel(
        if (new || !identical(d$PriceType, "matrix")) "true" else "input.asm_price_type == 'flat'",
        layout_columns(col_widths = c(4, 4, 4),
          numericInput("asm_min_len", "Min length (cm)", v("MinLength_cm", 310), min = 10, step = 10),
          numericInput("asm_max_len", "Max length (cm)", v("MaxLength_cm", 550), min = 10, step = 10),
          numericInput("asm_step", "Length step (cm)", v("LengthStep_cm", 30), min = 10, step = 10))),
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

  observeEvent(input$asm_add, {
    asm_editing(NULL); asm_form_err(NULL); showModal(asm_modal())
  })
  observeEvent(input$asm_edit, {
    d <- asm_defs(); i <- match(input$asm_pick, assortment_table_keys(d)); req(!is.na(i))
    asm_editing(input$asm_pick); asm_form_err(NULL); showModal(asm_modal(d[i, , drop = FALSE]))
  })

  # Replace the prices of `keys` in a matrix by `new` (NULL: remove them).
  swap_prices <- function(pm, keys, new = NULL)
    pm_minimal(rbind(pm[!pm$ProductKey %in% keys, , drop = FALSE], new))

  observeEvent(input$asm_save, {
    num <- function(x) if (is.null(x) || length(x) == 0L || is.na(x)) NA_real_ else as.numeric(x)
    or  <- function(x, y) if (is.na(x)) y else x
    name <- trimws(or_default(input$asm_name, ""))
    type <- trimws(or_default(input$asm_type, ""))
    species <- if (identical(input$asm_species, "both")) PRODUCT_SPECIES else input$asm_species
    ptype <- input$asm_price_type
    rows <- .type_assortment_table(data.frame(
      Name = name, Type = if (nzchar(type)) type else assortment_slug(name), Species = species,
      MinTopDiameter_cm = or(num(input$asm_min_top), 0),
      MaxTopDiameter_cm = or(num(input$asm_max_top), Inf),
      MinButtDiameter_cm = or(num(input$asm_min_butt), 0),
      MaxButtDiameter_cm = or(num(input$asm_max_butt), Inf),
      MinLength_cm = num(input$asm_min_len), MaxLength_cm = num(input$asm_max_len),
      LengthStep_cm = num(input$asm_step),
      DiameterTopPosition_cm = or(num(input$asm_dtp), 0),
      VolumeDiameterCategory = input$asm_method,
      VolumeDiameterAdjustment = input$asm_adjust, PriceType = ptype,
      stringsAsFactors = FALSE))
    new_keys <- assortment_table_keys(rows)

    d <- asm_defs(); keys <- assortment_table_keys(d)
    a <- pm_a(); b <- pm_b(); st <- b_state()
    old <- asm_editing()
    i_old <- if (is.null(old)) NA_integer_ else match(old, keys)
    same_pricing <- !is.na(i_old) && identical(d$PriceType[i_old], ptype)
    if (same_pricing && ptype == "matrix") {   # lengths stay the matrix columns
      rows$MinLength_cm <- d$MinLength_cm[i_old]; rows$MaxLength_cm <- d$MaxLength_cm[i_old]
      rows$LengthStep_cm <- d$LengthStep_cm[i_old]
    }

    errs <- character(0); start <- NULL
    taken <- intersect(new_keys, setdiff(keys, old))
    if (length(taken) > 0L)
      errs <- c(errs, sprintf("%s already exists.", paste(key_label(taken), collapse = " and ")))
    if (!same_pricing) {
      price <- num(input$asm_price)
      if (is.na(price) || price < 0) errs <- c(errs, "Give a starting price of 0 NOK/m³ or more.")
      lens <- if (ptype == "matrix" && !anyNA(c(rows$MinLength_cm[1], rows$MaxLength_cm[1], rows$LengthStep_cm[1])) &&
                  rows$LengthStep_cm[1] > 0 && rows$MaxLength_cm[1] >= rows$MinLength_cm[1])
        seq(rows$MinLength_cm[1], rows$MaxLength_cm[1], by = rows$LengthStep_cm[1]) else numeric(0)
      hi <- if (is.finite(rows$MaxTopDiameter_cm[1])) floor(rows$MaxTopDiameter_cm[1]) else num(input$asm_max_class)
      if (length(errs) == 0L)
        start <- tryCatch(do.call(rbind, lapply(new_keys, function(k)
          pm_start_prices(k, ptype, price, max(1, floor(rows$MinTopDiameter_cm[1])), hi, lens))),
          error = function(e) { errs <<- c(errs, conditionMessage(e)); NULL })
    }
    if (length(errs) > 0L) { asm_form_err(errs); return() }

    # The new table and prices. An edit keeps the row's place and, with the
    # same pricing, its prices on both sides (moved to the new key).
    if (is.na(i_old)) {
      d2 <- rbind(d, rows)
      a2 <- swap_prices(a, new_keys, start); b2 <- swap_prices(b, new_keys, start)
      st2 <- c(st[setdiff(names(st), new_keys)], setNames(rep("same", length(new_keys)), new_keys))
    } else {
      d2 <- d; d2[i_old, ] <- rows
      k1 <- new_keys
      if (same_pricing) {
        a2 <- pm_rename_key(swap_prices(a, setdiff(k1, old)), old, k1)
        b2 <- pm_rename_key(swap_prices(b, setdiff(k1, old)), old, k1)
        st2 <- st; names(st2)[names(st2) == old] <- k1
      } else {
        a2 <- swap_prices(a, c(old, k1), start); b2 <- swap_prices(b, c(old, k1), start)
        st2 <- st; names(st2)[names(st2) == old] <- k1; st2[k1] <- "same"
      }
    }
    st2 <- st2[assortment_table_keys(d2)]
    v <- check_price_set(d2, a2)
    if (v$ok) {
      bb <- b_set(d2, a2, b2, st2)
      vb <- check_price_set(bb$defs, bb$pm)
      if (!vb$ok) v <- list(ok = FALSE, errors = paste("B:", vb$errors))
    }
    if (!v$ok) { asm_form_err(v$errors); return() }

    asm_defs(d2); pm_a(a2); pm_b(b2); b_state(st2)
    redraw(); removeModal()
    asm_msg(list(type = "ok", warnings = v$warnings,
                 text = sprintf("%s %s.", if (is.na(i_old)) "Added" else "Saved",
                                paste(key_label(new_keys), collapse = " and "))))
  })

  observeEvent(input$asm_remove, {
    d <- asm_defs(); k <- input$asm_pick; i <- match(k, assortment_table_keys(d)); req(!is.na(i))
    if (nrow(d) == 1L) {
      asm_msg(list(type = "error", text = "The last assortment cannot be removed."))
      return()
    }
    lbl <- key_label(k)
    asm_defs(d[-i, , drop = FALSE])
    pm_a(swap_prices(pm_a(), k)); pm_b(swap_prices(pm_b(), k))
    st <- b_state(); b_state(st[names(st) != k])
    redraw()
    asm_msg(list(type = "ok", text = sprintf("Removed %s.", lbl)))
  })

  # Base assortments back to their defaults (definitions, 2021 prices in A,
  # "Same as A" in B); custom assortments stay.
  observeEvent(input$asm_reset, {
    base <- assortment_table_keys(APP_DEFS)
    d <- asm_defs(); custom <- d[!assortment_table_keys(d) %in% base, , drop = FALSE]
    d2 <- rbind(APP_DEFS, custom)
    def_rows <- APP_PM_DEFAULT[APP_PM_DEFAULT$ProductKey %in% base, , drop = FALSE]
    pm_a(swap_prices(pm_a(), base, def_rows)); pm_b(swap_prices(pm_b(), base, def_rows))
    st <- b_state(); st[base] <- "same"
    asm_defs(d2); b_state(st[assortment_table_keys(d2)])
    redraw()
    asm_msg(list(type = "ok", text = "Base assortments reset to the defaults; custom assortments kept."))
  })

  output$dl_assortments <- downloadHandler(
    filename = function() "apter_assortments.xlsx",
    content  = function(file) assortment_table_export(asm_defs(), pm_a(), pm_b(), b_state(), file))

  observeEvent(input$asm_upload, {
    f <- input$asm_upload; req(f)
    res <- assortment_table_import(f$datapath)
    if (isTRUE(res$ok)) {
      asm_defs(res$defs); pm_a(res$pm_a); pm_b(res$pm_b); b_state(res$state)
      redraw()
      asm_msg(list(type = "ok", warnings = res$warnings, text = sprintf(
        "Loaded %s: %d assortment(s) with prices A and B.", f$name, nrow(res$defs))))
    } else {
      asm_msg(list(type = "error", text = paste(
        c(sprintf("%s was not loaded:", f$name), res$errors), collapse = "\n")))
    }
  })

  msg_ui <- function(m) {
    if (is.null(m)) return(NULL)
    tagList(
      div(class = if (identical(m$type, "error")) "pm-msg-error" else "pm-msg-ok", m$text),
      if (length(m$warnings) > 0L)
        div(class = "pm-msg-warn", paste("Warning:", m$warnings, collapse = "\n")))
  }
  output$asm_msg <- renderUI(msg_ui(asm_msg()))
  output$pm_upload_msg <- renderUI(msg_ui(pm_msg()))


  # ---- price matrices ----
  # Matrix choices: matrix-priced products in table order, then the flat table.
  pm_ids <- reactive({
    d <- asm_defs(); k <- assortment_table_keys(d)
    c(k[d$PriceType == "matrix"], if (any(d$PriceType == "flat")) PM_FLAT_ID)
  })
  observeEvent(pm_ids(), {
    ids <- pm_ids(); sel <- isolate(input$pm_id)
    lbl <- ifelse(ids == PM_FLAT_ID, "Flat prices", key_label(ids))
    updateSelectInput(session, "pm_id", choices = setNames(ids, lbl),
                      selected = if (isTRUE(sel %in% ids)) sel else ids[1L])
  })
  is_flat <- function(id) identical(id, PM_FLAT_ID)
  flat_keys <- reactive({ d <- asm_defs(); assortment_table_keys(d)[d$PriceType == "flat"] })

  pm_widget <- function(grid, id, read_only, renderer = NULL, def = NULL) {
    w <- rhandsontable(grid, rowHeaders = rownames(grid), readOnly = read_only,
                       height = 520, stretchH = "all", matrix_id = id, def = def)
    w <- hot_cols(w, renderer = renderer, columnSorting = FALSE)
    w <- hot_context_menu(w, allowRowEdit = FALSE, allowColEdit = FALSE)
    if (!read_only) w <- hot_validate_numeric(w, cols = seq_len(ncol(grid)), min = 0)
    w
  }

  # A: every matrix editable.
  output$pm_a <- renderRHandsontable({
    pm_tick(); id <- input$pm_id; req(id, id %in% pm_ids())
    a <- isolate(pm_a())
    g <- if (is_flat(id)) {
      f <- pm_to_grid(a, PM_FLAT_ID); f[rownames(f) %in% isolate(flat_keys()), , drop = FALSE]
    } else pm_to_grid(a, id)
    pm_widget(g, id, FALSE, PM_PLAIN_RENDERER)
  })

  # B: "Same as A" shows A greyed and read-only, "Own prices" B's own
  # prices (cells that differ from A highlighted), "Not used" no grid. The
  # flat table carries the state per product in its first column.
  output$pm_b <- renderRHandsontable({
    pm_tick(); id <- input$pm_id; req(id, id %in% pm_ids())
    a <- isolate(pm_a()); b <- isolate(pm_b()); st <- isolate(b_state())
    if (is_flat(id)) {
      k <- isolate(flat_keys())
      pa <- pm_to_grid(a, PM_FLAT_ID); pb <- pm_to_grid(b, PM_FLAT_ID)
      s  <- st[k]; s[is.na(s)] <- "same"
      price <- ifelse(s == "own", pb[k, 1], ifelse(s == "same", pa[k, 1], NA_real_))
      g <- data.frame(B = unname(B_STATES[s]), PriceNOKperM3 = price, row.names = k,
                      stringsAsFactors = FALSE)
      w <- rhandsontable(g, rowHeaders = k, height = 520, stretchH = "all", matrix_id = id,
                         def = unname(cbind(NA, pa[k, 1])))
      w <- hot_col(w, "B", type = "dropdown", source = unname(B_STATES), strict = TRUE,
                   allowInvalid = FALSE)
      w <- hot_col(w, "PriceNOKperM3", renderer = PM_DIFF_RENDERER)
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

  # The B grid is hidden when the shown matrix is "Not used" in B.
  output$pm_b_shown <- reactive({
    id <- input$pm_id
    is.null(id) || is_flat(id) || !identical(unname(b_state()[id]), "none")
  })
  outputOptions(output, "pm_b_shown", suspendWhenHidden = FALSE)

  output$pm_b_state_ui <- renderUI({
    pm_tick(); id <- input$pm_id; req(id, !is_flat(id))   # follows uploads and resets
    selectInput("pm_b_state", NULL, setNames(names(B_STATES), B_STATES),
                selected = or_default(isolate(b_state())[id], "same"), width = "220px")
  })
  observeEvent(input$pm_b_state, {
    id <- input$pm_id; req(id, !is_flat(id))
    st <- b_state(); new <- input$pm_b_state
    if (identical(unname(st[id]), new)) return()
    if (new == "own") pm_b(pm_b_copy_a(pm_a(), pm_b(), id))
    st[id] <- new; b_state(st); redraw()
  })
  output$pm_b_note <- renderUI({
    id <- input$pm_id; req(id, !is_flat(id))
    if (identical(unname(b_state()[id]), "none")) div(class = "note-muted", "Not used in B.")
  })
  output$pm_a_note <- renderUI(NULL)

  # Grid edits. The matrix id travels with the widget, so a stale grid of
  # the previously shown matrix is never written elsewhere.
  apply_grid_edit <- function(x, pmv) {
    if (is.null(x$changes$changes)) return()            # load, not an edit
    id <- or_default(x$params$matrix_id, isolate(input$pm_id))
    g  <- hot_to_r(x)
    ref <- pm_to_grid(isolate(pmv()), id)
    if (is_flat(id)) ref <- ref[rownames(ref) %in% rownames(g), , drop = FALSE]
    if (!identical(dim(g), dim(ref))) return()
    rownames(g) <- rownames(ref); colnames(g) <- colnames(ref)
    if (any(as.matrix(g) < 0, na.rm = TRUE)) {
      pm_msg(list(type = "error", text = "Negative prices are not allowed; the edit was undone."))
      redraw(); return()
    }
    pmv(pm_from_grid(g, id, isolate(pmv())))
  }
  observeEvent(input$pm_a, apply_grid_edit(input$pm_a, pm_a))
  observeEvent(input$pm_b, {
    x <- input$pm_b
    if (is.null(x$changes$changes)) return()
    id <- or_default(x$params$matrix_id, isolate(input$pm_id))
    if (!is_flat(id)) {
      if (identical(unname(isolate(b_state())[id]), "own")) apply_grid_edit(x, pm_b)
      return()
    }
    g  <- hot_to_r(x); k <- isolate(flat_keys())
    if (nrow(g) != length(k)) return()
    st <- isolate(b_state()); b <- isolate(pm_b()); a <- isolate(pm_a())
    new_st <- names(B_STATES)[match(g$B, B_STATES)]
    changed <- FALSE
    for (i in seq_along(k)) {
      if (is.na(new_st[i])) next
      if (!identical(unname(st[k[i]]), new_st[i])) {
        if (new_st[i] == "own") b <- pm_b_copy_a(a, b, k[i])
        st[k[i]] <- new_st[i]; changed <- TRUE
      } else if (new_st[i] == "own") {
        p <- as.numeric(g$PriceNOKperM3[i])
        if (!is.na(p) && p < 0) {
          pm_msg(list(type = "error", text = "Negative prices are not allowed; the edit was undone."))
          redraw(); return()
        }
        j <- b$ProductKey == k[i] & is.na(b$LengthClass_dm)
        if (!is.na(p) && any(j) && abs(b$PriceNOKperM3[j] - p) > 1e-9) b$PriceNOKperM3[j] <- p
      }
    }
    pm_b(b); b_state(st)
    if (changed) redraw()
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
    pm_tick(); id <- input$pm_id; req(id, !is_flat(id), id %in% pm_ids())
    axes_ui("pm_a", id, isolate(pm_a()))
  })
  output$pm_b_axes <- renderUI({
    pm_tick(); id <- input$pm_id; req(id, !is_flat(id), id %in% pm_ids())
    req(identical(unname(isolate(b_state())[id]), "own"))
    axes_ui("pm_b", id, isolate(pm_b()))
  })
  axes_observers <- function(s, pmv) {
    edit <- function(f) {
      id <- isolate(input$pm_id); req(id, !is_flat(id))
      res <- tryCatch(f(isolate(pmv()), id), error = function(e) e)
      if (inherits(res, "error"))
        return(pm_msg(list(type = "error", text = conditionMessage(res))))
      pmv(res); pm_msg(NULL); redraw()
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
  axes_observers("pm_a", pm_a)
  axes_observers("pm_b", pm_b)

  # Reset to 2021 prices: the base products' 2021 rows (classes and lengths
  # too). In B, a base product equal to A goes back to "Same as A",
  # otherwise it gets the 2021 prices as its own.
  base_reset_keys <- function() {
    d <- asm_defs(); k <- assortment_table_keys(d)
    bk <- assortment_table_keys(APP_DEFS)
    k[k %in% bk & d$PriceType == APP_DEFS$PriceType[match(k, bk)]]
  }
  observeEvent(input$pm_a_reset, {
    k <- base_reset_keys()
    pm_a(swap_prices(pm_a(), k, APP_PM_DEFAULT[APP_PM_DEFAULT$ProductKey %in% k, , drop = FALSE]))
    redraw()
    pm_msg(list(type = "ok", text = "A: base assortments reset to the 2021 prices."))
  })
  observeEvent(input$pm_b_reset, {
    k <- base_reset_keys(); st <- b_state(); a <- pm_minimal(pm_a())
    def <- APP_PM_DEFAULT[APP_PM_DEFAULT$ProductKey %in% k, , drop = FALSE]
    pm_b(swap_prices(pm_b(), k, def))
    for (key in k) {
      same <- isTRUE(all.equal(a[a$ProductKey == key, ], pm_minimal(def[def$ProductKey == key, ]),
                               check.attributes = FALSE))
      st[key] <- if (same) "same" else "own"
    }
    b_state(st); redraw()
    pm_msg(list(type = "ok", text = "B: base assortments reset to the 2021 prices."))
  })

  # Uploads (helpers_upload.R).
  do_upload <- function(side, f) {
    res <- upload_price_set(f$datapath, f$name, side, asm_defs(), pm_a(), pm_b(), b_state())
    if (isTRUE(res$ok)) {
      if (side == "A") pm_a(res$pm) else { pm_b(res$pm); b_state(res$state) }
      redraw()
      pm_msg(list(type = "ok", warnings = res$warnings, text = sprintf(
        "%s: loaded %s (%s layout): %s.", side, f$name, res$layout,
        paste(ifelse(res$ids == PM_FLAT_ID, "flat prices", key_label(res$ids)), collapse = ", "))))
    } else {
      pm_msg(list(type = "error", text = paste(
        c(sprintf("%s: %s was not loaded:", side, f$name), res$errors), collapse = "\n")))
    }
  }
  observeEvent(input$pm_a_upload, { req(input$pm_a_upload); do_upload("A", input$pm_a_upload) })
  observeEvent(input$pm_b_upload, { req(input$pm_b_upload); do_upload("B", input$pm_b_upload) })

  side_pm <- function(side) if (side == "A") pm_a() else b_now()$pm
  output$dl_a_wide <- downloadHandler(filename = function() "apter_prices_A.xlsx",
                                      content = function(file) pm_template_wide(side_pm("A"), file))
  output$dl_a_long <- downloadHandler(filename = function() "apter_prices_A.csv",
                                      content = function(file) pm_export(side_pm("A"), file))
  output$dl_b_wide <- downloadHandler(filename = function() "apter_prices_B.xlsx",
                                      content = function(file) pm_template_wide(side_pm("B"), file))
  output$dl_b_long <- downloadHandler(filename = function() "apter_prices_B.csv",
                                      content = function(file) pm_export(side_pm("B"), file))

  output$pm_status <- renderUI({
    div(class = "pm-status", if (b_differ()) tags$strong("B differs from A: runs compare A and B.")
                             else "B equals A: runs use A only.")
  })


  # ---- Runs as background tasks (helpers_jobs.R) ---------------------------
  # The backend runs on the workers (R/helpers_parallel.R) as an
  # ExtendedTask: the app stays responsive, the run status shows progress
  # and Run is disabled while a task is running.
  run_progress <- reactiveVal(NULL)
  progress_cb  <- function(done, total, what)
    run_progress(list(done = done, total = total, what = what))

  run_task <- ExtendedTask$new(function(spec, extra) {
    p <- promises::then(run_job_async(spec, progress_cb), function(res) {
      for (n in names(extra)) res[[n]] <- extra[[n]]
      res
    })
    p <- promises::catch(p, function(e) {
      showNotification(paste0("Backend error: ", conditionMessage(e)), type = "error",
                       duration = NULL, session = session)
      NULL
    })
    promises::finally(p, function() unlink(spec$files))
  })
  task_busy <- reactive(run_task$status() == "running")
  observe(updateActionButton(session, "run_btn", disabled = task_busy()))

  # Run: price set A, and B on the same trees when B differs from A (one
  # candidate pass for both; the backend's `scenario`).
  run_rejected <- reactiveVal(FALSE)
  observeEvent(input$run_btn, {
    reject <- function(msg) {
      showNotification(msg, type = "error", duration = 10); run_rejected(TRUE)
    }
    src <- if (identical(input$tree_source, "list")) "list" else "nfi"
    tl  <- if (src == "list") tl_prepared() else NULL
    if (src == "list" && !isTRUE(tl$ok))
      return(reject("Upload a tree list that passes the checks first."))
    if (src == "nfi") req(length(input$hkl_group) > 0L, length(input$abonitet) > 0L)

    d <- asm_defs(); a <- pm_minimal(pm_a())
    va <- check_price_set(d, a)
    if (!va$ok) return(reject(paste(c("Price set A cannot be used:", va$errors), collapse = "\n")))
    compare <- b_differ(); b <- NULL
    if (compare) {
      b <- b_now()
      vb <- check_price_set(b$defs, b$pm)
      if (!vb$ok) return(reject(paste(c("Price set B cannot be used:", vb$errors), collapse = "\n")))
    }
    run_rejected(FALSE)

    # A with the default assortments and 2021 prices runs exactly as before
    # Phase 4c (the default price file and products).
    files <- character(0); path <- "default_2021"; pd <- NULL; lc <- NULL
    if (!(identical(d, APP_DEFS) && identical(a, APP_PM_DEFAULT))) {
      path <- write_price_file(a); files <- path
      p <- products_from_defs(d, a); pd <- p$pd; lc <- p$lc
    }
    scenario <- NULL
    if (compare) {
      path_b <- write_price_file(b$pm); files <- c(files, path_b)
      pb <- products_from_defs(b$defs, b$pm)
      scenario <- list(price_matrix_path = path_b, products = pb$pd, length_classes = pb$lc)
    }
    spec <- list(src = src, params = if (src == "nfi") build_run_params(input) else list(),
                 tl = tl, list_params = list(file = tl$file), path = path, pd = pd, lc = lc,
                 scenario = scenario, files = files)
    run_progress(NULL)
    run_task$invoke(spec, list(price_matrix = a, price_matrix_b = if (compare) b$pm,
                               state = pm_state()))
  })

  result <- reactive({
    if (run_rejected()) return(NULL)
    run_task$result()
  })
  has_result <- reactive(input$run_btn > 0L && !task_busy() && !is.null(result()))

  # B's run (the backend's $scenario), with its prices.
  result_b <- reactive({
    req(has_result()); r <- result(); req(!is.null(r$scenario))
    s <- r$scenario; s$price_matrix <- r$price_matrix_b
    s
  })
  # Shown when the last finished run compared A and B. While a run is busy
  # the result is pending and this observer waits, so the tab stays as it
  # is (hiding the open tab would leave its outputs stale).
  observe({
    show <- input$run_btn > 0L && !is.null(result()) && !is.null(result()$scenario)
    if (show) return(nav_show("result_tabs", "comparison"))
    if (identical(isolate(input$result_tabs), "comparison")) nav_select("result_tabs", "summary")
    nav_hide("result_tabs", "comparison")
  })

  output$run_status <- renderUI({
    if (!task_busy()) return(NULL)
    p <- run_progress()
    if (is.null(p)) return(div(class = "status-running", "Starting run..."))
    known <- !is.na(p$total) && p$total > 0L
    div(class = "status-running",
        if (known) sprintf("%s: %d / %d", p$what, p$done, p$total) else p$what,
        if (known) div(class = "run-progress",
                       div(class = "run-progress-bar",
                           style = sprintf("width: %.0f%%", 100 * p$done / p$total))))
  })

  output$stale_notice <- renderUI({
    req(has_result())
    if (!identical(pm_state(), result()$state))
      div(class = "stale-notice", "Prices or assortments changed since this run.")
  })

  # Heights from reference curves (tree lists): shown above all results.
  output$height_banner <- renderUI({
    if (!has_result() || !identical(result()$tree_source, "tree_list")) return(NULL)
    w <- result()$height_warnings
    w <- w[grepl("FinnHoyde", w, fixed = TRUE)]
    if (length(w) == 0L) return(NULL)
    div(class = "height-notice", tags$strong("Heights: "),
        tagList(lapply(w, function(x) div(x))))
  })


  # ---- Summary ----------------------------------------------------------------
  fmt_int <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
  output$summary_cards <- renderUI({
    if (input$run_btn == 0L) return(div(class = "note-muted", "No run yet."))
    req(has_result()); r <- result()
    tot <- r$per_m3_summary[r$per_m3_summary$scope == "Total", ]
    ro_field_row(
      ro_field("Run time", sprintf("%.1f s", r$elapsed_seconds)),
      ro_field("Plots", r$n_plots_sampled),
      ro_field("Trees", r$n_trees_total),
      ro_field("Trees with logs", r$n_trees_with_logs),
      ro_field("Logs", nrow(r$selected_logs_all)),
      ro_field("Commercial volume (m³ u.b.)", format(round(tot$commercial_volume_m3, 2), nsmall = 2)),
      ro_field("Total value (NOK)", fmt_int(sum(r$selected_logs_all$value_nok, na.rm = TRUE))),
      ro_field("NOK per m³",
               if (is.na(tot$nok_per_m3)) "–" else format(round(tot$nok_per_m3, 1), nsmall = 1)),
      ro_field("Price volume (m³)",
               format(round(sum(r$selected_logs_all$price_volume_m3, na.rm = TRUE), 2), nsmall = 2))
    )
  })


  # ---- Per m3 -----------------------------------------------------------------
  output$tbl_per_m3 <- renderReactable(apter_reactable(local({
    req(has_result()); s <- result()$per_m3_summary
    data.frame(Scope = s$scope, Trees = s$n_trees,
               Value_NOK = round(s$value_nok, 2),
               Commercial_volume_ub_m3 = round(s$commercial_volume_m3, 4),
               NOK_per_m3 = round(s$nok_per_m3, 1),
               check.names = FALSE, stringsAsFactors = FALSE)
  })))

  output$tbl_per_m3_assortment <- renderReactable(apter_reactable(local({
    req(has_result()); a <- result()$per_m3_by_assortment
    data.frame(Scope = a$scope, Assortment = a$assortment,
               Value_NOK = round(a$value_nok, 2),
               Commercial_volume_ub_m3 = round(a$commercial_volume_m3, 4),
               NOK_per_m3_contribution = round(a$nok_per_m3_contribution, 1),
               Volume_share_pct = round(100 * a$volume_share, 1),
               check.names = FALSE, stringsAsFactors = FALSE)
  })))

  total_by_assortment <- function() {
    a <- result()$per_m3_by_assortment
    a[a$scope == "Total", , drop = FALSE]
  }
  output$bar_per_m3_contribution <- renderPlot(res = 96, {
    req(has_result()); a <- total_by_assortment(); req(nrow(a) > 0L)
    apter_barplot(setNames(a$nok_per_m3_contribution, a$assortment),
                  col = APTER_CHART$series1,
                  main = "NOK per m³ by assortment (contribution)",
                  ylab = "NOK / m³",
                  fmt = function(v) format(round(v, 1), nsmall = 1, trim = TRUE))
  })

  output$bar_volume_share <- renderPlot(res = 96, {
    req(has_result()); a <- total_by_assortment(); req(nrow(a) > 0L)
    apter_barplot(setNames(100 * a$volume_share, a$assortment),
                  col = APTER_CHART$series1,
                  main = "Share of commercial volume by assortment",
                  ylab = "%",
                  fmt = function(v) sprintf("%.1f %%", v))
  })

  # Tree detail on demand: one row per tree, its logs in the row details.
  output$tbl_trees <- renderReactable({
    req(has_result()); r <- result()
    ts <- r$tree_summary
    tr <- data.frame(Plot = ts$plot_id_github, Tree = ts$treid,
                     Species = ts$species_group, DBH_cm = ts$dbh_cm,
                     Height_m = round(ts$height_m, 1), Height_source = ts$height_source,
                     Logs = ts$n_logs_selected,
                     Value_NOK = round(ts$total_value_nok, 2),
                     Commercial_volume_ub_m3 = round(ts$commercial_volume_m3, 4),
                     NOK_per_m3 = round(ts$nok_per_m3, 1),
                     Status = ts$optimizer_status,
                     check.names = FALSE, stringsAsFactors = FALSE)
    sl <- r$selected_logs_all
    log_cols <- intersect(c("ProductKey", "StartPos", "StopPos", "LogLength",
                            "TopDiameter_ob", "TopDiameter_ub",
                            "PriceDiameterClass_cm_used", "price_volume_m3",
                            "commercial_volume_m3", "PriceNOKperM3", "value_nok"),
                          names(sl))
    apter_reactable(tr, page_size = 10L, details = function(i) {
      logs <- sl[sl$plot_id_github %in% tr$Plot[i] & sl$treid == tr$Tree[i],   # plot may be NA (tree lists)
                 log_cols, drop = FALSE]
      if (nrow(logs) == 0L)
        return(div(class = "note-muted", "No selected logs for this tree."))
      div(class = "tree-logs", apter_reactable(logs, digits = 3, page_size = 20L))
    })
  })


  # ---- Products -----------------------------------------------------------------
  output$tbl_products <- renderReactable(apter_reactable(local({
    req(has_result()); result()$product_summary
  }), digits = 3))

  output$bar_value_by_product <- renderPlot(res = 96, {
    req(has_result()); ps <- result()$product_summary; req(nrow(ps) > 0L)
    apter_barplot(setNames(ps$total_value_nok, ps$ProductKey), col = APTER_CHART$series1,
                  main = "Total value (NOK) by product", ylab = "NOK", fmt = fmt_int)
  })

  output$bar_volume_by_product <- renderPlot(res = 96, {
    req(has_result()); ps <- result()$product_summary; req(nrow(ps) > 0L)
    apter_barplot(setNames(ps$total_volume_m3, ps$ProductKey), col = APTER_CHART$series1,
                  main = "Total volume (m³) by product", ylab = "m³",
                  fmt = function(v) format(round(v, 2), nsmall = 2, trim = TRUE))
  })


  # ---- Selected logs ------------------------------------------------------------
  output$tbl_selected_logs <- renderReactable(apter_reactable(local({
    req(has_result()); sl <- result()$selected_logs_all
    if (nrow(sl) == 0L)
      return(data.frame(Message = "No selected logs.", stringsAsFactors = FALSE))
    sl
  }), digits = 3))


  # ---- Price matrix comparison (A vs B) ------------------------------------------
  # Per-tree pattern comparison (R/helpers_comparison.R).
  cmp_trees <- reactive({
    r1 <- result(); r2 <- result_b()
    compare_cutting_patterns(r1, r2, r1$price_matrix, r2$price_matrix)
  })

  output$tbl_cmp_overview <- renderReactable(apter_reactable(local({
    r1 <- result(); r2 <- result_b()
    mk <- function(r, nm) {
      tot <- r$per_m3_summary[r$per_m3_summary$scope == "Total", ]
      data.frame(Price_set = nm, Plots = r$n_plots_sampled, Trees = r$n_trees_total,
                 Trees_w_logs = r$n_trees_with_logs,
                 Value_NOK = round(sum(r$selected_logs_all$value_nok, na.rm = TRUE), 2),
                 Price_volume_m3 = round(sum(r$selected_logs_all$price_volume_m3, na.rm = TRUE), 4),
                 Commercial_m3_ub = round(tot$commercial_volume_m3, 4),
                 NOK_per_m3 = round(tot$nok_per_m3, 1),
                 stringsAsFactors = FALSE)
    }
    rbind(mk(r1, "A"), mk(r2, "B"))
  })))

  output$tbl_cmp_delta <- renderReactable(apter_reactable(local({
    r1 <- result(); r2 <- result_b()
    v1 <- sum(r1$selected_logs_all$value_nok, na.rm = TRUE)
    v2 <- sum(r2$selected_logs_all$value_nok, na.rm = TRUE)
    m1 <- sum(r1$selected_logs_all$price_volume_m3, na.rm = TRUE)
    m2 <- sum(r2$selected_logs_all$price_volume_m3, na.rm = TRUE)
    t1 <- r1$per_m3_summary[r1$per_m3_summary$scope == "Total", ]
    t2 <- r2$per_m3_summary[r2$per_m3_summary$scope == "Total", ]
    pct <- function(a, b) if (abs(b) > 1e-9) round((a - b) / abs(b) * 100, 2) else 0
    data.frame(
      Delta_value_NOK      = round(v2 - v1, 2),
      Delta_value_pct      = pct(v2, v1),
      Delta_volume_m3      = round(m2 - m1, 4),
      Delta_volume_pct     = pct(m2, m1),
      Delta_commercial_m3  = round(t2$commercial_volume_m3 - t1$commercial_volume_m3, 4),
      Delta_NOK_per_m3     = round(t2$nok_per_m3 - t1$nok_per_m3, 1),
      Delta_NOK_per_m3_pct = pct(t2$nok_per_m3, t1$nok_per_m3),
      Trees_changed        = sum(cmp_trees()$changed),
      stringsAsFactors = FALSE)
  })))

  output$tbl_cmp_per_m3 <- renderReactable(apter_reactable(local({
    x <- compare_per_m3(result(), result_b())
    data.frame(Scope = x$scope, Assortment = x$assortment,
               NOK_per_m3_A = round(x$nok_per_m3_default, 1),
               NOK_per_m3_B = round(x$nok_per_m3_scenario, 1),
               Delta_NOK_per_m3 = round(x$delta_nok_per_m3, 1),
               Share_pct_A = round(100 * x$volume_share_default, 1),
               Share_pct_B = round(100 * x$volume_share_scenario, 1),
               Delta_share_pp = round(100 * x$delta_volume_share, 1),
               stringsAsFactors = FALSE)
  }), page_size = 12L))

  output$cmp_patterns <- renderUI({
    x <- cmp_trees()
    div(class = "stat-strip",
        tags$strong(sprintf("%d of %d trees changed their cutting pattern", sum(x$changed), nrow(x))),
        if (sum(x$tie_only) > 0L)
          sprintf(" (%d more differ only by equal-value ties)", sum(x$tie_only)))
  })

  output$tbl_cmp_trees <- renderReactable({
    x <- cmp_trees(); x <- x[x$changed, , drop = FALSE]
    if (nrow(x) == 0L)
      return(apter_reactable(data.frame(Message = "No tree changed its cutting pattern.")))
    x <- x[order(-abs(x$value_scenario - x$value_default)), , drop = FALSE]
    apter_reactable(data.frame(
      Plot = x$plot_id_github, Tree = x$treid, Species = x$species_group,
      Logs_A = x$n_logs_default, Logs_B = x$n_logs_scenario,
      Value_A_NOK = round(x$value_default, 2),
      Value_B_NOK = round(x$value_scenario, 2),
      Gain_at_B_prices = round(x$gain_at_scenario, 2),
      Gain_at_A_prices = round(x$gain_at_default, 2),
      Max_volume_diff_m3 = round(x$max_volume_diff_m3, 4),
      stringsAsFactors = FALSE))
  })

  output$cmp_tree_ui <- renderUI({
    x <- cmp_trees()
    x <- x[order(!x$changed, -abs(x$value_scenario - x$value_default)), , drop = FALSE]
    lbl <- sprintf("%s / %s (%s%s)", x$plot_id_github, x$treid, x$species_group,
                   ifelse(x$changed, sprintf(", %+.1f NOK", x$value_scenario - x$value_default),
                          ", unchanged"))
    selectInput("cmp_tree", "Tree (changed trees first, largest value change first)",
                choices = setNames(paste(x$plot_id_github, x$treid, sep = "|"), lbl),
                width = "100%")
  })

  output$stem_compare <- renderPlot(res = 96, {
    req(!is.null(input$cmp_tree))
    k  <- input$cmp_tree
    tk <- function(df) paste(df$plot_id_github, df$treid, sep = "|")
    l1 <- result()$selected_logs_all;   l1 <- l1[tk(l1) == k, , drop = FALSE]
    l2 <- result_b()$selected_logs_all; l2 <- l2[tk(l2) == k, , drop = FALSE]
    ts <- result()$tree_summary; tr <- ts[tk(ts) == k, , drop = FALSE]
    req(nrow(tr) == 1L)
    apter_stem_compare(l1, l2, tr$height_m,
                       sprintf("Tree %s (%s, dbh %.1f cm, height %.1f m)",
                               tr$treid, tr$species_group, tr$dbh_cm, tr$height_m))
  })

  product_values <- function() {
    ps1 <- result()$product_summary; ps2 <- result_b()$product_summary
    keys <- union(ps1$ProductKey, ps2$ProductKey)
    v1 <- ps1$total_value_nok[match(keys, ps1$ProductKey)]; v1[is.na(v1)] <- 0
    v2 <- ps2$total_value_nok[match(keys, ps2$ProductKey)]; v2[is.na(v2)] <- 0
    list(keys = keys, a = v1, b = v2)
  }

  output$tbl_cmp_products <- renderReactable(apter_reactable(local({
    r1 <- result(); r2 <- result_b()
    ps1 <- r1$product_summary[, c("ProductKey", "ProductType", "total_value_nok", "total_volume_m3")]
    ps2 <- r2$product_summary[, c("ProductKey", "ProductType", "total_value_nok", "total_volume_m3")]
    names(ps2)[2:4] <- c("type_b", "B_val", "B_vol")
    m <- merge(ps1, ps2, by = "ProductKey", all = TRUE)
    m$ProductType[is.na(m$ProductType)] <- m$type_b[is.na(m$ProductType)]
    for (n in c("total_value_nok", "total_volume_m3", "B_val", "B_vol")) m[[n]][is.na(m[[n]])] <- 0
    pct <- function(a, b) ifelse(abs(b) > 1e-9, round((a - b) / abs(b) * 100, 2), 0)
    # Share of the run's total commercial volume under bark, per product.
    share <- function(r, keys) {
      sl  <- r$selected_logs_all
      tot <- sum(sl$commercial_volume_m3, na.rm = TRUE)
      vapply(keys, function(k)
        if (tot > 0) sum(sl$commercial_volume_m3[sl$ProductKey == k]) / tot else 0,
        numeric(1), USE.NAMES = FALSE)
    }
    data.frame(
      ProductKey = m$ProductKey, ProductType = m$ProductType,
      A_value = round(m$total_value_nok, 2), B_value = round(m$B_val, 2),
      Delta_value = round(m$B_val - m$total_value_nok, 2),
      Delta_val_pct = pct(m$B_val, m$total_value_nok),
      A_volume = round(m$total_volume_m3, 4), B_volume = round(m$B_vol, 4),
      Delta_volume = round(m$B_vol - m$total_volume_m3, 4),
      Delta_vol_pct = pct(m$B_vol, m$total_volume_m3),
      A_vol_share_pct = round(100 * share(r1, m$ProductKey), 1),
      B_vol_share_pct = round(100 * share(r2, m$ProductKey), 1),
      stringsAsFactors = FALSE)
  })))

  output$bar_cmp_value <- renderPlot(res = 96, {
    v <- product_values()
    ch  <- APTER_CHART
    old <- apter_par(); on.exit(par(old))
    m   <- rbind(A = v$a, B = v$b); colnames(m) <- gsub("_", "\n", v$keys)
    barplot(m, beside = TRUE, col = NA, border = NA, axes = FALSE,
            ylim = c(0, max(m, 0) * 1.15), las = 1, cex.names = 0.8,
            padj = 0.6, space = c(0, 0.9),
            main = "Value by product: A and B", ylab = "NOK")
    abline(h = axTicks(2), col = ch$grid, lwd = 0.7)
    barplot(m, beside = TRUE, col = c(ch$series1, ch$series2),
            border = ch$surface, lwd = 2, axes = FALSE, add = TRUE,
            space = c(0, 0.9), names.arg = rep("", ncol(m)))
    axis(2, at = axTicks(2), col = NA, col.ticks = NA,
         labels = format(axTicks(2), big.mark = ",", scientific = FALSE, trim = TRUE))
    abline(h = 0, col = ch$baseline)
    legend("topright", c("A", "B"), fill = c(ch$series1, ch$series2),
           border = NA, bty = "n", text.col = ch$ink, cex = 0.85)
  })

  output$bar_cmp_delta <- renderPlot(res = 96, {
    v <- product_values()
    # Sign is carried by bar direction from the zero baseline and the signed
    # label, so one neutral colour is used (no colour-only meaning).
    apter_barplot(setNames(v$b - v$a, v$keys), col = APTER_CHART$neutral,
                  main = "Value change (B − A) by product", ylab = "NOK",
                  fmt = function(x) ifelse(x > 0, paste0("+", fmt_int(x)), fmt_int(x)))
  })

}  # end server
