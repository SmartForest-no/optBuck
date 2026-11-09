# app/server_scenarios.R
#
# Purpose : The "2 Assortment specifications and prices" tab (Phase 4d): Scenario A and
#           Scenario B, each with its own assortment table and price matrices.
#           Called from server() in app_server.R; returns the reactives the
#           runs need.
#
# State   : Two independent scenarios, sc$A and sc$B, each list(defs = the
#           assortment table, pm = its prices). B starts as an exact copy of
#           A; nothing is linked. Both sides are always editable; cells and
#           rows that differ from the other scenario are highlighted on both
#           sides. "Make assortment equal to A/B" copies one assortment from
#           the other scenario (R/helpers_price_matrix.R: copy_assortment());
#           "Copy from A/B" copies the whole other scenario (all assortments
#           and prices, after a confirmation). A run compares A and B when
#           scenarios_differ().
#           Internal names: "default" and "scenario" in the backend mean A and B.
#
# Text    : in the viewer's language, lang() (R/helpers_i18n.R). The
#           scenarios' messages are kept as i18n_text() / English backend
#           text and rendered when shown, so a switch re-renders them. Data
#           stay as they are: the assortment table is translated only for
#           display (base names, species, volume methods).

scenario_server <- function(input, output, session, lang = reactive("en"), grade_dist = reactive(NULL)) {
  L <- function(id, ...) tr(id, ..., lang = lang())
  keys_of <- assortment_table_keys
  sc <- list(A = list(defs = reactiveVal(APP_DEFS), pm = reactiveVal(APP_PM_DEFAULT)),
             B = list(defs = reactiveVal(APP_DEFS), pm = reactiveVal(APP_PM_DEFAULT)))
  other  <- function(side) if (side == "A") "B" else "A"
  low    <- function(side) tolower(side)
  defs   <- function(side) sc[[side]]$defs()
  prices <- function(side) sc[[side]]$pm()
  T_     <- i18n_text
  # Scenarios are called A and B (names were removed in R4).
  scen_name <- function(side) side

  pm_tick <- reactiveVal(0L)                      # re-render the grids
  redraw  <- function() pm_tick(isolate(pm_tick()) + 1L)
  msgs    <- list(A = reactiveVal(NULL), B = reactiveVal(NULL))
  # text: i18n_text() or English backend text (a vector: one line each);
  # warnings: English backend text.
  say     <- function(side, type, text, warnings = NULL)
    msgs[[side]](list(type = type, text = text, warnings = warnings))

  # Stem-grade options of a scenario (Phase 6): the two checkboxes, in effect
  # only for .hpr stems (otherwise both off, so they never change a run).
  #
  # R5: grades are used only for stems whose file has the same instruction as the scenario
  # (hpr_same_instruction(), per species). `grade_dist()` is list(dist, instr, map) from the
  # Stems panel (NULL: no .hpr stems read). With no matching file for any species the box is
  # disabled (and off).
  grade_same <- function(side) {
    gi <- grade_dist()
    if (!identical(input$tree_source, "hpr") || is.null(gi) || NROW(gi$dist) == 0L) return(NULL)
    hpr_same_instruction(gi$instr, gi$map, defs(side), files = sort(unique(gi$dist$file)))
  }
  grade_disabled <- function(side) {
    sm <- grade_same(side)
    !is.null(sm) && !any(sm$same)
  }
  grade_opts <- function(side) {
    if (!identical(input$tree_source, "hpr")) return(GRADE_OPTS_OFF)
    s <- low(side)
    use <- isTRUE(input[[paste0("grade_use_", s)]]) && !grade_disabled(side)
    o <- list(use_grades = use, cut_at_changes = isTRUE(input[[paste0("grade_cut_", s)]]))
    # Either option works only on the stems of files with the scenario's instruction (R5).
    if ((use || o$cut_at_changes) && !is.null(grade_same(side))) o$grade_files <- grade_files_of(grade_same(side), TRUE)
    o
  }
  # Grade mismatch warnings of a scenario (R4 polish): sawlog products that permit the grades of
  # less than half of a species' stem length, with "Use stem grades" on (.hpr stems only); only
  # over the stems whose grades are used (R5).
  grade_warnings <- function(side) {
    o <- grade_opts(side)
    grade_mismatch_check(grade_dist_by_grade(grade_dist()$dist, o$grade_files), defs(side), o$use_grades)
  }
  # The note for species whose grades are not used (R5): shown with the box on, or disabled.
  grade_notes <- function(side) {
    sm <- grade_same(side)
    none <- grade_unused_notes(NULL, NULL)
    if (is.null(sm) || !(isTRUE(input[[paste0("grade_use_", low(side))]]) || grade_disabled(side))) return(none)
    grade_unused_notes(grade_dist()$dist, sm)
  }
  differ   <- reactive(scenarios_differ(defs("A"), prices("A"), defs("B"), prices("B"),
                                        grade_opts("A"), grade_opts("B")))
  # Everything that changes a run; a run stores it to detect stale results.
  pm_state <- reactive(list(defs("A"), prices("A"), defs("B"), prices("B"),
                            grade_opts("A"), grade_opts("B")))

  # Check a scenario and store it; FALSE (with the message) if it fails.
  commit <- function(side, d, pm, err = function(e) say(side, "error", e)) {
    if (nrow(d) == 0L) { err(T_("err_scen_needs_one", side)); return(FALSE) }
    v <- check_price_set(d, pm)
    if (!v$ok) { err(lapply(v$errors, function(e) T_("msg_scenario_prefix", side, e))); return(FALSE) }
    sc[[side]]$defs(d); sc[[side]]$pm(v$pm)
    TRUE
  }

  # "Spruce sawlog" etc. for keys of either scenario, in the viewer's
  # language (species and base names translated; other names as given).
  key_label <- function(keys, l = isolate(lang())) {
    a <- isolate(defs("A")); b <- isolate(defs("B"))
    d <- rbind(a, b[!keys_of(b) %in% keys_of(a), , drop = FALSE])
    k <- keys_of(d); i <- match(keys, k)
    # File names as written; only the base names start in lower case: "Spruce sawlog", "Spruce 140 Sagtømmer gran".
    ifelse(is.na(i), keys, assortment_display_name(d$Species[i], d$Name[i], l))
  }
  # Labels in a message, taken in both languages when the message is made
  # (the assortment may be gone when it is shown): "Spruce sawlog and Pine
  # sawlog"; price files also name "Flat prices".
  fixed_text <- function(f)
    structure(lapply(stats::setNames(OPTBUCK_LANGS, OPTBUCK_LANGS), f), class = "optbuck_fixed")
  key_text <- function(keys)
    fixed_text(function(l) paste(key_label(keys, l), collapse = tr("word_and", lang = l)))
  price_file_text <- function(ids)
    fixed_text(function(l) paste(ifelse(ids == PM_FLAT_ID, tr("flat_prices", lang = l), key_label(ids, l)),
                                 collapse = ", "))
  render_msg <- function(x) {
    l <- lang()
    one <- function(e) {
      if (inherits(e, "optbuck_fixed")) return(e[[l]])
      if (inherits(e, "optbuck_text"))
        return(do.call(tr, c(list(e$id), lapply(e$args, function(a) if (is.numeric(a)) a else one(a)),
                             list(lang = l))))
      if (is.list(e)) return(paste(vapply(e, one, ""), collapse = "\n"))
      paste(tr_msg(e, l), collapse = "\n")
    }
    one(x)
  }
  swap_prices <- function(pm, keys, new = NULL)
    pm_minimal(pm_bind(pm_minimal(pm)[!pm_minimal(pm)$ProductKey %in% keys, , drop = FALSE], new))


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


  # ---- assortment tables ----------------------------------------------------
  # Each value formatted on its own ("4", "14.2"; not "4.0" next to 14.2).
  # The table is built in English (the column names are its fields) and
  # translated for display (show_asm()).
  dash <- function(x) ifelse(is.finite(x), vapply(x, function(v) format(v, trim = TRUE), ""), "–")
  rng  <- function(lo, hi) ifelse(is.na(lo), "–",
                                  ifelse(is.finite(hi), sprintf("%s–%s", dash(lo), dash(hi)),
                                         sprintf("≥ %s", dash(lo))))
  asm_display <- function(d, pm) {
    p <- products_from_defs(d, pm)$pd
    data.frame(
      name = d$Name, type = type_choice(d$Type), species = tools::toTitleCase(d$Species),
      top = rng(d$MinTopDiameter_cm, d$MaxTopDiameter_cm),
      butt = dash(d$MaxButtDiameter_cm),
      length = rng(p$MinLength, p$MaxLength),
      method = unname(VOLUME_METHOD_SHORT[d$VolumeDiameterCategory]),
      position = ifelse(is.na(d$ButtCondition), "\u2013", BUTT_POSITION_VALUES[match(d$ButtCondition, BUTT_CONDITIONS)]),
      pricing = ifelse(d$PriceType == "matrix", "Matrix", "Flat"),
      stringsAsFactors = FALSE)
  }
  # Field -> column title (text id).
  ASM_COL_IDS <- c(name = "col_assortment", type = "col_type", species = "col_species", top = "col_top_d_cm",
                   butt = "col_max_butt_d_cm", length = "col_length_cm",
                   method = "col_volume_method", position = "asm_c_butt", pricing = "col_pricing")
  # Display values: names, species, volume methods and pricing in `l`; the
  # decimal comma in Norsk.
  show_values <- function(disp, l) {
    for (n in c("name", "species", "method", "position", "pricing")) disp[[n]] <- tr_value(disp[[n]], l)
    if (l == "no") for (n in c("top", "butt", "length"))
      disp[[n]] <- gsub(".", ",", disp[[n]], fixed = TRUE)
    disp
  }
  HIDDEN_ASM_COLS <- c("MinButtDiameter_cm", "DiameterTopPosition_cm", "PriceLengths_cm", "PermittedGrades", "ReferenceGrades", "HprProductKey")

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
    l <- lang()
    d <- defs(side); o <- defs(other(side))
    disp <- asm_display(d, prices(side)); disp_o <- asm_display(o, prices(other(side)))
    k <- keys_of(d); ko <- keys_of(o)
    rd <- assortment_diff(d, o)
    cell <- matrix(FALSE, nrow(disp), ncol(disp), dimnames = list(NULL, names(disp)))
    for (i in seq_len(nrow(disp))) {
      j <- match(k[i], ko)
      if (is.na(j)) next
      cell[i, ] <- unlist(disp[i, ]) != unlist(disp_o[j, ])
      if (any(rd$diff[i, HIDDEN_ASM_COLS])) cell[i, "name"] <- TRUE
    }
    cls <- rep("", nrow(d))   # rows the other scenario lacks are not shaded
    all_rows <- d
    ord <- asm_order(all_rows)
    disp <- disp[ord, , drop = FALSE]; cell <- cell[ord, , drop = FALSE]; cls <- cls[ord]
    rownames(disp) <- NULL
    if (!any(!is.na(d$ButtCondition))) {   # the log position column only when this scenario has a condition
      disp$position <- NULL; cell <- cell[, colnames(cell) != "position", drop = FALSE]
    }
    disp <- show_values(disp, l)
    names(disp) <- colnames(cell) <- tr(unname(ASM_COL_IDS[names(disp)]), lang = l)
    cols <- setNames(lapply(names(disp), function(n) reactable::colDef(
      class = function(value, index) if (isTRUE(cell[index, n])) "asm-diff")), names(disp))
    # Hidden key column: a row click selects that assortment on both sides.
    disp$.key <- keys_of(all_rows)[ord]
    # Type (R5): a dropdown per row (Sawlog / Pulpwood / Other); the reserved waste rows are text.
    tcol <- tr("col_type", lang = l)
    cols[[tcol]] <- reactable::colDef(
      class = function(value, index) if (isTRUE(cell[index, tcol])) "asm-diff",
      cell = type_select_js(tolower(side), disp$.key, l))
    cls <- paste(cls, ifelse(disp$.key == or_default(selected(), ""), "asm-selected", ""))
    cols$.key <- reactable::colDef(show = FALSE)
    optbuck_reactable(disp, page_size = 20L, columns = cols, sortable = FALSE,
                    row_class = function(index) cls[index],
                    on_click = reactable::JS(
                      "function(rowInfo) { Shiny.setInputValue('asm_click', rowInfo.original['.key'], {priority: 'event'}); }"),
                    lang = l)
  })
  output$tbl_asm_a <- render_asm("A")
  output$tbl_asm_b <- render_asm("B")

  # The user's choice of an assortment's type (R5): written to that scenario's table
  # (Type and TypeUser), prices untouched.
  for (side in c("A", "B")) local({
    side <- side
    observeEvent(input[[paste0("asm_type_", low(side))]], {
      e <- input[[paste0("asm_type_", low(side))]]
      d <- defs(side); i <- match(e$key, keys_of(d))
      if (is.na(i) || identical(type_choice(d$Type[i]), e$type)) return()
      d2 <- set_assortment_type(d, e$key, e$type)
      if (commit(side, d2, prices(side))) redraw()
    })
  })

  # The detail panel's header: the selected assortment's name, its rules in
  # one line (fields that differ from the other scenario highlighted) and
  # Edit. The panel's parts follow output.asm_state_<s>.
  for (side in c("A", "B")) local({
    side <- side; s <- low(side)
    output[[paste0("asm_state_", s)]] <- reactive(sel_state(side))
    outputOptions(output, paste0("asm_state_", s), suspendWhenHidden = FALSE)
    output[[paste0("asm_head_", s)]] <- renderUI({
      k <- selected(); req(k); l <- lang()
      title <- div(class = "asm-detail-title", key_label(k, l))
      if (sel_state(side) == "none") return(title)
      d <- defs(side); i <- match(k, keys_of(d))
      disp <- asm_display(d, prices(side))[i, ]
      o <- defs(other(side)); j <- match(k, keys_of(o))
      disp_o <- if (!is.na(j)) asm_display(o, prices(other(side)))[j, ]
      hidden_diff <- !is.na(j) && any(assortment_diff(d[i, , drop = FALSE], o)$diff[1L, HIDDEN_ASM_COLS])
      shown <- show_values(disp, l)
      field <- function(n, id = NULL) {
        differs <- !is.null(disp_o) && !identical(disp[[n]], disp_o[[n]])
        span(class = paste("asm-rule", if (differs) "asm-diff"),
             if (is.null(id)) shown[[n]] else tr(id, shown[[n]], lang = l))
      }
      tagList(
        div(class = "asm-detail-head", title,
            actionButton(paste0("asm_edit_", s), L("btn_edit"), class = "btn-secondary btn-sm")),
        div(class = "asm-rules",
          field("species"), field("top", "rule_top"),
          field("butt", "rule_max_butt"), field("length", "rule_length"),
          field("method"), field("pricing", "rule_price"),
          if (hidden_diff) span(class = "asm-rule asm-diff", L("rule_other_differ", other(side)))))
    })
  })


  # ---- the assortment form ---------------------------------------------------
  asm_editing  <- reactiveVal(NULL)   # list(side, key (NULL = new))
  asm_form_err <- reactiveVal(NULL)
  output$asm_form_msg <- renderUI({
    e <- asm_form_err(); if (is.null(e)) return(NULL)
    div(class = "pm-msg-error", render_msg(e))
  })

  asm_modal <- function(side, d = NULL) {
    l <- isolate(lang()); M <- function(id, ...) tr(id, ..., lang = l)
    new <- is.null(d)
    v <- function(n, default) {
      x <- if (new) NULL else d[[n]]
      if (is.null(x) || length(x) == 0L || is.na(x)) default else x
    }
    fin <- function(n) { x <- v(n, NA); if (is.finite(x)) x else NA }
    sp <- species_group_choices(l)[1:2]                  # spruce, pine
    modalDialog(
      title = if (new) M("asm_add_title", side)
              else M("asm_edit_title", tr_value(d$Name, l), tr_value(d$Species, l), side),
      size = "l", easyClose = FALSE, fade = FALSE,
      layout_columns(col_widths = c(4, 4, 4),
        # The name in the viewer's language; saving it unchanged keeps the
        # stored name (form_rows()), so a base row keeps its key.
        textInput("asm_name", M("col_assortment"), tr_value(v("Name", ""), l)),
        selectInput("asm_species", M("col_species"),
                    if (new) species_group_choices(l)[c(3, 1, 2)] else sp,
                    v("Species", if (new) "both" else "spruce")),
        radioButtons("asm_price_type", M("col_pricing"),
                     stats::setNames(c("matrix", "flat"), M(c("pricing_matrix", "pricing_flat"))),
                     v("PriceType", "matrix")),
        numericInput("asm_min_top", M("lbl_min_top"), v("MinTopDiameter_cm", 0), min = 0),
        numericInput("asm_max_top", M("lbl_max_top"), fin("MaxTopDiameter_cm"), min = 0),
        numericInput("asm_dtp", M("lbl_dtp"), v("DiameterTopPosition_cm", 0), min = 0, step = 10),
        numericInput("asm_min_butt", M("lbl_min_butt"), v("MinButtDiameter_cm", 0), min = 0),
        numericInput("asm_max_butt", M("lbl_max_butt"), fin("MaxButtDiameter_cm"), min = 0),
        selectInput("asm_method", M("col_volume_method"),
                    stats::setNames(VOLUME_METHODS, tr_value(VOLUME_METHODS, l)),
                    v("VolumeDiameterCategory", "Top cylinder"))
      ),
      # Lengths: a flat price uses min..max in 10 cm steps; a matrix's
      # lengths are its columns (set here only for a new matrix: every 30 cm).
      conditionalPanel(
        if (new || !identical(d$PriceType, "matrix")) "true" else "input.asm_price_type == 'flat'",
        layout_columns(col_widths = c(4, 4, 4),
          numericInput("asm_min_len", M("lbl_min_len"), v("MinLength_cm", 310), min = 10, step = 10),
          numericInput("asm_max_len", M("lbl_max_len"), v("MaxLength_cm", 550), min = 10, step = 10),
          conditionalPanel("input.asm_price_type == 'matrix'",
            div(class = "note-muted", M("asm_new_matrix_note"))))),
      conditionalPanel(
        if (new) "true" else sprintf("input.asm_price_type != '%s'", d$PriceType),
        layout_columns(col_widths = c(4, 4, 4),
          numericInput("asm_price", M("lbl_start_price"), 500, min = 0),
          conditionalPanel("input.asm_price_type == 'matrix'",
            numericInput("asm_max_class", M("lbl_max_class"), NA, min = 1)))),
      if (new) checkboxInput("asm_both", M("lbl_add_both"), TRUE),
      uiOutput("asm_form_msg"),
      footer = tagList(modalButton(M("btn_cancel")),
                       actionButton("asm_save", M("btn_save"), class = "btn-primary")))
  }

  for (side in c("A", "B")) local({
    side <- side; s <- low(side)
    observeEvent(input[[paste0("asm_add_", s)]], {
      asm_editing(list(side = side, key = NULL)); asm_form_err(NULL); showModal(asm_modal(side))
    })
    observeEvent(input[[paste0("asm_edit_", s)]], {
      d <- defs(side); k <- selected(); i <- match(k, keys_of(d))
      if (is.na(i)) return(say(side, "error", T_("err_not_in_scenario", key_text(k), side)))
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
    # The name as shown (translated base name) unchanged: keep the stored one.
    if (!is.null(old_row) && identical(name, tr_value(old_row$Name, isolate(lang())))) name <- old_row$Name
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
    rows$HprProductKey <- if (is.null(old_row)) NA_character_ else old_row$HprProductKey   # hidden; kept on edits
    rows$TypeUser <- if (is.null(old_row)) FALSE else old_row$TypeUser
    same_pricing <- !is.null(old_row) && identical(old_row$PriceType, ptype)
    if (same_pricing && ptype == "matrix") {   # lengths stay the matrix columns
      rows$MinLength_cm <- old_row$MinLength_cm; rows$MaxLength_cm <- old_row$MaxLength_cm
      rows$PriceLengths_cm <- old_row$PriceLengths_cm   # hidden (hpr imports); kept on edits
    }
    rows$PermittedGrades <- if (!is.null(old_row)) old_row$PermittedGrades else NA_character_   # hidden; kept on edits
    rows$ReferenceGrades <- if (!is.null(old_row)) old_row$ReferenceGrades else NA_character_
    rows$ButtCondition <- if (!is.null(old_row)) old_row$ButtCondition else NA_character_   # read-only
    for (n in c(DISTRIBUTION_DEF_COLS, SOURCE_DEF_COLS))   # hidden fields (Phase 7a, R4 D6); kept on edits
      rows[[n]] <- if (!is.null(old_row)) old_row[[n]] else rows[[n]][NA_integer_]
    errs <- list(); start <- NULL
    if (!same_pricing) {
      price <- num(input$asm_price)
      if (is.na(price) || price < 0) errs <- c(errs, list(T_("err_start_price")))
      lo <- rows$MinLength_cm[1]; hi_len <- rows$MaxLength_cm[1]
      lens <- if (ptype == "matrix" && !anyNA(c(lo, hi_len)) && hi_len >= lo)
        unique(c(seq(lo, hi_len, by = 30), hi_len)) else numeric(0)
      hi <- if (is.finite(rows$MaxTopDiameter_cm[1])) floor(rows$MaxTopDiameter_cm[1]) else num(input$asm_max_class)
      if (length(errs) == 0L)
        start <- tryCatch(do.call(rbind, lapply(keys_of(rows), function(k)
          pm_start_prices(k, ptype, price, max(1, floor(rows$MinTopDiameter_cm[1])), hi, lens))),
          error = function(e) { errs <<- c(errs, list(conditionMessage(e))); NULL })
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
      f$errors <- c(list(T_("err_exists", key_text(taken))), f$errors)
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
    if (!v$ok) return(err(lapply(v$errors, function(e) T_("msg_scenario_prefix", side, e))))
    if (both) {
      o <- other(side)
      d3 <- rbind(defs(o), f$rows); pm3 <- swap_prices(prices(o), new_keys, f$start)
      if (!commit(o, d3, pm3, err)) return()
    }
    commit(side, d2, v$pm, err)
    selected(new_keys[1L])
    redraw(); removeModal()
    say(side, "ok", T_(if (is.null(old)) (if (both) "msg_added_both" else "msg_added") else "msg_saved",
                       key_text(new_keys)))
  })


  # ---- remove, copy, reset -----------------------------------------------------
  asm_removing <- reactiveVal(NULL)    # list(side, key)
  scen_copying <- reactiveVal(NULL)    # the side being replaced
  for (side in c("A", "B")) local({
    side <- side; s <- low(side)
    observeEvent(input[[paste0("asm_remove_", s)]], {
      k <- selected()
      if (!isTRUE(k %in% keys_of(defs(side))))
        return(say(side, "error", T_("err_not_in_scenario", key_text(k), side)))
      in_other <- k %in% keys_of(defs(other(side)))
      asm_removing(list(side = side, key = k))
      showModal(modalDialog(
        title = L("rm_title", key_label(k)), easyClose = TRUE, fade = FALSE,
        if (in_other) L("rm_which", side) else L("rm_only_in", side),
        footer = tagList(modalButton(L("btn_cancel")),
          actionButton("asm_rm_one", if (in_other) L("btn_rm_one", side) else L("btn_remove"),
                       class = "btn-secondary"),
          if (in_other) actionButton("asm_rm_both", L("btn_rm_both"), class = "btn-primary"))))
    })

    # One assortment (definition and prices) equal to the other scenario's.
    observeEvent(input[[paste0("asm_copy_one_", s)]], {
      o <- other(side); k <- selected()
      if (!isTRUE(k %in% keys_of(defs(o))))
        return(say(side, "error", T_("err_not_in_scenario", key_text(k), o)))
      r <- copy_assortment(defs(o), prices(o), defs(side), prices(side), k)
      if (commit(side, r$defs, r$pm)) {
        redraw(); say(side, "ok", T_("msg_equal_to", key_text(k), o))
      }
    })

    # The whole other scenario (all assortments and prices), after a
    # confirmation (scen_copy_ok).
    observeEvent(input[[paste0("scen_copy_", s)]], {
      o <- other(side); scen_copying(side)
      showModal(modalDialog(
        title = L("copy_title", o, side), easyClose = TRUE, fade = FALSE,
        L("copy_body", side, o),
        footer = tagList(modalButton(L("btn_cancel")),
                         actionButton("scen_copy_ok", L("btn_copy"), class = "btn-primary"))))
    })

    # Base assortments back to their defaults (definitions and prices);
    # custom ones stay.
    observeEvent(input[[paste0("asm_reset_", s)]], {
      base <- keys_of(APP_DEFS)
      d <- defs(side); custom <- d[!keys_of(d) %in% base, , drop = FALSE]
      pm <- swap_prices(prices(side), base, APP_PM_DEFAULT[APP_PM_DEFAULT$ProductKey %in% base, , drop = FALSE])
      if (commit(side, rbind(APP_DEFS, custom), pm)) {
        redraw(); say(side, "ok", T_("msg_reset_defaults"))
      }
    })
  })

  do_remove <- function(both) {
    r <- asm_removing(); req(r); side <- r$side; k <- r$key
    sides <- c(side, if (both) other(side))
    new <- lapply(setNames(sides, sides), function(x) {
      d <- defs(x); keep <- keys_of(d) != k
      list(defs = d[keep, , drop = FALSE], pm = swap_prices(prices(x), k))
    })
    for (x in sides) if (nrow(new[[x]]$defs) == 0L) {
      removeModal(); return(say(side, "error", T_("err_scen_needs_one", x)))
    }
    lbl <- key_text(k)   # before the key leaves the scenarios
    for (x in sides) { rownames(new[[x]]$defs) <- NULL; sc[[x]]$defs(new[[x]]$defs); sc[[x]]$pm(new[[x]]$pm) }
    redraw(); removeModal()
    say(side, "ok", if (both) T_("msg_removed_both", lbl) else T_("msg_removed_from", lbl, side))
  }
  observeEvent(input$asm_rm_one,  do_remove(FALSE))
  observeEvent(input$asm_rm_both, do_remove(TRUE))

  # `side` becomes a copy of scenario `from`'s stem-grade boxes with these
  # assortments and prices (Copy from A/B, and the Price tuning tab).
  replace_scenario <- function(side, from, d, pm) {
    if (commit(side, d, pm)) {
      # The stem-grade checkboxes come along (they are part of the scenario).
      for (n in c("grade_use_", "grade_cut_"))
        updateCheckboxInput(session, paste0(n, low(side)), value = isTRUE(input[[paste0(n, low(from))]]))
      redraw(); say(side, "ok", T_("msg_scen_copied", side, from))
    }
  }
  observeEvent(input$scen_copy_ok, {
    side <- scen_copying(); req(side); o <- other(side)
    removeModal(); scen_copying(NULL)
    replace_scenario(side, o, defs(o), prices(o))
  })


  # ---- scenario files (assortments + prices, one xlsx per scenario) ----------
  # The files keep their machine format in both languages (sheet and column
  # names); only the file names follow the language.
  for (side in c("A", "B")) local({
    side <- side; s <- low(side)
    output[[paste0("scen_title_", s)]] <- renderUI(L("scenario_x", scen_name(side)))
    output[[paste0("dl_scen_", s)]] <- downloadHandler(
      filename = function() L("file_scenario", side),
      content  = function(file) scenario_export(defs(side), prices(side), file))
    # The scenario as a StanForD 2010 .pin (Phase 5b, R/helpers_pin_export.R).
    output[[paste0("dl_pin_", s)]] <- downloadHandler(
      filename = function() L("file_pin", side),
      content  = function(file) {
        x <- tryCatch(pin_export_scenario(defs(side), prices(side), file),
                      error = function(e) list(ok = FALSE, errors = conditionMessage(e)))
        if (!isTRUE(x$ok)) {
          showNotification(L("pin_export_failed", paste(tr_msg(x$errors, lang()), collapse = " ")),
                           type = "error", duration = NULL)
          stop("pin_export_failed")
        }
      })
    # A scenario xlsx (with an "assortments" sheet) replaces the scenario;
    # any other xlsx or CSV is a price file for all its assortments; an
    # .hpr, .pin (or .hpr.gz / .zip) opens the import dialog (R1.6).
    observeEvent(input[[paste0("scen_", s, "_upload")]], {
      f <- input[[paste0("scen_", s, "_upload")]]; req(f)
      if (grepl("\\.(hpr|pin|gz|zip)$", f$name, ignore.case = TRUE))
        return(open_hpr_import(side, f$datapath, f$name, uploaded = TRUE))
      sheets <- if (grepl("\\.xlsx$", f$name, ignore.case = TRUE))
        tryCatch(readxl::excel_sheets(f$datapath), error = function(e) character(0))
      if (!"assortments" %in% sheets) {
        res <- upload_price_set(f$datapath, f$name, defs(side), prices(side))
        if (!isTRUE(res$ok))
          return(say(side, "error", c(list(T_("err_not_loaded", f$name)), as.list(res$errors))))
        sc[[side]]$pm(res$pm); redraw()
        return(say(side, "ok", T_("msg_loaded_prices", f$name, price_file_text(res$ids)), res$warnings))
      }
      res <- scenario_import(f$datapath)
      if (!isTRUE(res$ok))
        return(say(side, "error", c(list(T_("err_not_loaded", f$name)), as.list(res$errors))))
      sc[[side]]$defs(res$defs); sc[[side]]$pm(res$pm); redraw()
      say(side, "ok", T_("msg_loaded_scenario", f$name, nrow(res$defs)), res$warnings)
    })
  })

  # ---- import from .hpr (R/helpers_hpr.R; hpr Phase A) ---------------------------
  # A dialog per scenario: read the file, confirm each species group's
  # species (guessed from its name) and the products to import (those
  # without the best grade start unticked), then the file's products and
  # prices replace this scenario's. The other scenario is not changed.
  hpr <- reactiveVal(NULL)    # list(side, data = read_product_file() result (.hpr or .pin), file)
  hpr_err <- reactiveVal(NULL)

  hpr_modal <- function(side) {
    l <- isolate(lang())
    modalDialog(
      title = tr("hpr_title", side, lang = l),
      size = "xl", easyClose = FALSE, fade = FALSE,
      # The file comes from the scenario's upload field or from the Stems tab
      # (restructure R1.5/R1.6): the dialog has no upload of its own.
      uiOutput("hpr_check"), uiOutput("hpr_species"), uiOutput("hpr_products"),
      uiOutput("hpr_msg"),
      footer = tagList(modalButton(tr("btn_cancel", lang = l)),
                       actionButton("hpr_apply", tr("btn_replace_scenario", side, lang = l),
                                    class = "btn-primary", disabled = TRUE)))
  }

  # Read a product file (.hpr, .hpr.gz, .zip or .pin; decided by its content)
  # and open the dialog for a scenario. `uploaded`: Shiny's upload folder is
  # deleted when the session ends. A file taken from the Stems tab is only
  # read here: the stems keep their own copy and are never changed.
  hpr_uploads <- character(0)     # Shiny's upload folders of this session
  session$onSessionEnded(function() unlink(hpr_uploads, recursive = TRUE))
  hpr_kept <- reactiveVal(NULL)   # list(name, path): the file the dialog shows (test value)
  open_hpr_import <- function(side, path, name, uploaded = FALSE) {
    if (uploaded) hpr_uploads <<- unique(c(hpr_uploads, dirname(path)))
    data <- withProgress(message = L("prog_reading", name), value = 0.5,
                         read_product_file(path, name))
    if (!identical(data$kind, "pin")) hpr_kept(list(name = name, path = path))   # a .pin has no stems and is not kept
    hpr(list(side = side, data = data, file = name)); hpr_err(NULL)
    showModal(hpr_modal(side))
  }
  hpr_data <- reactive({ h <- hpr(); if (!is.null(h$data)) h$data })
  # Test mode only (shinytest2): where the kept file is, so audit 17 can check
  # that it is deleted when the session ends.
  exportTestValues(hpr_kept_path = hpr_kept()$path)
  observe(updateActionButton(session, "hpr_apply", disabled = !isTRUE(hpr_data()$ok)))

  output$hpr_check <- renderUI({
    d <- hpr_data(); req(d); l <- lang()
    tagList(
      if (length(d$errors)) div(class = "pm-msg-error", paste(c(L("hpr_cannot_import"), tr_msg(d$errors, l)),
                                                              collapse = "\n")),
      if (isTRUE(d$ok)) div(class = "hpr-summary", L("hpr_summary", nrow(d$species), length(d$products))),
      if (length(d$warnings)) div(class = "hpr-warn",
                                  paste(paste(L("lbl_warning"), tr_msg(d$warnings, l)), collapse = "\n")))
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
      div(class = "section-hdr", L("hdr_species_groups")),
      tags$table(class = "hpr-table",
        tags$thead(tags$tr(tags$th(L("col_key")), tags$th(L("col_species_group")), tags$th(L("col_user_id")),
                           tags$th(L("col_products")), tags$th(L("col_optbuck_species")))),
        tags$tbody(lapply(seq_len(nrow(d$species)), function(i) tags$tr(
          tags$td(d$species$key[i]), tags$td(d$species$name[i]),
          tags$td(or_default(d$species$user_id[i], "")), tags$td(d$species$n_products[i]),
          tags$td(selectInput(paste0("hpr_sp_", d$species$key[i]), NULL, hpr_species_choices(lang()),
                              isolate(or_default(input[[paste0("hpr_sp_", d$species$key[i])]], guess[i])),
                              width = "130px")))))))
  })

  # "130–360 mm (14 classes)" in the viewer's language.
  hpr_range_text <- function(x, l) {
    if (l == "en") return(x)
    m <- regmatches(x, regexec("^(.*) \\(([0-9]+) class(es)?\\)$", x))
    vapply(seq_along(x), function(i) {
      p <- m[[i]]; if (length(p) == 0L) return(x[i])
      paste(p[2L], tr(if (nzchar(p[4L])) "hpr_classes" else "hpr_class_1", p[3L], lang = l))
    }, "")
  }
  output$hpr_products <- renderUI({
    d <- hpr_data(); req(isTRUE(d$ok)); l <- lang()
    tab <- hpr_product_table(d, hpr_map())
    species_txt <- function(x) if (x == "none") L("sp_not_used")
      else paste(tr_value(tools::toTitleCase(strsplit(x, ", ", fixed = TRUE)[[1L]]), l), collapse = ", ")
    tagList(
      div(class = "section-hdr", L("col_products")),
      tags$table(class = "hpr-table",
        tags$thead(tags$tr(tags$th(L("col_import")), tags$th(L("col_product")), tags$th(L("col_species_group")),
                           tags$th(L("col_species")), tags$th(L("col_top_classes")), tags$th(L("col_length_classes_cm")),
                           tags$th(L("col_forbidden_cells")), tags$th(""))),
        tags$tbody(lapply(seq_len(nrow(tab)), function(i) tags$tr(
          class = if (!tab$default[i]) "hpr-off",
          tags$td(class = "hpr-tick", if (tab$species[i] == "none") "–"
                  else checkboxInput(paste0("hpr_pr_", tab$key[i]), NULL,
                                     isolate(or_default(input[[paste0("hpr_pr_", tab$key[i])]], tab$default[i])),
                                     width = "auto")),
          tags$td(tab$name[i]), tags$td(tab$group[i]),
          tags$td(species_txt(tab$species[i])),
          tags$td(class = "hpr-nowrap", hpr_range_text(tab$diameters[i], l)),
          tags$td(class = "hpr-nowrap", hpr_range_text(tab$lengths[i], l)),
          tags$td(tab$forbidden[i]), tags$td(class = "note-muted", tr_msg(tab$reason[i], l)))))))
  })
  output$hpr_msg <- renderUI({
    e <- hpr_err(); if (!is.null(e)) div(class = "pm-msg-error", render_msg(e))
  })

  observeEvent(input$hpr_apply, {
    h <- hpr(); d <- h$data; req(isTRUE(d$ok)); side <- h$side
    map <- hpr_map()
    tab <- hpr_product_table(d, map)
    keep <- tab$key[tab$species != "none" &
                    vapply(tab$key, function(k) isTRUE(input[[paste0("hpr_pr_", k)]]), logical(1))]
    if (length(keep) == 0L) return(hpr_err(T_("hpr_tick_one")))
    res <- hpr_to_scenario(d, map, keep, base_defs = APP_DEFS)
    if (!isTRUE(res$ok)) return(hpr_err(c(list(T_("hpr_not_imported", h$file)), as.list(res$errors))))
    if (!commit(side, res$defs, res$pm, err = function(e) hpr_err(e))) return()
    selected(keys_of(res$defs)[asm_order(res$defs)][1L])
    redraw(); removeModal()
    n <- sum(res$defs$Type != "waste")
    say(side, "ok", T_(if (n == 1L) "msg_imported_1" else "msg_imported", n, h$file, side), res$warnings)
  })

  msg_ui <- function(m) {
    if (is.null(m)) return(NULL)
    tagList(
      div(class = if (identical(m$type, "error")) "pm-msg-error" else "pm-msg-ok", render_msg(m$text)),
      if (length(m$warnings) > 0L)
        div(class = "pm-msg-warn", paste(paste(L("lbl_warning"), tr_msg(m$warnings, lang())), collapse = "\n")))
  }
  output$msg_a <- renderUI(msg_ui(msgs$A()))
  output$msg_b <- renderUI(msg_ui(msgs$B()))


  # ---- prices of the selected assortment ---------------------------------------------
  # In the detail panel, directly under the rules: a matrix-priced assortment
  # shows its editable grid with the row/column buttons, a flat-priced one a
  # single price field. Cells (or the price) that differ from the other
  # scenario, or have no counterpart there, are highlighted.
  flat_price <- function(side, k) {
    p <- pm_minimal(prices(side))
    v <- p$PriceNOKperM3[p$ProductKey == k & is.na(p$LengthClass_dm)]
    if (length(v)) as.numeric(v[1L]) else NA_real_
  }

  # rhandsontable's context menu: undo and redo in the viewer's language
  # (rows and columns are edited with the buttons; the alignment submenu,
  # which cannot be translated and has no use for prices, is left out).
  pm_widget <- function(grid, id, def) {
    l <- isolate(lang())
    # Full height, no scrolling inside the grid: every row is on the page
    # (a 480 px box hid rows added at the end, 2026-10-22).
    w <- rhandsontable(grid, rowHeaders = rownames(grid), colHeaders = colnames(grid),
                       rowHeaderWidth = 50, height = 34 + 24 * nrow(grid),
                       stretchH = "all", matrix_id = id, def = def,
                       selectCallback = TRUE)           # input$pm_<s>_select (bulk edit range)
    w <- hot_cols(w, renderer = PM_DIFF_RENDERER, columnSorting = FALSE)
    # Norsk: prices with a decimal comma (numbro's nb-NO culture, shipped
    # with rhandsontable); English as before.
    if (l == "no") w <- hot_col(w, col = seq_len(ncol(grid)), format = "0.00", language = "nb-NO")
    w <- hot_context_menu(w, allowRowEdit = FALSE, allowColEdit = FALSE, allowReadOnly = FALSE,
                          allowComments = FALSE, allowCustomBorders = FALSE,
                          customOpts = list(
                            undo = list(name = tr("hot_undo", lang = l)),
                            redo = list(name = tr("hot_redo", lang = l))))
    w$x$contextMenu$items <- w$x$contextMenu$items[!grepl("^(alignment|hsep)", names(w$x$contextMenu$items))]
    hot_validate_numeric(w, cols = seq_len(ncol(grid)), min = 0)
  }

  # Both scenarios editable; a cell that differs from the other scenario
  # (or has no counterpart there) is highlighted.
  render_grid <- function(side) renderRHandsontable({
    pm_tick(); id <- selected(); lang()
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
      say(side, "error", T_("err_negative_price"))
      redraw(); return()
    }
    sc[[side]]$pm(pm_from_grid(g, id, isolate(prices(side)), by_class = TRUE))
    redraw()                                          # the other side's highlighting
  }
  observeEvent(input$pm_a, apply_grid_edit(input$pm_a, "A"))
  observeEvent(input$pm_b, apply_grid_edit(input$pm_b, "B"))

  # Flat price edits arrive as input$flat_edit = {side, key, value}
  # (www/optbuck.js, on change of the field), so the key travels with the
  # value, as the matrix id does with a grid.
  observeEvent(input$flat_edit, {
    e <- input$flat_edit; side <- toupper(or_default(e$side, "")); k <- e$key
    if (!side %in% c("A", "B")) return()
    v <- suppressWarnings(as.numeric(e$value))
    d <- defs(side); i <- match(k, keys_of(d))
    if (is.na(i) || d$PriceType[i] != "flat" || length(v) != 1L || is.na(v)) return()
    if (v < 0) {
      say(side, "error", T_("err_negative_price"))
      redraw(); return()
    }
    if (isTRUE(all.equal(v, flat_price(side, k)))) return()
    g <- data.frame(PriceNOKperM3 = v); rownames(g) <- k
    sc[[side]]$pm(pm_from_grid(g, PM_FLAT_ID, prices(side))); msgs[[side]](NULL)
  })

  # ---- target distribution view (restructure R2; R/helpers_distribution_edit.R) -------
  # The same grid with the target percentage per cell and a sum per diameter
  # row (bold when it is neither 0 nor 100). Cells without a price are not
  # editable. An edit goes to every whole-cm row of the same file class
  # (dist_set_cell()); price edits never change it and these edits never
  # change prices.
  DT_RENDERER <- "
function(instance, td, row, col, prop, value, cellProperties) {
  Handsontable.renderers.NumericRenderer.apply(this, arguments);
  var av = instance.params && instance.params.avail;
  var fl = instance.params && instance.params.flag;
  var last = instance.countCols() - 1;
  td.classList.remove('pm-na'); td.style.fontWeight = '';
  if (col === last) { cellProperties.readOnly = true; }
  else if (av && av[row] && av[row][col] === false) { cellProperties.readOnly = true; td.classList.add('pm-na'); }
  if (fl && fl[row] === true) td.style.fontWeight = 'bold';
  return td;
}"
  dt_widget <- function(pm, id) {
    l <- isolate(lang())
    g <- dist_grid(pm, id)
    sums <- dist_row_sums(g)
    df <- cbind(g, sums); names(df) <- c(colnames(g), L("col_sum"))
    av <- cbind(dist_available(pm, id), TRUE)
    w <- rhandsontable(df, rowHeaders = rownames(g), colHeaders = names(df),
                       rowHeaderWidth = 50, height = 34 + 24 * nrow(df), stretchH = "all",
                       matrix_id = id, avail = unname(av), flag = dist_row_flag(sums))
    w <- hot_cols(w, renderer = DT_RENDERER, columnSorting = FALSE, format = "0.[00]")
    if (l == "no") w <- hot_col(w, col = seq_len(ncol(df)), format = "0.[00]", language = "nb-NO")
    w <- hot_col(w, col = ncol(df), readOnly = TRUE)
    w <- hot_context_menu(w, allowRowEdit = FALSE, allowColEdit = FALSE, allowReadOnly = FALSE,
                          allowComments = FALSE, allowCustomBorders = FALSE,
                          customOpts = list(undo = list(name = tr("hot_undo", lang = l)),
                                            redo = list(name = tr("hot_redo", lang = l))))
    w$x$contextMenu$items <- w$x$contextMenu$items[!grepl("^(alignment|hsep)", names(w$x$contextMenu$items))]
    hot_validate_numeric(w, cols = seq_len(ncol(g)), min = 0, max = 100)
  }
  render_target <- function(side) renderRHandsontable({
    pm_tick(); id <- selected(); lang()
    req(id, sel_state(side) == "matrix")
    dt_widget(isolate(prices(side)), id)
  })
  output$dt_a <- render_target("A")
  output$dt_b <- render_target("B")

  # Write a changed target (and the four fields the first time) back.
  set_target <- function(side, pm, id, has_target = dist_has_target(pm, id)) {
    d <- isolate(defs(side))
    d <- if (has_target) dist_init_fields(d, id) else dist_clear_fields(d, id)
    sc[[side]]$defs(d); sc[[side]]$pm(pm); msgs[[side]](NULL); redraw()
  }
  apply_target_edit <- function(x, side) {
    if (is.null(x$changes$changes)) return()            # load, not an edit
    id <- or_default(x$params$matrix_id, isolate(selected()))
    d <- isolate(defs(side))
    if (!isTRUE(id %in% keys_of(d)[d$PriceType == "matrix"])) return()
    pm <- isolate(prices(side)); ref <- dist_grid(pm, id)
    g <- hot_to_r(x)
    if (ncol(g) != ncol(ref) + 1L || nrow(g) != nrow(ref)) return()
    g <- g[, seq_len(ncol(ref)), drop = FALSE]
    a <- as.matrix(g); b <- as.matrix(ref)
    chg <- which(!((is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) < 1e-9)), arr.ind = TRUE)
    err <- NULL
    for (k in seq_len(nrow(chg))) {
      r <- chg[k, 1L]; cc <- chg[k, 2L]
      res <- tryCatch(dist_set_cell(pm, id, as.integer(rownames(ref)[r]), as.integer(colnames(ref)[cc]),
                                    suppressWarnings(as.numeric(a[r, cc]))), error = function(e) e)
      if (inherits(res, "error")) { err <- conditionMessage(res); break }
      pm <- res
    }
    if (!is.null(err)) { say(side, "error", err); redraw(); return() }
    set_target(side, pm, id)
  }
  observeEvent(input$dt_a, apply_target_edit(input$dt_a, "A"))
  observeEvent(input$dt_b, apply_target_edit(input$dt_b, "B"))
  for (side in c("A", "B")) local({
    side <- side; s <- low(side); p <- paste0("dt_", s)
    on_matrix <- function() { id <- isolate(selected()); req(id, isolate(sel_state(side)) == "matrix"); id }
    observeEvent(input[[paste0(p, "_even")]], { id <- on_matrix(); set_target(side, dist_apply_target(isolate(prices(side)), id, "neutral"), id, TRUE) })
    observeEvent(input[[paste0(p, "_long")]], { id <- on_matrix(); set_target(side, dist_apply_target(isolate(prices(side)), id, "long"), id, TRUE) })
    observeEvent(input[[paste0(p, "_clear")]], { id <- on_matrix(); set_target(side, dist_clear(isolate(prices(side)), id), id, FALSE) })
    # DistributionCategory: only for an assortment that has a target.
    output[[paste0(p, "_cat_ui")]] <- renderUI({
      pm_tick(); lang(); id <- selected(); req(id, sel_state(side) == "matrix")
      d <- isolate(defs(side)); cat_ <- d$DistributionCategory[match(id, keys_of(d))]
      req(!is.na(cat_))
      selectInput(paste0(p, "_cat"), L("lbl_dist_category"), width = "190px", selected = cat_,
                  choices = setNames(DIST_CATEGORIES, c(L("dist_cat_number"), L("dist_cat_volume"))))
    })
    # Apportionment bucking (exports only): DistributionAllowed and MAXDeviation, for an
    # assortment that has a target.
    output[[paste0(p, "_app_ui")]] <- renderUI({
      pm_tick(); lang(); id <- selected(); req(id, sel_state(side) == "matrix")
      d <- isolate(defs(side)); i <- match(id, keys_of(d))
      req(!is.na(d$DistributionAllowed[i]))
      ck <- paste0(p, "_allowed")
      tagList(
        checkboxInput(ck, L("lbl_dist_apportion"), value = isTRUE(d$DistributionAllowed[i]), width = "100%"),
        conditionalPanel(sprintf("input.%s", ck),
          numericInput(paste0(p, "_maxdev"), L("lbl_dist_maxdev"), value = d$MAXDeviation[i], min = 0, max = 100,
                       step = 0.5, width = "190px")))
    })
    observeEvent(input[[paste0(p, "_allowed")]], {
      id <- isolate(selected()); v <- input[[paste0(p, "_allowed")]]
      d <- isolate(defs(side)); i <- match(id, keys_of(d))
      if (is.na(i) || is.na(d$DistributionAllowed[i]) || identical(d$DistributionAllowed[i], isTRUE(v))) return()
      sc[[side]]$defs(dist_set_allowed(d, id, v)); msgs[[side]](NULL)
    })
    observeEvent(input[[paste0(p, "_maxdev")]], {
      id <- isolate(selected()); v <- input[[paste0(p, "_maxdev")]]
      d <- isolate(defs(side)); i <- match(id, keys_of(d))
      if (is.na(i) || is.na(d$DistributionAllowed[i]) || is.null(v) || is.na(v) || v < 0 || v > 100 ||
          isTRUE(d$MAXDeviation[i] == v)) return()
      sc[[side]]$defs(dist_set_maxdev(d, id, v)); msgs[[side]](NULL)
    })
    observeEvent(input[[paste0(p, "_cat")]], {
      id <- isolate(selected()); v <- input[[paste0(p, "_cat")]]
      d <- isolate(defs(side)); i <- match(id, keys_of(d))
      if (is.na(i) || is.na(d$DistributionCategory[i]) || identical(d$DistributionCategory[i], v) || !v %in% DIST_CATEGORIES) return()
      sc[[side]]$defs(dist_set_category(d, id, v)); msgs[[side]](NULL)
    })
  })

  # Rows (diameter classes, ends only) and columns (lengths) of a matrix.
  axes_ui <- function(s, id, pm) {
    # The bulk-edit field and mode keep their values when the controls are redrawn.
    kept <- function(n, d) isolate(or_default(input[[paste0(s, n)]], d))
    lens <- sort(unique(pm$LengthClass_dm[pm$ProductKey == id & !is.na(pm$LengthClass_dm)]))
    b <- function(n, l) actionButton(paste0(s, n), l, class = "btn-secondary btn-sm")
    div(class = "pm-axes",
      div(class = "pm-axes-row",
          span(class = "pm-axes-lbl", L("lbl_diameter_classes")),
          b("_cls_add_lo", L("btn_cls_add_lo")), b("_cls_rm_lo", L("btn_cls_rm_lo")),
          b("_cls_add_hi", L("btn_cls_add_hi")), b("_cls_rm_hi", L("btn_cls_rm_hi"))),
      div(class = "pm-axes-row",
          span(class = "pm-axes-lbl", L("lbl_lengths_dm")),
          numericInput(paste0(s, "_len_new"), NULL, NA, min = 1, step = 1, width = "90px"),
          b("_len_add", L("btn_add")),
          selectInput(paste0(s, "_len_pick"), NULL, lens, width = "90px"),
          b("_len_rm", L("btn_remove"))),
      div(class = "pm-axes-row",
          span(class = "pm-axes-lbl", L("lbl_bulk_edit")),
          numericInput(paste0(s, "_bulk_value"), NULL, kept("_bulk_value", NA), step = 1, width = "90px"),
          radioButtons(paste0(s, "_bulk_mode"), NULL, inline = TRUE, selected = kept("_bulk_mode", "amount"),
                       choices = setNames(c("amount", "percent"), c(L("bulk_nok"), L("bulk_pct")))),
          b("_bulk_apply", L("btn_apply")),
          b("_fill_open", L("btn_fill_rows"))))
  }

  # Reset prices: a base assortment priced as by default gets its default
  # rows back (classes and lengths too).
  base_reset_keys <- function(d) {
    k <- keys_of(d); bk <- keys_of(APP_DEFS)
    k[k %in% bk & d$PriceType == APP_DEFS$PriceType[match(k, bk)]]
  }

  for (side in c("A", "B")) local({
    side <- side; s <- low(side); p <- paste0("pm_", s)
    # The cap rule under the grid: logs above the largest class are priced
    # at it, up to the max top diameter (price_class_used()).
    output[[paste0(p, "_cap")]] <- renderUI({
      pm_tick(); id <- selected(); req(id, sel_state(side) == "matrix")
      cls <- suppressWarnings(max(as.integer(rownames(pm_to_grid(prices(side), id)))))
      d <- defs(side); top <- d$MaxTopDiameter_cm[match(id, keys_of(d))]
      req(is.finite(cls))
      top_txt <- if (lang() == "no") sub(".", ",", format(top), fixed = TRUE) else format(top)
      txt <- if (!is.finite(top)) L("pm_cap", cls, cls)
             else if (floor(top) > cls) L("pm_cap_top", cls, cls, top_txt)
      if (!is.null(txt)) div(class = "pm-cap", txt)
    })
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
    observeEvent(input[[paste0(p, "_cls_add_hi")]], edit(function(pm, id) {
      d <- isolate(defs(side))
      pm_add_class(pm, id, "high", max_top_cm = d$MaxTopDiameter_cm[match(id, keys_of(d))])
    }))
    observeEvent(input[[paste0(p, "_cls_rm_lo")]],  edit(function(pm, id) pm_remove_class(pm, id, "low")))
    observeEvent(input[[paste0(p, "_cls_rm_hi")]],  edit(function(pm, id) pm_remove_class(pm, id, "high")))
    observeEvent(input[[paste0(p, "_len_add")]],
                 edit(function(pm, id) pm_add_length(pm, id, input[[paste0(p, "_len_new")]])))
    observeEvent(input[[paste0(p, "_len_rm")]],
                 edit(function(pm, id) pm_remove_length(pm, id, input[[paste0(p, "_len_pick")]])))

    # Bulk edit of the cells selected in the grid (rhandsontable's
    # input$pm_<s>_select: 1-based r, c, r2, c2, any corner order).
    observeEvent(input[[paste0(p, "_bulk_apply")]], {
      sel <- input[[paste0(p, "_select")]]$select
      v <- suppressWarnings(as.numeric(input[[paste0(p, "_bulk_value")]]))
      if (is.null(sel) || length(v) != 1L || !is.finite(v)) return()
      mode <- or_default(input[[paste0(p, "_bulk_mode")]], "amount")
      edit(function(pm, id) pm_bulk_adjust(pm, id, seq(min(sel$r, sel$r2), max(sel$r, sel$r2)),
                                           seq(min(sel$c, sel$c2), max(sel$c, sel$c2)), mode, v))
    })

    # Fill from a price per diameter class + a correction per length.
    observeEvent(input[[paste0(p, "_fill_open")]], {
      id <- selected(); req(id, sel_state(side) == "matrix")
      pm <- prices(side); first <- pm_row_first_prices(pm, id)
      lens <- sort(unique(pm$LengthClass_dm[pm$ProductKey == id & !is.na(pm$LengthClass_dm)]))
      field <- function(prefix, k, v) numericInput(sprintf("%s_fill_%s_%s", p, prefix, k), NULL,
                                                   if (is.na(v)) NA else v, step = 1, width = "110px")
      showModal(modalDialog(
        title = L("fill_title"), size = "m", easyClose = TRUE,
        div(style = "display: flex; gap: 24px; flex-wrap: wrap;",
          tags$table(class = "hpr-table",
            tags$thead(tags$tr(tags$th(L("fill_col_class")), tags$th(L("fill_col_price")))),
            tags$tbody(lapply(names(first), function(k)
              tags$tr(tags$td(k), tags$td(field("row", k, first[[k]])))))),
          tags$table(class = "hpr-table",
            tags$thead(tags$tr(tags$th(L("fill_col_len")), tags$th(L("fill_col_corr")))),
            tags$tbody(lapply(lens, function(k) tags$tr(tags$td(k), tags$td(field("len", k, 0))))))),
        footer = tagList(modalButton(L("btn_cancel")),
                         actionButton(paste0(p, "_fill_apply"), L("btn_apply"), class = "btn-primary"))))
    })
    observeEvent(input[[paste0(p, "_fill_apply")]], {
      id <- selected(); req(id, sel_state(side) == "matrix")
      pm <- prices(side); g <- pm_to_grid(pm, id)
      get <- function(prefix, k) input[[sprintf("%s_fill_%s_%s", p, prefix, k)]]
      num <- function(x, d) { v <- suppressWarnings(as.numeric(x)); if (length(v) != 1L || is.na(v)) d else v }
      rp <- vapply(rownames(g), function(k) num(get("row", k), NA_real_), 0)
      lc <- vapply(colnames(g), function(k) num(get("len", k), 0), 0)
      removeModal()
      if (any(is.na(rp) & rowSums(!is.na(as.matrix(g))) > 0L)) return(say(side, "error", T_("err_fill_price")))
      edit(function(pm, id) pm_bulk_fill(pm, id, rp, lc))
    })

    # The flat price field, rendered per assortment (data-key: the key its
    # edits are written to). Re-rendered on selection, resets and a language
    # switch, not on its own edits, so typing keeps the focus.
    output[[paste0("flat_", s)]] <- renderUI({
      pm_tick(); k <- selected(); req(k, sel_state(side) == "flat")
      div(class = "asm-flat-input", `data-side` = side, `data-key` = k,
          numericInput(paste0("flat_price_", s), L("lbl_flat_price"),
                       isolate(flat_price(side, k)), min = 0, width = "200px"))
    })
    # The other scenario's flat price, highlighted when it differs.
    output[[paste0("flat_other_", s)]] <- renderUI({
      k <- selected(); req(k, sel_state(side) == "flat")
      o <- other(side); mine <- flat_price(side, k)
      theirs <- if (sel_state(o) == "flat") flat_price(o, k) else NA_real_
      # Only an assortment both scenarios have can differ.
      differs <- sel_state(o) != "none" && !isTRUE(all.equal(mine, theirs))
      session$sendCustomMessage("optbuck_toggle_class",
                                list(id = paste0("flat_wrap_", s), cls = "is-diff", on = differs))
      span(class = paste("asm-flat-other", if (differs) "asm-diff"),
           if (is.na(theirs)) L("flat_none_other", o)
           else L("flat_other", o, fmt_num(theirs, if (theirs %% 1 == 0) 0L else 2L, lang())))
    })

    # Reset prices of the selected assortment (base assortments priced as by
    # default only; the button is disabled otherwise).
    observe(updateActionButton(session, paste0(p, "_reset"),
                               disabled = !isTRUE(selected() %in% base_reset_keys(defs(side)))))
    observeEvent(input[[paste0(p, "_reset")]], {
      k <- selected()
      if (!isTRUE(k %in% base_reset_keys(defs(side))))
        return(say(side, "error", T_("err_no_default_prices", key_text(k))))
      if (commit(side, defs(side), swap_prices(prices(side), k,
                                               APP_PM_DEFAULT[APP_PM_DEFAULT$ProductKey == k, , drop = FALSE]))) {
        redraw(); say(side, "ok", T_("msg_prices_reset", key_text(k)))
      }
    })

    # Upload prices for the selected assortment only: rows for other
    # assortments in the file are ignored (and reported).
    observeEvent(input[[paste0(p, "_upload")]], {
      f <- input[[paste0(p, "_upload")]]; req(f); k <- selected()
      res <- upload_assortment_prices(f$datapath, f$name, defs(side), prices(side), k)
      if (!isTRUE(res$ok))
        return(say(side, "error", c(list(T_("err_not_loaded", f$name)), as.list(res$errors))))
      sc[[side]]$pm(res$pm); redraw()
      say(side, "ok", if (length(res$ignored))
                        T_("msg_loaded_asm_prices_ignored", key_text(k), f$name, price_file_text(res$ignored))
                      else T_("msg_loaded_asm_prices", key_text(k), f$name),
          res$warnings)
    })
    # Download the selected assortment's prices.
    sel_prices <- function() { pm <- pm_minimal(prices(side)); pm[pm$ProductKey == selected(), , drop = FALSE] }
    output[[paste0("dl_", s, "_wide")]] <- downloadHandler(
      filename = function() L("file_prices_xlsx", side, selected()),
      content = function(file) pm_template_wide(sel_prices(), file))
    output[[paste0("dl_", s, "_long")]] <- downloadHandler(
      filename = function() L("file_prices_csv", side, selected()),
      content = function(file) pm_export(sel_prices(), file))
  })

  for (side in c("A", "B")) local({
    side <- side; s <- low(side)
    output[[paste0("grade_warn_", s)]] <- renderUI({
      l <- lang(); w <- grade_warnings(side); n <- grade_notes(side)
      dis <- grade_disabled(side)
      tagList(
        if (dis) div(class = "note-muted", tr("grade_disabled", lang = l)),
        if (nrow(n)) div(class = "pm-msg-warn", paste(grade_unused_text(n, side, l), collapse = "
")),
        if (nrow(w)) div(class = "pm-msg-warn", paste(grade_warning_text(w, function(k) key_label(k, l), l), collapse = "
")))
    })
    # The box is disabled (and unticked) when no stem file has the scenario's instruction.
    observe({
      dis <- grade_disabled(side)
      session$sendCustomMessage("optbuck-disable", list(id = paste0("grade_use_", s), disabled = dis))
      if (dis && isTRUE(isolate(input[[paste0("grade_use_", s)]])))
        updateCheckboxInput(session, paste0("grade_use_", s), value = FALSE)
    })
  })

  list(defs_a = sc$A$defs, pm_a = sc$A$pm, defs_b = sc$B$defs, pm_b = sc$B$pm,
       differ = differ, pm_state = pm_state, key_label = key_label, grade_opts = grade_opts,
       grade_warnings = grade_warnings, grade_notes = grade_notes,
       replace_scenario = replace_scenario,
       open_hpr_import = open_hpr_import)  # opens the import dialog for a file (Stems tab)
}