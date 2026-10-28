# app/app_server.R
#
# Purpose : Shiny server function (`server`). Sourced by app.R after setup.R
#           and the app helpers (helpers_ui.R, helpers_validation.R,
#           helpers_upload.R, helpers_jobs.R). Styling via CSS classes
#           (www/apter.css) and APTER_CHART colours (theme.R); no inline colours.
#           Not named server.R: Shiny/shinytest2 reject ui.R/server.R next to app.R.
#
# State   : Scenarios A and B (assortments and prices) live in
#           scenario_server() (server_scenarios.R, Phase 4d); this file holds
#           the Simulator: tree sources, runs and results.
#           Internal names: "default" and "scenario" in the backend mean A and B.
#
# Text    : every text comes from the translation table in the viewer's
#           language, lang() (R/helpers_i18n.R): tr() by id, tr_msg() for the
#           backend's English messages, tr_value() for data values. Outputs
#           depend on lang(), so a switch re-renders them; nothing else
#           changes.

# ---- server -----------------------------------------------------------------
server <- function(input, output, session) {

  # ---- language (Phase 1, 2026-10-27) ----------------------------------------
  # The browser sends input$lang (the navbar switch, remembered per browser;
  # www/apter.js). Until it arrives: the page's start language.
  lang <- reactive(i18n_lang(or_default(input$lang, APP_LANG)))
  L <- function(id, ...) tr(id, ..., lang = lang())
  fmt_int <- function(x) fmt_num(x, 0L, lang())
  # Plain selects carry their labels as choice names: re-labelled on a switch.
  observeEvent(lang(), {
    updateSelectInput(session, "species_group", choices = species_group_choices(lang()),
                      selected = isolate(input$species_group))
    updateSelectInput(session, "fylnr_sel", choices = fylnr_choices(lang()),
                      selected = isolate(input$fylnr_sel))
  }, ignoreInit = TRUE)


  # ---- Sidebar: NFI selection ----------------------------------------------

  # Bonitet choices: values available for the selected HKL group and county.
  abon_lang <- NULL                 # the language of the last render
  output$abonitet_ui <- renderUI({
    hkl_raw    <- unlist(HKL_MAP[input$hkl_group])
    fyl        <- selected_fylnr(input$fylnr_sel)
    sub <- filter_stratum_plots(plots_prod, hkl_raw, fyl = fyl)
    abon_avail <- as.character(sort(unique(as.integer(sub$abonitet))))
    if (length(abon_avail) == 0L)
      abon_avail <- as.character(sort(unique(as.integer(plots_prod$abonitet))))
    pref <- intersect(c("14", "11", "17"), abon_avail)
    sel  <- if (length(pref) > 0L) pref else abon_avail[1L]
    # A language switch keeps the ticked classes (another stratum resets them).
    l <- lang(); cur <- intersect(isolate(input$abonitet), abon_avail)
    if (!identical(abon_lang, l) && !is.null(abon_lang) && length(cur)) sel <- cur
    abon_lang <<- l
    checkboxGroupInput("abonitet", L("lbl_site_class"),
                       choiceNames  = unname(bonitet_labels(abon_avail, lang())),
                       choiceValues = abon_avail,
                       selected     = sel)
  })

  # Only real errors: an empty selection or no matching plots.
  output$stratum_error <- renderUI({
    hkl_raw <- unlist(HKL_MAP[input$hkl_group])
    abon    <- as.integer(input$abonitet)
    if (length(hkl_raw) == 0L || length(abon) == 0L)
      return(div(class = "note-error", L("err_select_class")))
    if (count_eligible_plots(plots_prod, hkl_raw, abon, selected_fylnr(input$fylnr_sel),
                             selected_hoh(input$hoh_range)) == 0L)
      div(class = "note-error", L("err_no_plots"))
  })

  # Backend messages (English) in the viewer's language, one per line.
  lines_of <- function(x) paste(tr_msg(x, lang()), collapse = "\n")
  warn_div <- function(w) if (length(w) > 0L)
    div(class = "pm-msg-warn", paste(paste0(L("lbl_warning"), " ", tr_msg(w, lang())), collapse = "\n"))

  # ---- Uploaded tree list (Phase 5; R/helpers_tree_list.R) -----------------
  tl_prepared <- reactiveVal(NULL)   # prepare_tree_list() result + $file

  observeEvent(input$tl_upload, {
    f <- input$tl_upload; req(f)
    rd <- read_tree_list(f$datapath, f$name)
    p <- if (length(rd$errors) > 0L)
      list(ok = FALSE, errors = rd$errors, warnings = character(0))
    else withProgress(message = L("prog_tree_list"), value = 0.5,
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
            L("tl_ok", p$file, fmt_int(nrow(p$trees)), fmt_int(sum(p$trees$in_range)))
          else paste(c(L("err_file_unusable", p$file), tr_msg(p$errors, lang())), collapse = "\n")),
      warn_div(p$warnings),
      if (!is.null(ex) && nrow(ex) > 0L)
        tags$details(class = "tl-excluded",
          tags$summary(L("tl_not_bucked", fmt_int(nrow(ex)))),
          tags$ul(lapply(seq_len(nrow(ex)), function(i)
            tags$li(sprintf("%s: %s", ex$treid[i], tr_msg(ex$excluded_reason[i], lang())))))),
      if (!is.null(h) && nrow(h) > 0L)
        div(class = "tl-heights",
            div(class = "section-hdr", L("hdr_heights")),
            tags$ul(lapply(seq_len(nrow(h)), function(i) {
              args <- list(tr_value(h$species[i], lang()), fmt_int(h$n_trees[i]), fmt_int(h$n_measured[i]),
                           if (h$method[i] == "fieldPlot") sprintf("fieldPlot (%s)", h$model[i])
                           else tr_msg(h$method[i], lang()))
              tags$li(if (is.na(h$ratio[i])) do.call(L, c("tl_heights_line", args))
                      else do.call(L, c("tl_heights_line_ratio", args, fmt_num(h$ratio[i], 2L, lang()))))
            })))
    )
  })

  output$dl_tree_template <- downloadHandler(
    filename = function() L("file_tree_template"),
    content  = function(file) tree_list_template(file))


  # ---- .hpr stems (hpr Phase B; R/helpers_hpr_stems.R) ---------------------
  # The file: one uploaded in this panel, else the file imported into a
  # scenario this session (with its confirmed species map; a new import
  # takes over again). Several files can be uploaded at once (Phase 2,
  # 2026-10-28): they are read one after the other in one task and combined;
  # file names stay out of the results. The stems are read once, on a worker,
  # when the source is selected; the species map only re-checks them.
  hs_own <- reactiveVal(NULL)       # list(name, path) uploaded here (vectors, in upload order)
  hs_uploads <- character(0)        # Shiny's upload folders of this panel
  session$onSessionEnded(function() unlink(hs_uploads, recursive = TRUE))
  observeEvent(input$hs_upload, {
    f <- input$hs_upload; req(f)
    hs_uploads <<- unique(c(hs_uploads, dirname(f$datapath)))
    hs_own(list(name = f$name, path = f$datapath))
  })
  observeEvent(sc$hpr_imported(), hs_own(NULL), ignoreInit = TRUE)
  hs_file <- reactive({
    own <- hs_own(); imp <- sc$hpr_imported()
    if (!is.null(own)) return(list(name = own$name, path = own$path))
    if (!is.null(imp)) return(list(name = imp$name, path = imp$path, side = imp$side, map = imp$map))
    NULL
  })

  hs_stems   <- reactiveVal(NULL)   # read_hpr_stems() result, plus $path and $read_id
  hs_reading <- reactiveVal(NULL)   # the file being read
  hs_reads   <- 0L
  # The error stays English (like the backend's messages): it is translated
  # when shown.
  hs_task <- ExtendedTask$new(function(path, name)
    promises::catch(
      if (length(path) == 1L) read_hpr_stems_async(path, name) else read_hpr_stem_files_async(path, name),
      function(e)
        list(ok = FALSE, errors = sprintf("The stems of %s cannot be read: %s",
                                          if (length(path) == 1L) name else "the files", conditionMessage(e)),
             warnings = character(0), file = if (length(path) == 1L) name else NA_character_)))
  observe({
    req(identical(input$tree_source, "hpr"))
    f <- hs_file(); req(f)
    if (hs_task$status() == "running" || identical(isolate(hs_stems())$path, f$path)) return()
    hs_reading(f); hs_stems(NULL)
    hs_task$invoke(f$path, f$name)
  })
  observeEvent(hs_task$result(), {
    r <- hs_task$result(); f <- isolate(hs_reading())
    hs_reads <<- hs_reads + 1L
    r$path <- f$path; r$read_id <- hs_reads
    hs_stems(r); hs_reading(NULL)
    own <- isolate(hs_own())                       # an upload here is no longer needed
    if (!is.null(own) && identical(own$path, f$path)) unlink(own$path)
  })

  output$hs_file_ui <- renderUI({
    imp <- sc$hpr_imported(); own <- hs_own()
    tagList(
      if (!is.null(imp) && is.null(own))
        div(class = "hs-file", L("hs_using", imp$name, imp$side)),
      file_input_lang("hs_upload",
                      if (is.null(imp)) L("lbl_hpr_file", HPR_MAX_UPLOAD_MB) else L("lbl_hpr_other"),
                      accept = c(".hpr", ".zip", ".gz"), lang = lang(), multiple = TRUE))
  })

  # Species groups, one row per group name (case-insensitive, trimmed) with
  # the stems summed over files; the choice applies to every file's group of
  # that name. Group keys repeat across files, so rows are mapped by name.
  hs_groups <- function(st) {
    sp <- st$species[st$species$n_stems > 0L, , drop = FALSE]
    nid <- tolower(trimws(sp$name)); u <- unique(nid); i <- match(u, nid)
    data.frame(name_id = u, name = sp$name[i], user_id = sp$user_id[i],
               n = vapply(u, function(x) sum(sp$n_profiles[nid == x]), 0L),
               stringsAsFactors = FALSE, row.names = NULL)
  }
  # Species group key -> "spruce" / "pine" / "none": the panel's choice, else
  # the import's confirmed map for that file, else the guess. Input IDs carry
  # the read, so another file's groups start from their own defaults.
  hs_sp_id <- function(st, i) sprintf("hs_sp_%d_%d", st$read_id, i)
  hs_default <- function(st, g) {
    guess <- guess_hpr_species(g$name, g$user_id)
    f <- isolate(hs_file())
    if (!is.null(f$map) && identical(f$path, st$path)) {
      nid <- tolower(trimws(st$species$name))
      for (i in seq_len(nrow(g))) {
        m <- f$map[st$species$key[nid == g$name_id[i]]]; m <- m[!is.na(m)]
        if (length(m)) guess[i] <- m[[1L]]
      }
    }
    guess
  }
  hs_map <- reactive({
    st <- hs_stems(); req(isTRUE(st$ok))
    g <- hs_groups(st); def <- hs_default(st, g)
    choice <- vapply(seq_len(nrow(g)), function(i) or_default(input[[hs_sp_id(st, i)]], def[i]), "")
    out <- choice[match(tolower(trimws(st$species$name)), g$name_id)]
    out[is.na(out)] <- "none"
    setNames(out, st$species$key)
  })
  output$hs_species_ui <- renderUI({
    st <- hs_stems(); req(isTRUE(st$ok))
    g <- hs_groups(st); def <- hs_default(st, g)
    tagList(
      div(class = "section-hdr", L("hdr_species_groups")),
      tags$table(class = "hpr-table hs-species",
        tags$thead(tags$tr(tags$th(L("col_species_group")), tags$th(L("col_stems")),
                           tags$th(L("col_apter_species")))),
        tags$tbody(lapply(seq_len(nrow(g)), function(i) tags$tr(
          tags$td(g$name[i]),
          tags$td(fmt_int(g$n[i])),
          tags$td(selectInput(hs_sp_id(st, i), NULL, hpr_species_choices(lang()),
                              isolate(or_default(input[[hs_sp_id(st, i)]], def[i])),
                              width = "115px")))))))
  })

  # The stems the run will buck: hpr_stem_checks() (no profiles built).
  hs_check <- reactive({
    st <- hs_stems(); req(st)
    if (!isTRUE(st$ok)) return(st)
    ck <- hpr_stem_checks(st, hs_map())
    ck$n_files <- st$n_files; ck$failed <- st$failed
    ck
  })
  output$hs_validation <- renderUI({
    f <- hs_reading()
    if (!is.null(f))
      return(div(class = "note-muted", if (length(f$name) > 1L) L("hs_reading_files", fmt_int(length(f$name)))
                                       else L("hs_reading", f$name)))
    ck <- hs_check(); req(ck)
    multi <- !is.null(ck$n_files)      # several files: one total line, no file names
    head <- if (!is.null(ck$n_read))
      if (multi) L(if (ck$n_files == 1L) "hs_summary_file1" else "hs_summary_files",
                   fmt_int(ck$n_files), fmt_int(ck$n_read), fmt_int(ck$n_used))
      else L("hs_summary", ck$file, fmt_int(ck$n_read), fmt_int(ck$n_used))
    failed <- if (multi && NROW(ck$failed))
      div(class = "pm-msg-warn", paste(vapply(seq_len(nrow(ck$failed)), function(i)
        L("hs_file_failed", ck$failed$file[i], tr_msg(ck$failed$reason[i], lang())), ""), collapse = "\n"))
    tagList(
      if (isTRUE(ck$ok)) div(class = "pm-msg-ok", head)
      else div(class = "pm-msg-error", paste(c(if (!is.null(head)) head else if (!multi) L("err_file_unusable", ck$file),
                                               tr_msg(ck$errors, lang())), collapse = "\n")),
      failed,
      warn_div(ck$warnings))
  })


  # Live tree counts: the trees the run will buck, from the cached data and
  # the run's own selection code (select_nfi_trees() with build_run_params(),
  # as Run uses them; for tree lists the rows in taperNOR's range), plus a
  # rough run time. No bucking.
  sel_counts <- reactive({
    if (identical(input$tree_source, "list")) {
      p <- tl_prepared(); req(p, isTRUE(p$ok))
      trees <- p$trees[p$trees$in_range, , drop = FALSE]
      pl <- unique(trees$plot_id[!is.na(trees$plot_id)])
      return(tree_counts(trees$species_group, if (length(pl)) length(pl) else NA_integer_, lang = lang()))
    }
    if (identical(input$tree_source, "hpr")) {
      ck <- hs_check(); req(isTRUE(ck$ok))
      return(tree_counts(ck$species_used, NA_integer_, unit = "stem", lang = lang()))
    }
    req(length(input$hkl_group) > 0L, length(input$abonitet) > 0L)
    sel <- select_nfi_trees(trees_meta, build_run_params(input))
    tree_counts(sel$trees$species_group, length(sel$plots), lang = lang())
  })
  output$tree_counts <- renderUI({
    n <- sel_counts()
    secs <- estimate_run_seconds(n$n_trees, APP_WORKERS, compare = sc$differ())
    div(class = "tree-counts",
      div(class = "tree-counts-n", n$text),
      if (n$n_trees > 0L) div(class = "tree-counts-time", L("tc_time",
        if (secs < 90) L("unit_s", fmt_int(secs)) else L("unit_min", fmt_int(secs / 60)))))
  })


  # ---- Assortments and prices (server_scenarios.R) ---------------------------
  sc <- scenario_server(input, output, session, lang)


  # ---- Runs as background tasks (helpers_jobs.R) ---------------------------
  # The backend runs on the workers (R/helpers_parallel.R) as an
  # ExtendedTask: the app stays responsive, the run status shows progress
  # and Run is disabled while a task is running. The browser shows the bar
  # as soon as Run is clicked (www/apter.js); the server replaces it with
  # staged progress: loading (NFI data and taper profiles, or taperNOR for
  # tree lists), bucking, summarising.
  run_progress <- reactiveVal(NULL)
  progress_cb  <- function(done, total, what)
    run_progress(list(done = done, total = total, what = what))

  run_task <- ExtendedTask$new(function(spec, extra) {
    p <- promises::then(run_job_async(spec, progress_cb), function(res) {
      for (n in names(extra)) res[[n]] <- extra[[n]]
      # The Summary's run time: from clicking Run to the results (the
      # backend's elapsed_seconds counts only its own work).
      res$elapsed_total_seconds <- as.numeric(difftime(Sys.time(), extra$t_click, units = "secs"))
      res
    })
    p <- promises::catch(p, function(e) {
      l <- isolate(lang())
      showNotification(tr("err_backend", tr_msg(conditionMessage(e), l), lang = l), type = "error",
                       duration = NULL, session = session)
      NULL
    })
    promises::finally(p, function() unlink(spec$files))
  })
  task_busy <- reactive(run_task$status() == "running")
  observe(updateActionButton(session, "run_btn", disabled = task_busy()))

  # Run: Scenario A, and B on the same trees when B differs from A (one
  # candidate pass for both; the backend's `scenario`).
  run_rejected <- reactiveVal(FALSE)
  run_src      <- reactiveVal("nfi")
  observeEvent(input$run_btn, {
    t_click <- Sys.time()
    reject <- function(msg) {
      session$sendCustomMessage("apter_progress_clear", TRUE)
      showNotification(msg, type = "error", duration = 10); run_rejected(TRUE)
    }
    src <- if (input$tree_source %in% c("list", "hpr")) input$tree_source else "nfi"
    tl  <- if (src == "list") tl_prepared() else NULL
    if (src == "list" && !isTRUE(tl$ok))
      return(reject(L("err_run_tree_list")))
    hs <- if (src == "hpr") tryCatch(hs_check(), error = function(e) NULL)
    if (src == "hpr" && !isTRUE(hs$ok))
      return(reject(L("err_run_hpr")))
    if (src == "nfi" && (length(input$hkl_group) == 0L || length(input$abonitet) == 0L))
      return(reject(L("err_select_class")))

    d <- sc$defs_a(); a <- pm_minimal(sc$pm_a())
    va <- check_price_set(d, a)
    if (!va$ok) return(reject(paste(c(L("err_scen_unusable", "A"), tr_msg(va$errors, lang())), collapse = "\n")))
    compare <- sc$differ(); b <- NULL
    if (compare) {
      b <- list(defs = sc$defs_b(), pm = pm_minimal(sc$pm_b()))
      vb <- check_price_set(b$defs, b$pm)
      if (!vb$ok) return(reject(paste(c(L("err_scen_unusable", "B"), tr_msg(vb$errors, lang())), collapse = "\n")))
    }
    run_rejected(FALSE)

    # A with the default assortments and prices runs exactly as before
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
                 tl = tl, list_params = list(file = if (src == "hpr") (if (is.na(hs$file)) NULL else hs$file) else tl$file),
                 path = path, pd = pd, lc = lc, scenario = scenario, files = files)
    if (src == "hpr") { spec$stems <- hs_stems(); spec$species_map <- hs_map() }
    run_src(src)
    run_progress(list(done = 0L, total = NA_integer_,
                      what = switch(src, nfi = "Loading NFI data and taper profiles",
                                    hpr = "Preparing stem profiles", "Taper profiles")))
    # Each scenario's assortments at run time: per-assortment B - A only for
    # assortments both have (2026-10-22).
    run_task$invoke(spec, list(price_matrix = a, price_matrix_b = if (compare) b$pm,
                               keys = list(A = assortment_table_keys(d),
                                           B = if (compare) assortment_table_keys(b$defs)),
                               state = sc$pm_state(), t_click = t_click))
  })

  result <- reactive({
    if (run_rejected()) return(NULL)
    run_task$result()
  })
  has_result <- reactive(input$run_btn > 0L && !task_busy() && !is.null(result()))
  compared   <- reactive(has_result() && !is.null(result()$scenario))

  # Assortments both scenarios of the last comparison have.
  shared_keys <- reactive({ req(compared()); k <- result()$keys; intersect(k$A, k$B) })
  # " (A only)" / " (B only)" after an assortment label in a comparison.
  only_tag <- function(keys) {
    if (!compared()) return(rep("", length(keys)))
    k <- result()$keys
    ifelse(!keys %in% k$B, L("tag_only", "A"), ifelse(!keys %in% k$A, L("tag_only", "B"), ""))
  }

  # B's run (the backend's $scenario), with its prices.
  result_b <- reactive({
    req(has_result()); r <- result(); req(!is.null(r$scenario))
    s <- r$scenario; s$price_matrix <- r$price_matrix_b
    s
  })
  # One bar over the whole run: loading (unknown length: animated), then
  # the stages' shares of the bar. Stages are named by their English text
  # (helpers_jobs.R) and shown through the table's values.
  STAGES <- list(nfi  = list("Bucking trees" = c(0.10, 0.95), "Summarising results" = c(0.95, 1)),
                 list = list("Taper profiles" = c(0, 0.40), "Bucking trees" = c(0.40, 0.95),
                             "Summarising results" = c(0.95, 1)),
                 hpr  = list("Bucking trees" = c(0.10, 0.95), "Summarising results" = c(0.95, 1)))
  output$run_status <- renderUI({
    if (!task_busy()) return(NULL)
    p <- run_progress()
    if (is.null(p)) p <- list(done = 0L, total = NA_integer_, what = "Starting run")
    rng <- STAGES[[run_src()]][[p$what]]
    known <- !is.null(rng) && !is.na(p$total) && p$total > 0L
    frac <- if (known) rng[1] + (rng[2] - rng[1]) * p$done / p$total else NA
    what <- tr_value(p$what, lang())
    div(class = "status-running",
        if (known) sprintf("%s: %s / %s", what, fmt_int(p$done), fmt_int(p$total)) else paste0(what, "..."),
        div(class = "run-progress",
            if (known) div(class = "run-progress-bar", style = sprintf("width: %.0f%%", 100 * frac))
            else div(class = "run-progress-bar is-indeterminate")))
  })

  output$stale_notice <- renderUI({
    req(has_result())
    if (!identical(sc$pm_state(), result()$state))
      div(class = "stale-notice", L("stale_notice"))
  })

  # Heights from reference curves (tree lists): shown above all results.
  output$height_banner <- renderUI({
    if (!has_result() || !identical(result()$tree_source, "tree_list")) return(NULL)
    w <- result()$height_warnings
    w <- w[grepl("FinnHoyde", w, fixed = TRUE)]
    if (length(w) == 0L) return(NULL)
    div(class = "height-notice", tags$strong(L("lbl_heights")), " ",
        tagList(lapply(tr_msg(w, lang()), function(x) div(x))))
  })


  # ---- readable labels ----------------------------------------------------------
  # "spruce_sawlog" -> "Spruce sawlog"; assortment classes ("sawlog") -> "Sawlog".
  # From the assortment's own name where a scenario has it ("Spruce 140 SCA
  # Sagtømmer"; only the name's first letter lowercased, so "Spruce
  # sawlog"), else from the key (2026-10-22: keys drop ø, å and capitals).
  # In Norsk the species and the base names (Sawlog, Pulpwood, Waste) are
  # translated ("Gran sagtømmer"); other names are the user's own.
  asm_label  <- function(k) {
    l <- lang()
    k <- as.character(k)
    sp <- sub("_.*$", "", k); rest <- gsub("_", " ", sub("^[^_]+_", "", k))
    out <- ifelse(grepl("_", k), paste(tr_value(tools::toTitleCase(sp), l), rest), k)
    d <- isolate(rbind(sc$defs_a(), sc$defs_b())); dk <- assortment_table_keys(d)
    i <- match(k, dk)
    nm <- tr_value(d$Name[i], l); nm <- paste0(tolower(substring(nm, 1, 1)), substring(nm, 2))
    ifelse(is.na(i), out, paste(tr_value(tools::toTitleCase(d$Species[i]), l), nm))
  }
  scope_label <- function(s) tr_value(ifelse(s == "Total", "Total", tools::toTitleCase(as.character(s))), lang())

  # Log table columns -> text ids.
  LOG_LABELS <- c(
    plot_id_github = "col_plot", treid = "col_tree", file = "col_file", species_group = "col_species",
    ProductKey = "col_assortment", StartPos = "col_startpos", StopPos = "col_stoppos",
    LogLength = "col_length_cm", LengthClass_dm = "col_length_class_dm",
    TopDiameter_ob = "col_top_ob_mm", TopDiameter_ub = "col_top_ub_mm",
    PriceDiameterClass_cm_uncapped = "col_diameter_class_cm",
    PriceDiameterClass_cm_used = "col_price_class_used_cm",
    PriceNOKperM3 = "col_price_nok_m3", price_volume_m3 = "col_price_volume_m3",
    commercial_volume_m3 = "col_commercial_volume_m3", value_nok = "col_value_nok")
  # .hpr stems (hpr Phase B): no plots, so no Plot column or Plots figure;
  # stems instead of trees, measured length instead of tree height.
  is_hpr <- reactive(has_result() && identical(result()$tree_source, "hpr"))
  drop_plot <- function(df) if (isTRUE(is_hpr())) df[, setdiff(names(df), L("col_plot")), drop = FALSE] else df
  unit_id   <- function(id) if (isTRUE(is_hpr())) paste0(id, "_stems") else id
  height_lbl <- function() L(if (isTRUE(is_hpr())) "col_measured_length_m" else "col_height_m")
  species_lbl <- function(x) tr_value(tools::toTitleCase(x), lang())

  readable_logs <- function(sl) {
    if (isTRUE(is_hpr())) sl$plot_id_github <- NULL
    cols <- intersect(names(LOG_LABELS), names(sl))
    out <- sl[, cols, drop = FALSE]
    if ("ProductKey" %in% cols) out$ProductKey <- asm_label(out$ProductKey)
    if ("species_group" %in% cols) out$species_group <- species_lbl(out$species_group)
    for (n in intersect(c("price_volume_m3", "commercial_volume_m3"), cols)) out[[n]] <- round(out[[n]], 4)
    for (n in intersect(c("value_nok", "PriceNOKperM3"), cols)) out[[n]] <- round(out[[n]], 2)
    names(out) <- L(unname(LOG_LABELS[cols]))
    out
  }
  # A result CSV in the viewer's language and number format.
  write_result_csv <- function(df, file, na = "NA") write_download_csv(df, file, lang(), na = na)


  # ---- Summary ----------------------------------------------------------------
  output$summary_cards <- renderUI({
    if (input$run_btn == 0L) return(div(class = "note-muted", L("no_run_yet")))
    req(has_result()); r <- result(); l <- lang()
    tot <- function(r) r$per_m3_summary[r$per_m3_summary$scope == "Total", ]
    vol   <- function(r) fmt_num(tot(r)$commercial_volume_m3, 2L, l)
    value <- function(r) fmt_int(sum(r$selected_logs_all$value_nok, na.rm = TRUE))
    nok   <- function(r) fmt_num(tot(r)$nok_per_m3, 1L, l, big = FALSE)
    pvol  <- function(r) fmt_num(sum(r$selected_logs_all$price_volume_m3, na.rm = TRUE), 2L, l)
    run_time <- ro_field(L("lbl_run_time"), L("unit_s_1",
      fmt_num(or_default(r$elapsed_total_seconds, r$elapsed_seconds), 1L, l, big = FALSE)))
    plots <- if (!isTRUE(is_hpr())) ro_field(L("lbl_plots"), fmt_int(r$n_plots_sampled))
    if (!compared())
      return(ro_field_row(
        run_time,
        plots,
        ro_field(L(unit_id("lbl_trees")), fmt_int(r$n_trees_total)),
        ro_field(L(unit_id("lbl_trees_with_logs")), fmt_int(r$n_trees_with_logs)),
        ro_field(L("lbl_logs"), fmt_int(nrow(r$selected_logs_all))),
        ro_field(L("lbl_commercial_volume"), vol(r)),
        ro_field(L("lbl_total_value"), value(r)),
        ro_field(L("lbl_nok_per_m3"), nok(r)),
        ro_field(L("lbl_price_volume"), pvol(r))))
    # A comparison: the run once, then Scenario A (left) and B (right), each
    # a 2 x 2 grid: value and commercial volume, then NOK/m3 and price volume.
    scen <- function(title, x) div(class = "summary-scen",
      div(class = "section-hdr", title),
      div(class = "ro-row ro-grid-2",
        ro_field(L("lbl_total_value"), value(x)),
        ro_field(L("lbl_commercial_volume"), vol(x)),
        ro_field(L("lbl_nok_per_m3"), nok(x)),
        ro_field(L("lbl_price_volume"), pvol(x))))
    tagList(
      ro_field_row(
        run_time,
        plots,
        ro_field(L(unit_id("lbl_trees")), fmt_int(r$n_trees_total)),
        ro_field(L(unit_id("lbl_trees_with_logs_ab")),
                 sprintf("%s / %s", fmt_int(r$n_trees_with_logs), fmt_int(result_b()$n_trees_with_logs))),
        # Trees whose cutting pattern changed (compare_cutting_patterns());
        # a short one-line label, the total is the Trees card (2026-10-23).
        ro_field(L(if (isTRUE(is_hpr())) "lbl_changed_stems" else "lbl_changed_patterns"),
                 fmt_int(sum(cmp_trees()$changed)))),
      layout_columns(col_widths = breakpoints(sm = c(12, 12), lg = c(6, 6)),
                     scen(L("scenario_a"), r), scen(L("scenario_b"), result_b())))
  })


  # ---- Products (results restructure, 2026-10-23) --------------------------------
  # One row per assortment (assortment_summary(), R/helpers_result_tables.R).
  # A single run: logs, price volume, value, NOK/m3 contribution and volume
  # share. After a comparison the table shows value and contribution as A,
  # B and change and the volume share as A and B; the CSV has every column.
  # "–": an assortment the scenario does not have.
  asm_tab <- reactive({
    req(has_result())
    if (compared()) assortment_summary(result(), result_b(), keys = result()$keys)
    else assortment_summary(result())
  })
  # Waste assortments (value 0): left out of the figures.
  waste_keys <- reactive({
    l <- result()$selected_logs_all[, c("ProductKey", "ProductType")]
    if (compared()) l <- rbind(l, result_b()$selected_logs_all[, c("ProductKey", "ProductType")])
    unique(l$ProductKey[l$ProductType %in% "waste"])
  })
  products_df <- function(full = FALSE) {
    x <- asm_tab(); lbl <- asm_label(x$key)
    if (!compared()) {
      out <- data.frame(lbl, x$logs_a, round(x$price_vol_a, 3), round(x$value_a, 0),
                        round(x$contrib_a, 1), round(100 * x$share_a, 1), stringsAsFactors = FALSE)
      names(out) <- L(c("col_assortment", "col_logs", "col_price_volume_m3", "col_value_nok",
                        "col_nok_m3_contribution", "col_volume_share_pct"))
      return(out)
    }
    # id: a column template with %s = "A" or "B"; id_change: the change
    # column's own title ("Value change (NOK)", "Verdiendring (kr)").
    ab <- function(id, id_change, a, b, digits, change = TRUE) {
      out <- list(round(a, digits), round(b, digits))
      names(out) <- L(id, c("A", "B"))
      if (change) out[[L(id_change)]] <- round(b - a, digits)
      out
    }
    cols <- c(stats::setNames(list(lbl), L("col_assortment")),
              if (full) ab("col_logs_x", "col_logs_change", x$logs_a, x$logs_b, 0),
              if (full) ab("col_price_volume_x_m3", "col_price_volume_change_m3", x$price_vol_a, x$price_vol_b, 3),
              ab("col_value_x_nok", "col_value_change_nok", x$value_a, x$value_b, 0),
              ab("col_nok_m3_x_contribution", "col_nok_m3_change_contribution", x$contrib_a, x$contrib_b, 1),
              ab("col_volume_share_x_pct", "col_volume_share_change_pct", 100 * x$share_a, 100 * x$share_b, 1,
                 change = full))
    as.data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE)
  }
  output$tbl_products <- renderReactable({
    df <- products_df(); num <- names(df)[-1L]
    apter_reactable(df, columns = setNames(lapply(num, function(n) reactable::colDef(na = "–")), num),
                    page_size = 25L, lang = lang())
  })
  output$dl_products <- downloadHandler(
    filename = function() L("file_products"),
    content  = function(file) write_result_csv(products_df(full = TRUE), file, na = ""))

  # NOK/m3 and volume share by assortment type (Sawlog, Pulpwood, Other) per
  # scope; always by type (per assortment is the table above).
  output$tbl_per_m3_type <- renderReactable(apter_reactable(local({
    req(has_result())
    x <- compare_per_m3(result(), if (compared()) result_b() else result(), by = "type")
    out <- data.frame(scope_label(x$scope), tr_value(x$assortment, lang()), stringsAsFactors = FALSE)
    names(out) <- L(c("col_scope", "col_assortment"))
    num <- if (!compared())
      stats::setNames(list(round(x$nok_per_m3_default, 1), round(100 * x$volume_share_default, 1)),
                      L(c("col_nok_per_m3", "col_volume_share_pct")))
    else stats::setNames(list(round(x$nok_per_m3_default, 1), round(x$nok_per_m3_scenario, 1),
                              round(x$delta_nok_per_m3, 1),
                              round(100 * x$volume_share_default, 1), round(100 * x$volume_share_scenario, 1),
                              round(100 * x$delta_volume_share, 1)),
                         c(L("col_nok_per_m3_x", "A"), L("col_nok_per_m3_x", "B"), L("col_change_nok_m3"),
                           L("col_volume_share_x_pct", "A"), L("col_volume_share_x_pct", "B"),
                           L("col_change_pp")))
    cbind(out, as.data.frame(num, check.names = FALSE))
  }), page_size = 20L, lang = lang()))

  # NOK/m3 by assortment (contribution): horizontal bars, A and B after a
  # comparison (waste left out).
  contrib_fig <- reactive({
    x <- asm_tab(); x[!x$key %in% waste_keys(), , drop = FALSE]
  })
  fmt_nok1 <- function(v) fmt_num(v, 1L, lang(), big = FALSE)
  output$bar_nok_contribution <- renderPlot(res = 96, height = function() hbar_height(nrow(contrib_fig()), ab = compared()), {
    x <- contrib_fig(); req(nrow(x) > 0L)
    ttl <- L("fig_nok_by_assortment"); xl <- L("unit_nok_m3")
    if (compared())
      return(apter_hbar_ab(x$contrib_a, x$contrib_b, asm_label(x$key), title = ttl, xlab = xl,
                           fmt = fmt_nok1, lang = lang()))
    apter_hbar(x$contrib_a, asm_label(x$key), title = ttl, xlab = xl, fmt = fmt_nok1,
               col = APTER_CHART$series1, lang = lang())
  })

  # Value change (B − A) of the assortments both scenarios have (waste left
  # out): horizontal bars sorted by change; no change (to the NOK) is left
  # out and counted in a line under the figure. Sign is carried by the bar's
  # side of zero and the signed label (one neutral colour).
  delta_fmt <- function(x) ifelse(x > 0, paste0("+", fmt_int(x)), fmt_int(x))
  delta_shared <- reactive({
    x <- asm_tab(); x <- x[x$key %in% shared_keys() & !x$key %in% waste_keys(), , drop = FALSE]
    d <- x$value_b - x$value_a; ch <- !is.na(d) & round(d) != 0
    list(value = d[ch], label = asm_label(x$key[ch]), n = nrow(x), n_same = sum(!ch))
  })
  output$cmp_value_ui <- renderUI({
    if (!compared()) return(NULL)
    x <- delta_shared()
    note <- if (x$n == 0L) L("cmp_none_shared")
            else if (x$n_same > 0L)
              L(paste0(if (length(x$value) == 0L) "cmp_all_unchanged" else "cmp_n_unchanged",
                       if (x$n_same == 1L) "_1" else ""), x$n_same)
    tagList(
      div(class = "section-hdr", L("hdr_value_change")),
      if (length(x$value)) plotOutput("bar_cmp_delta", height = "auto"),
      if (!is.null(note)) div(class = "fig-note", id = "cmp_delta_note", note))
  })
  output$bar_cmp_delta <- renderPlot(res = 96, height = function() hbar_height(length(delta_shared()$value)), {
    x <- delta_shared(); req(length(x$value) > 0L)
    apter_hbar(x$value, x$label, title = L("fig_value_change"), xlab = L("unit_nok"), fmt = delta_fmt,
               lang = lang())
  })


  # ---- Trees (replaces Per m3, 2026-10-23) ---------------------------------------
  # Figures (DBH classes, heights), the trees table (all trees, server-side
  # pages, sortable; after a comparison value A, B and change) and the
  # cutting pattern of the tree clicked in it.
  readable_trees <- function(r) {
    ts <- r$tree_summary
    out <- data.frame(ts$plot_id_github, ts$treid, species_lbl(ts$species_group),
                      if (isTRUE(is_hpr())) round(ts$dbh_cm, 1) else ts$dbh_cm,   # hpr: read from the profile
                      round(ts$height_m, 1), tr_msg(ts$height_source, lang()),
                      ts$n_logs_selected, round(ts$total_value_nok, 2),
                      round(ts$commercial_volume_m3, 4), round(ts$nok_per_m3, 1),
                      stringsAsFactors = FALSE)
    names(out) <- c(L(c("col_plot", "col_tree", "col_species", "col_dbh_cm")), height_lbl(),
                    L(c("col_height_source", "col_logs", "col_value_nok", "col_commercial_volume_m3",
                        "col_nok_per_m3")))
    if (isTRUE(is_hpr())) out[[L("col_height_source")]] <- NULL   # Tree = the StemKey
    if ("file" %in% names(ts))                                    # several .hpr files: the file's position
      out <- cbind(out[, 1:2, drop = FALSE], stats::setNames(data.frame(ts$file), L("col_file")),
                   out[, -(1:2), drop = FALSE])
    drop_plot(out)
  }
  tree_key <- function(df) paste(df$plot_id_github, df$treid, sep = "|")
  # The table: A's trees; after a comparison Value A, Value B and Change
  # (B's value of the same tree); logs, volume and NOK/m3 are A's.
  trees_df <- reactive({
    req(has_result()); t <- readable_trees(result())
    if (!compared()) return(t)
    a <- result()$tree_summary; b <- result_b()$tree_summary
    vb <- round(b$total_value_nok[match(tree_key(a), tree_key(b))], 2)
    names(t)[names(t) == L("col_logs")] <- L("col_logs_x", "A")
    names(t)[names(t) == L("col_commercial_volume_m3")] <- L("col_commercial_volume_x_m3", "A")
    names(t)[names(t) == L("col_nok_per_m3")] <- L("col_nok_per_m3_x", "A")
    i <- match(L("col_value_nok"), names(t)); va <- t[[i]]
    v <- stats::setNames(data.frame(va, vb, round(vb - va, 2)),
                         c(L("col_value_x_nok", "A"), L("col_value_x_nok", "B"), L("col_change_nok")))
    cbind(t[, seq_len(i - 1L), drop = FALSE], v, t[, -seq_len(i), drop = FALSE])
  })
  tree_sel <- paged_table_server(input, output, session, "tbl_trees", trees_df, page_size = 15L,
                                 selection = TRUE, filename = function(l) tr("file_trees_table", lang = l),
                                 lang = lang)
  output$dl_trees <- downloadHandler(
    filename = function() L("file_trees"),
    content  = function(file) {
      sc_col <- function(x, s) cbind(stats::setNames(data.frame(s), L("col_scenario")), x)
      t <- sc_col(readable_trees(result()), "A")
      if (compared()) t <- rbind(t, sc_col(readable_trees(result_b()), "B"))
      write_result_csv(t, file)
    })

  # Cutting pattern of the clicked tree (the table's first row until one is
  # clicked): A and B after a comparison, A alone otherwise.
  output$stem_note <- renderUI({
    req(has_result())
    div(class = "note-muted", L(paste0(if (isTRUE(is_hpr())) "stem_note_stem" else "stem_note_tree",
                                       if (compared()) "_ab" else "")))
  })
  output$stem_compare <- renderPlot(res = 96, {
    req(has_result()); ts <- result()$tree_summary; req(nrow(ts) > 0L)
    i <- or_default(tree_sel(), 1L); row <- ts[i, , drop = FALSE]; k <- tree_key(row)
    l1 <- result()$selected_logs_all; l1 <- l1[tree_key(l1) == k, , drop = FALSE]
    l2 <- if (compared()) { l <- result_b()$selected_logs_all; l[tree_key(l) == k, , drop = FALSE] }
    apter_stem_compare(l1, l2, row$height_m,
                       L(if (isTRUE(is_hpr())) "stem_title_stem" else "stem_title_tree",
                         row$treid, tr_value(row$species_group, lang()),
                         fmt_num(row$dbh_cm, 1L, lang(), big = FALSE), fmt_num(row$height_m, 1L, lang(), big = FALSE)),
                       lang = lang())
  })

  # Per-tree pattern comparison (R/helpers_comparison.R): the Summary's
  # count and the changed-trees CSV.
  cmp_trees <- reactive({
    r1 <- result(); r2 <- result_b()
    compare_cutting_patterns(r1, r2, r1$price_matrix, r2$price_matrix)
  })
  cmp_changed_df <- reactive({
    x <- cmp_trees(); x <- x[x$changed, , drop = FALSE]
    x <- x[order(-abs(x$value_scenario - x$value_default)), , drop = FALSE]
    fin <- function(v) ifelse(is.finite(v), v, NA_real_)   # a log not priced in the other scenario
    out <- data.frame(x$plot_id_github, x$treid, species_lbl(x$species_group),
                      x$n_logs_default, x$n_logs_scenario,
                      round(x$value_default, 2), round(x$value_scenario, 2),
                      round(fin(x$gain_at_scenario), 2), round(fin(x$gain_at_default), 2),
                      round(x$max_volume_diff_m3, 4), stringsAsFactors = FALSE)
    names(out) <- c(L(c("col_plot", "col_tree", "col_species")), L("col_logs_x", c("A", "B")),
                    L("col_value_x_nok", c("A", "B")), L("col_gain_at_x", c("B", "A")),
                    L("col_largest_volume_change"))
    drop_plot(out)
  })
  output$dl_changed_trees_ui <- renderUI({
    if (!compared()) return(NULL)
    downloadButton("dl_changed_trees", L("btn_dl_changed_trees"), class = "btn-secondary btn-sm")
  })
  output$dl_changed_trees <- downloadHandler(
    filename = function() L("file_changed_trees"),
    content  = function(file) write_result_csv(cmp_changed_df(), file))

  # Value by DBH class (R/helpers_result_tables.R; 5 cm classes): the figure
  # (trees per class, NOK/m3 per class in A and B) and the CSV.
  dbh_a <- reactive({ req(has_result()); dbh_class_summary(result()$tree_summary) })
  dbh_b <- reactive({ req(compared()); dbh_class_summary(result_b()$tree_summary) })
  dbh_table <- reactive({
    a <- dbh_a()
    out <- data.frame(species_lbl(a$species_group), i18n_decimals(tr_value(a$dbh_class, lang()), lang()), a$trees,
                      stringsAsFactors = FALSE)
    names(out) <- L(c("col_species", "col_dbh_class_cm", "col_trees"))
    if (!compared())
      return(cbind(out, stats::setNames(data.frame(round(a$commercial_volume_m3, 3), round(a$value_nok, 0),
                                                   round(a$nok_per_m3, 1)),
                                        L(c("col_commercial_volume_m3", "col_value_nok", "col_nok_per_m3")))))
    b <- dbh_b()[match(paste(a$species_group, a$dbh_class),
                       paste(dbh_b()$species_group, dbh_b()$dbh_class)), ]
    # Trees and commercial volume once (A's); B's NOK/m3 uses B's own volume.
    cbind(out, stats::setNames(data.frame(
      round(a$commercial_volume_m3, 3), round(a$value_nok, 0), round(b$value_nok, 0),
      round(b$value_nok - a$value_nok, 0),
      round(a$nok_per_m3, 1), round(b$nok_per_m3, 1), round(b$nok_per_m3 - a$nok_per_m3, 1)),
      c(L("col_commercial_volume_x_m3", "A"), L("col_value_x_nok", c("A", "B")), L("col_value_x_nok", "B − A"),
        L("col_nok_per_m3_x", c("A", "B", "B − A")))))
  })
  output$dl_dbh_class <- downloadHandler(
    filename = function() L("file_dbh_class"),
    content  = function(file) write_result_csv(dbh_table(), file))
  output$fig_dbh_class <- renderPlot(res = 96, height = function() 320, {
    req(has_result())
    apter_dbh_class_figure(dbh_a(), if (compared()) dbh_b(), lang = lang())
  })

  # DBH and height distributions per species, side by side (the same trees
  # in A and B, so shown once).
  output$fig_distributions <- renderPlot(res = 96, {
    req(has_result()); ts <- result()$tree_summary
    apter_distribution_figure(ts$dbh_cm, ts$height_m, ts$species_group, height_lbl(), lang = lang())
  })


  # ---- Logs (renamed from Selected logs) ------------------------------------------------------------
  # A short preview (Scenario A's first logs); all logs are a CSV download,
  # with both scenarios and a Scenario column after a comparison.
  LOG_PREVIEW_N <- 20L
  output$logs_preview_note <- renderUI({
    req(has_result()); n <- nrow(result()$selected_logs_all)
    div(class = "note-muted", L(if (compared()) "logs_preview_ab" else "logs_preview",
                                min(LOG_PREVIEW_N, n), fmt_int(n)))
  })
  output$tbl_selected_logs <- renderReactable({
    req(has_result()); sl <- result()$selected_logs_all
    if (nrow(sl) == 0L)
      return(apter_reactable(stats::setNames(data.frame(L("no_logs")), L("col_message")), lang = lang()))
    apter_reactable(readable_logs(utils::head(sl, LOG_PREVIEW_N)), sortable = FALSE, pagination = FALSE,
                    lang = lang())
  })
  output$dl_logs <- downloadHandler(
    filename = function() L("file_logs"),
    content  = function(file) {
      sc_col <- function(x, s) cbind(stats::setNames(data.frame(s), L("col_scenario")), x)
      l <- sc_col(readable_logs(result()$selected_logs_all), "A")
      if (compared()) l <- rbind(l, sc_col(readable_logs(result_b()$selected_logs_all), "B"))
      write_result_csv(l, file)
    })

  # Log tally per price matrix (stocknota; R/helpers_result_tables.R): the
  # commercial logs per price-diameter class x length class of one
  # assortment, laid out like its price grid; logs or volume; A, B or B - A.
  tallies <- reactive({
    req(has_result())
    c(list(A = log_tally(result()$selected_logs_all, result()$price_matrix)),
      if (compared()) list(B = log_tally(result_b()$selected_logs_all, result_b()$price_matrix)))
  })
  tally_pms <- reactive(c(list(A = result()$price_matrix),
                          if (compared()) list(B = result_b()$price_matrix)))
  observe({
    keys <- do.call(log_tally_keys, unname(tallies()))
    cur <- isolate(input$tally_key)
    updateSelectInput(session, "tally_key", choices = setNames(keys, paste0(asm_label(keys), only_tag(keys))),
                      selected = if (!is.null(cur) && cur %in% keys) cur else keys[1L])
  })
  # The views of the selected assortment: the scenarios that have it, and
  # B − A only when both have it (2026-10-22).
  tally_views <- reactive({
    if (!compared()) return("A")
    k <- or_default(input$tally_key, ""); sk <- result()$keys
    v <- c(if (k %in% sk$A) "A", if (k %in% sk$B) "B", if (k %in% sk$A && k %in% sk$B) "diff")
    if (length(v)) v else "A"
  })
  # Log length distribution (A, and B below it after a comparison).
  output$fig_log_lengths <- renderPlot(res = 96, height = function() if (compared()) 440 else 260, {
    req(has_result())
    apter_log_length_figure(log_length_distribution(result()$selected_logs_all),
                            if (compared()) log_length_distribution(result_b()$selected_logs_all),
                            lang = lang())
  })
  output$tally_scenario_ui <- renderUI({
    if (!compared()) return(NULL)
    v <- tally_views(); cur <- isolate(input$tally_scenario)
    radioButtons("tally_scenario", L("col_scenario"), setNames(v, c(A = "A", B = "B", diff = "B − A")[v]),
                 selected = if (!is.null(cur) && cur %in% v) cur else v[1L], inline = TRUE)
  })
  tally_view <- reactive({
    v <- tally_views(); s <- or_default(input$tally_scenario, "A")
    if (s %in% v) s else v[1L]
  })
  tally_grid <- reactive({
    t <- tallies(); p <- tally_pms(); k <- input$tally_key
    req(!is.null(k), nzchar(k), k %in% do.call(log_tally_keys, unname(t)))
    ms <- or_default(input$tally_measure, "logs"); v <- tally_view()
    if (v == "diff") log_tally_diff_grid(t$A, p$A, t$B, p$B, k, ms)
    else log_tally_grid(t[[v]], p[[v]], k, ms)
  })
  output$tally_note <- renderUI({
    g <- tally_grid()
    div(class = "note-muted", L(if (identical(g[[1L]][1L], "All diameters")) "tally_caption_flat"
                                else "tally_caption"))
  })
  output$tbl_tally <- renderReactable({
    g <- tally_grid(); ms <- or_default(input$tally_measure, "logs"); diff <- tally_view() == "diff"
    l <- lang(); num <- names(g)[-1L]
    fmt <- function(x) {
      s <- if (ms == "logs") fmt_num(x, 0L, l) else fmt_num(x, 3L, l)
      s <- if (diff) ifelse(x > 0, paste0("+", s), s) else s
      s[!is.na(x) & abs(x) < 1e-12] <- ""
      s[is.na(x)] <- "–"
      s
    }
    show <- g; show[num] <- lapply(g[num], fmt)
    show[[1L]] <- tr_value(show[[1L]], l)                       # "All diameters", "Total"
    names(show) <- tr_value(names(show), l)                     # the "Total" column
    names(show)[1L] <- if (identical(g[[1L]][1L], "All diameters")) " " else L("col_diameter_symbol_cm")
    cols <- setNames(lapply(names(show)[-1L], function(n) reactable::colDef(align = "right")), names(show)[-1L])
    last <- nrow(show)
    apter_reactable(show, columns = cols, sortable = FALSE, pagination = FALSE,
                    row_class = function(i) if (i == last) "tally-total", lang = l)
  })
  output$dl_tally_xlsx <- downloadHandler(
    filename = function() L("file_tally_xlsx"),
    content  = function(file) log_tally_xlsx(file, tallies(), tally_pms(), label = asm_label,
                                             scenario_keys = if (compared()) result()$keys, lang = lang()))
  output$dl_tally_csv <- downloadHandler(
    filename = function() L("file_tally_csv"),
    content  = function(file) {
      l <- log_tally_long(tallies())
      if (nrow(l)) l$Assortment <- asm_label(l$Assortment)
      write_result_csv(i18n_df(l, lang()), file)
    })


}  # end server
