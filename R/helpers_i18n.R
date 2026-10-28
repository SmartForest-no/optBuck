# ==============================================================================
# R/helpers_i18n.R
#
# Purpose : The app's two languages, Norsk (bokmål, "no") and English ("en"),
#           from one translation table, inputs/i18n/translations.csv
#           (Phase 1 of the 2026-10-27 roadmap). Columns:
#             id    the text's id; app code refers to `ui` texts by id
#             kind  ui    : app text (labels, buttons, titles, column names)
#                   msg   : a backend message template, matched by its
#                           English text (sprintf placeholders as in the code)
#                   value : a data value shown to users ("spruce", "Sawlog",
#                           "Top cylinder", "Total"), matched by its English
#             en, no  the texts; placeholders (%s, %d, %g, %.1f) in the same
#                     order in both (or positional, %1$s, in `no`)
#             note  free text for translators
#
# Backend messages stay English in the backend: scripts, audits and
# baselines see exactly the text they saw before. The app translates them
# when it shows them (tr_msg()), so a language switch also re-translates
# messages already on screen.
#
# Functions:
#   tr(id, ..., lang)       a `ui` text by id, sprintf-filled
#   tr_msg(text, lang)      English backend text -> lang (exact, then templates;
#                           the captured arguments are translated in turn)
#   tr_value(x, lang)       data values -> lang (exact matches only)
#   i18n_df(df, lang)       column names and character values of a table
#   i18n_text(id, ...)      a deferred text (rendered later by render_text())
#   render_text(x, lang)    i18n_text / English text / list of them -> lang
#   fmt_num(x, digits, lang, big)  numbers: "1,234.5" / "1 234,5"
#   write_download_csv(df, file, lang)  English: write.csv; Norsk: ";" and
#                           decimal comma (write.csv2), UTF-8 with BOM
# ==============================================================================

if (!exists("or_default", mode = "function"))   # helpers_utils.R
  source(file.path(dirname(if (is.null(sys.frame(1L)$ofile)) "." else sys.frame(1L)$ofile),
                   "helpers_utils.R"))

#' The languages, in the order of the switch; the first is the default.
APTER_LANGS <- c("no", "en")

.i18n <- new.env(parent = emptyenv())
.i18n$misses <- character(0)

#' The translation table (read once per process).
#'
#' @param path inputs/i18n/translations.csv by default (project root from
#'   get_root_dir() when available, else option apter.root / APTER_ROOT).
i18n_table <- function(path = NULL) {
  if (!is.null(.i18n$table) && is.null(path)) return(.i18n$table)
  if (is.null(path)) {
    root <- if (exists("get_root_dir", mode = "function")) get_root_dir()
            else or_default(getOption("apter.root"), Sys.getenv("APTER_ROOT"))
    path <- file.path(root, "inputs", "i18n", "translations.csv")
  }
  t <- utils::read.csv(path, encoding = "UTF-8", fileEncoding = "UTF-8", colClasses = "character",
                       na.strings = character(0), check.names = FALSE)
  for (n in c("id", "kind", "en", "no")) t[[n]] <- enc2utf8(t[[n]])
  .i18n$table <- t
  .i18n$by_id <- stats::setNames(seq_len(nrow(t)), t$id)
  # Exact matches by English text (msg and value rows; ui rows too, so an
  # English column label built from ui text also translates).
  ph <- grepl(.I18N_PH, t$en, perl = TRUE)
  ex <- t[!ph, , drop = FALSE]
  ex <- ex[order(match(ex$kind, c("value", "msg", "ui"))), , drop = FALSE]
  ex <- ex[!duplicated(ex$en), , drop = FALSE]
  .i18n$exact <- stats::setNames(ex$no, ex$en)
  .i18n$templates <- .i18n_templates(t[t$kind %in% c("msg", "value") & ph, , drop = FALSE])
  .i18n$misses <- character(0)
  t
}

#' Is `lang` a language of the app? Anything else falls back to the default.
i18n_lang <- function(lang) {
  lang <- as.character(or_default(lang, APTER_LANGS[1L]))[1L]
  if (lang %in% APTER_LANGS) lang else APTER_LANGS[1L]
}

#' A `ui` text by id, filled with sprintf(). Unknown ids are an error.
tr <- function(id, ..., lang = "en") {
  t <- i18n_table(); i <- .i18n$by_id[id]
  if (anyNA(i)) stop(sprintf("Unknown text id(s): %s", paste(id[is.na(i)], collapse = ", ")), call. = FALSE)
  s <- t[[i18n_lang(lang)]][i]
  if (...length() == 0L) return(unname(s))
  # Decimals go in as fmt_num() text (%s): sprintf knows no decimal comma.
  unname(sprintf(s, ...))
}

#' Data values (species, types, volume methods, "Total", column names) to
#' `lang`; exact matches only, unknown values unchanged. Keeps NA.
tr_value <- function(x, lang = "en") {
  if (i18n_lang(lang) == "en" || length(x) == 0L) return(x)
  i18n_table()
  y <- as.character(x)
  hit <- !is.na(y) & y %in% names(.i18n$exact)
  y[hit] <- .i18n$exact[y[hit]]
  y
}

#' English backend text to `lang`: exact match first, then the message
#' templates (most specific first). Captured arguments are translated in
#' turn (a reason inside "3 stems dropped: <reason>."), numbers get the
#' language's decimal mark. Unmatched text is returned unchanged and
#' recorded (i18n_misses()).
tr_msg <- function(text, lang = "en") {
  if (i18n_lang(lang) == "en" || length(text) == 0L) return(text)
  i18n_table()
  vapply(as.character(text), function(s) .tr_one(s, lang, depth = 0L), "", USE.NAMES = FALSE)
}

#' Texts tr_msg() could not translate since the table was (re)loaded.
i18n_misses <- function() unique(.i18n$misses)
i18n_reset_misses <- function() .i18n$misses <- character(0)

.tr_one <- function(s, lang, depth) {
  if (is.na(s) || !nzchar(trimws(s))) return(s)
  hit <- .i18n$exact[s]
  if (!is.na(hit)) return(unname(hit))
  # A text of several lines: each line on its own.
  if (grepl("\n", s, fixed = TRUE))
    return(paste(vapply(strsplit(s, "\n", fixed = TRUE)[[1L]], .tr_one, "", lang = lang,
                        depth = depth, USE.NAMES = FALSE), collapse = "\n"))
  for (tp in .i18n$templates) {
    m <- regmatches(s, regexec(tp$re, s, perl = TRUE))[[1L]]
    if (length(m) == 0L) next
    args <- m[-1L]
    args <- vapply(args, function(a) .tr_arg(a, lang, depth), "", USE.NAMES = FALSE)
    return(do.call(sprintf, c(list(tp$out), as.list(args))))
  }
  # "<name or check>: <message>" (the .hpr import's per-product warnings,
  # the price checks' "check: detail"): the message translated, the part
  # before it too if the table knows it (else it is a name and stays).
  cut <- regexpr(": ", s, fixed = TRUE)
  if (cut > 0L && depth < 2L) {
    head <- substring(s, 1L, cut - 1L); rest <- substring(s, cut + 2L)
    n0 <- length(.i18n$misses)
    rest_t <- .tr_one(rest, lang, depth + 1L)
    if (!identical(rest_t, rest) || !grepl("[A-Za-z]{3,}", rest)) {
      head_t <- .tr_arg(head, lang, 0L)
      return(paste0(head_t, ": ", rest_t))
    }
    length(.i18n$misses) <- n0
  }
  if (depth == 0L && grepl("[A-Za-z]{3,}", s)) .i18n$misses <- c(.i18n$misses, s)
  s
}

# An argument captured from a template: a number in the language's format,
# else translated if the table knows it (up to three levels deep).
.tr_arg <- function(a, lang, depth) {
  if (grepl("^-?[0-9]{1,3}(,[0-9]{3})+$", a)) return(gsub(",", " ", a, fixed = TRUE))
  if (grepl("^-?[0-9]+\\.[0-9]+$", a)) return(sub(".", ",", a, fixed = TRUE))
  if (grepl("^-?[0-9]+$", a) || depth >= 3L) return(a)
  hit <- .i18n$exact[a]
  if (!is.na(hit)) return(unname(hit))
  if (depth < 3L) {
    for (tp in .i18n$templates) {
      m <- regmatches(a, regexec(tp$re, a, perl = TRUE))[[1L]]
      if (length(m) == 0L) next
      args <- vapply(m[-1L], function(x) .tr_arg(x, lang, depth + 1L), "", USE.NAMES = FALSE)
      return(do.call(sprintf, c(list(tp$out), as.list(args))))
    }
    # A list ("1 with dbh below 5.0 cm, 1 with height below 4.5 m"): item by item.
    if (grepl(", ", a, fixed = TRUE)) {
      items <- strsplit(a, ", ", fixed = TRUE)[[1L]]
      out <- vapply(items, .tr_arg, "", lang = lang, depth = depth, USE.NAMES = FALSE)
      if (!identical(out, items)) return(paste(out, collapse = ", "))
    }
  }
  a
}

# sprintf placeholders: %s, %d, %g, %.1f, %5.2f, %1$s; "%%" is a literal %.
.I18N_PH <- "%(?:[0-9]+\\$)?[-+ 0#]*[0-9]*(?:\\.[0-9]+)?[sdifg]"

# Templates -> anchored regexes, most literal text first (the most specific
# wins), and the Norwegian template with every placeholder as %s (the
# arguments arrive as text).
.i18n_templates <- function(t) {
  if (nrow(t) == 0L) return(list())
  esc <- function(x) gsub("([][{}()*+?.\\\\^$|])", "\\\\\\1", x, perl = TRUE)
  out <- lapply(seq_len(nrow(t)), function(i) {
    en <- t$en[i]
    parts <- regmatches(en, gregexpr(.I18N_PH, en, perl = TRUE))[[1L]]
    lits  <- strsplit(en, .I18N_PH, perl = TRUE)[[1L]]
    if (length(lits) < length(parts) + 1L) lits <- c(lits, rep("", length(parts) + 1L - length(lits)))
    grp <- vapply(parts, function(p) switch(substring(p, nchar(p)),
                                            d = , i = "(-?[0-9][0-9,]*)",
                                            f = , g = "(-?[0-9][0-9.,eE+-]*|NA|Inf|NaN)",
                                            "(.*?)"), "")
    re <- esc(gsub("%%", "%", lits[1L], fixed = TRUE))
    for (j in seq_along(parts))
      re <- paste0(re, grp[j], esc(gsub("%%", "%", lits[j + 1L], fixed = TRUE)))
    no <- gsub("%((?:[0-9]+\\$)?)[-+ 0#]*[0-9]*(?:\\.[0-9]+)?[sdifg]", "%\\1s", t$no[i], perl = TRUE)
    list(re = paste0("^(?s)", re, "$"), out = no,
         weight = nchar(gsub(.I18N_PH, "", en, perl = TRUE)))
  })
  out[order(-vapply(out, `[[`, 0, "weight"))]
}

#' Column names and character values of a table in `lang` (exact matches).
i18n_df <- function(df, lang = "en") {
  if (i18n_lang(lang) == "en") return(df)
  names(df) <- tr_value(names(df), lang)
  for (n in seq_along(df)) if (is.character(df[[n]]) || is.factor(df[[n]]))
    df[[n]] <- tr_value(as.character(df[[n]]), lang)
  df
}

#' A text to render later, in the language of the moment: the id of a `ui`
#' text and its arguments (themselves texts, numbers or English backend
#' text).
i18n_text <- function(id, ...) structure(list(id = id, args = list(...)), class = "apter_text")

#' Render an i18n_text(), English backend text (tr_msg()), or a list of
#' them (one string each).
render_text <- function(x, lang = "en") {
  if (is.null(x)) return(NULL)
  if (inherits(x, "apter_text"))
    return(do.call(tr, c(list(x$id), lapply(x$args, function(a)
      if (is.numeric(a)) a else render_text(a, lang)), list(lang = lang))))
  if (is.list(x)) return(vapply(x, function(e) paste(render_text(e, lang), collapse = "\n"), ""))
  if (is.character(x)) return(tr_msg(x, lang))
  x
}

#' Numbers in the language's format: English "1,234.5", Norsk "1 234,5"
#' (non-breaking space). NA as `na`.
fmt_num <- function(x, digits = 0L, lang = "en", big = TRUE, na = "–") {
  no <- i18n_lang(lang) == "no"
  out <- formatC(round(as.numeric(x), digits), format = "f", digits = digits,
                 big.mark = if (!big) "" else if (no) " " else ",",
                 decimal.mark = if (no) "," else ".")
  out[is.na(x)] <- na
  trimws(out)
}

#' Decimal points inside labels ("5–9.9") as the language writes them
#' ("5–9,9" in Norsk).
i18n_decimals <- function(x, lang = "en") {
  if (i18n_lang(lang) == "en") return(x)
  gsub("([0-9])\\.([0-9])", "\\1,\\2", x, perl = TRUE)
}

#' A result table as CSV in the language's format: English as before
#' (write.csv, UTF-8); Norsk with ";" and a decimal comma (write.csv2) and a
#' UTF-8 byte-order mark, so Norwegian Excel opens it directly.
write_download_csv <- function(df, file, lang = "en", na = "NA") {
  if (i18n_lang(lang) == "en")
    return(utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8", na = na))
  con <- file(file, open = "wb"); on.exit(close(con))
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  tc <- textConnection("csv_lines", "w", local = TRUE)
  utils::write.csv2(df, tc, row.names = FALSE, na = na)
  close(tc)
  writeBin(charToRaw(enc2utf8(paste0(paste(csv_lines, collapse = "\n"), "\n"))), con)
  invisible(file)
}
