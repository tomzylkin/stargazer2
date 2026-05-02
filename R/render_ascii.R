# ASCII / plain-text rendering layer for stargazer2.
#
# render_ascii() converts a table_data list into a plain-text table
# matching the style of the original stargazer text output:
#   - === top/bottom double rules, --- internal single rules
#   - no vertical bars
#   - values centred within each column
#   - column numbers suppressed for single-model tables
#   - column widths are content-adaptive (matches original stargazer)
#
# Width determination is a strict two-pass process:
#   Pass 1  compute_col_widths() scans EVERY cell across ALL row types
#           (coef values, SE/CI strings, FE indicators, fit statistics, and
#           explicit CI bounds from table_data$ci_col_widths) before any
#           output is produced.
#   Pass 2  render_ascii() emits lines using the fixed widths from Pass 1.

#' Render a table_data object as a plain-text string
#'
#' Internal; called by \code{\link{stargazer}} when \code{type = "text"}.
#'
#' @param table_data   List returned by \code{\link{format_table}}.
#' @param title        Table title string.
#' @param dep.var.caption Caption above the dependent-variable line.
#' @param column.labels Optional character vector of column labels.
#' @param notes        Additional note strings.
#' @param notes.append Logical; if \code{FALSE}, replace the default note.
#' @keywords internal
render_ascii <- function(table_data,
                         title           = "",
                         dep.var.caption = "Dependent variable:",
                         column.labels   = NULL,
                         notes           = NULL,
                         notes.append    = TRUE) {

  nc <- table_data$n_cols

  # --- PASS 1: determine all column widths before producing any output ------

  # 1a. Label column width: max of all row labels and "Note:" prefix
  all_rows <- c(table_data$coef_rows, table_data$fe_rows, table_data$stat_rows)
  label_w  <- max(
    nchar(vapply(all_rows, function(r) strip_latex(r$label), character(1L))),
    nchar("Note:"),
    1L, na.rm = TRUE
  )

  # 1b. Per-value-column widths: complete scan of every cell in every column.
  col_w <- compute_col_widths(table_data, nc)

  note_text <- build_ascii_note(table_data, notes, notes.append)

  val_area <- sum(col_w) + nc - 1L   # value-column area width (cols + separators)
  total_w  <- label_w + 1L + val_area # total table width (label + sep + values)

  # --- PASS 2: render using fixed widths ------------------------------------

  dbl_line <- strrep("=", total_w)
  sep_line <- strrep("-", total_w)
  # Partial rule spanning only the value-column area (mimics LaTeX \cline)
  cline    <- paste0(strrep(" ", label_w + 1L), strrep("-", val_area))

  lines <- character(0L)

  # --- Title ---
  if (nchar(title) > 0L) {
    lines <- c(lines, "", paste0("  ", title))
  }

  lines <- c(lines, dbl_line)

  # --- Dependent variable caption (centred in value-column area) ---
  # Original stargazer: caption on its own line, centred over value cols only,
  # with label area filled by spaces.
  lines <- c(lines,
    paste0(strrep(" ", label_w + 1L), centre_in(dep.var.caption, val_area))
  )
  lines <- c(lines, cline)

  # --- Dep var name(s): centred in value-column area ---
  dep_vars <- strip_latex(table_data$dep_vars)
  if (length(unique(dep_vars)) == 1L) {
    lines <- c(lines,
      paste0(strrep(" ", label_w + 1L), centre_in(dep_vars[1L], val_area))
    )
  } else {
    dep_cells <- mapply(function(v, w) centre_in(v, w), dep_vars, col_w,
                        SIMPLIFY = TRUE)
    lines <- c(lines,
      paste0(strrep(" ", label_w + 1L), paste(dep_cells, collapse = " "))
    )
  }

  # --- Model-type row (when types or dep vars differ) ---
  col_labels <- if (!is.null(column.labels)) {
    column.labels
  } else if (table_data$show_model_row) {
    table_data$model_labels
  } else {
    NULL
  }
  if (!is.null(col_labels)) {
    col_labels <- pad_to(col_labels, nc, "")
    type_cells <- mapply(function(v, w) centre_in(v, w), col_labels, col_w,
                         SIMPLIFY = TRUE)
    lines <- c(lines,
      paste0(strrep(" ", label_w + 1L), paste(type_cells, collapse = " "))
    )
  }

  # --- Column numbers: only for multi-model tables ---
  if (nc > 1L) {
    num_cells <- mapply(function(v, w) centre_in(v, w),
                        table_data$col_numbers, col_w, SIMPLIFY = TRUE)
    lines <- c(lines,
      paste0(strrep(" ", label_w + 1L), paste(num_cells, collapse = " "))
    )
  }

  lines <- c(lines, sep_line)

  # --- Coefficient rows ---
  for (row in table_data$coef_rows) {
    lines <- c(lines, format_ascii_row(row$label, row$values, label_w, col_w))
    if (!is.null(row$se_values)) {
      lines <- c(lines, format_ascii_row("", row$se_values, label_w, col_w))
    }
    lines <- c(lines, strrep(" ", total_w))  # blank spacer between covariates
  }

  # --- Fixed-effects indicator rows ---
  if (length(table_data$fe_rows) > 0L) {
    lines <- c(lines, sep_line)
    for (row in table_data$fe_rows) {
      lines <- c(lines, format_ascii_row(row$label, row$values, label_w, col_w))
    }
  }

  # --- Fit-statistic rows ---
  lines <- c(lines, sep_line)
  for (row in table_data$stat_rows) {
    lines <- c(lines, format_ascii_row(row$label, row$values, label_w, col_w))
  }

  lines <- c(lines, dbl_line)

  # --- Notes ---
  if (nchar(note_text) > 0L) {
    # "Note:" left-justified in label column; note text word-wrapped to fit
    # within total_w, with continuation lines indented to align with the
    # start of the first note line (label_w + 2 spaces).
    note_indent <- label_w + 1L          # chars before the note text begins
    note_w      <- total_w - note_indent # available width for note text
    wrapped     <- wrap_note(note_text, note_w)
    note_label  <- formatC("Note:", width = label_w, flag = "-")
    indent_str  <- strrep(" ", note_indent)
    for (i in seq_along(wrapped)) {
      prefix <- if (i == 1L) paste0(note_label, " ") else paste0(indent_str, " ")
      lines  <- c(lines, paste0(prefix, wrapped[i]))
    }
  }

  paste(lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Pass 1: complete column-width computation
# ---------------------------------------------------------------------------

# compute_col_widths() is called once before any rendering begins.
# It scans EVERY cell type to determine the minimum column widths that
# can accommodate all content without truncation:
#   - coefficient values (with significance stars)
#   - SE strings  (parenthesised: "(0.013)")
#   - CI strings  (bracketed: "[-0.064, -0.019]")
#   - fixed-effect indicator cells ("Yes" / "No" / "")
#   - fit-statistic cells
#   - model labels and dep-var names (in column headers)
#   - column-number labels "(1)", "(2)", ...
#   - explicit CI widths precomputed from raw bounds in format_table()
#     (handles CI rows that appear late in the row list, e.g. a single-ATT
#      staggered_result row at the bottom of an event-study table)
compute_col_widths <- function(table_data, nc) {
  vapply(seq_len(nc), function(c) {
    cells <- character(0L)

    # Coefficient values and their SE / CI sub-rows
    for (row in table_data$coef_rows) {
      cells <- c(cells, strip_latex(row$values[c]))
      if (!is.null(row$se_values)) {
        cells <- c(cells, strip_latex(row$se_values[c]))
      }
    }

    # Fixed-effects indicator rows
    for (row in table_data$fe_rows) {
      cells <- c(cells, strip_latex(row$values[c]))
    }

    # Fit-statistic rows
    for (row in table_data$stat_rows) {
      cells <- c(cells, strip_latex(row$values[c]))
    }

    # Column-number label (multi-model tables only)
    if (nc > 1L) cells <- c(cells, table_data$col_numbers[c])

    # Model label and dep-var name in the column header
    cells <- c(cells,
               strip_latex(table_data$model_labels[c]),
               strip_latex(table_data$dep_vars[c]))

    # Maximum content width from scanned cells
    w <- max(nchar(cells), 1L, na.rm = TRUE)

    # Enforce minimum width from CI bracket precomputation.
    # ci_col_widths[c] == nchar(format_ci(lo, hi, digits)) for the widest CI
    # in column c.  Because format_ci() now uses sprintf() directly (no LaTeX
    # markup), ci_col_widths and the scanned se_values nchar() values are
    # guaranteed to agree.  The max() here is a safety net for CI rows that
    # appear late in the row list (e.g. single-ATT at the bottom of a long
    # event-study table) and may not be reached by the se_values scan above.
    if (!is.null(table_data$ci_col_widths)) {
      w <- max(w, table_data$ci_col_widths[c])
    }

    w
  }, integer(1L))
}

# ---------------------------------------------------------------------------
# ASCII helpers
# ---------------------------------------------------------------------------

strip_latex <- function(x) {
  x <- gsub("\\$\\^\\{([^}]*)\\}\\$", "\\1", x)   # $^{***}$ -> ***
  x <- gsub("\\$-\\$", "-", x)                      # $-$ -> -
  x <- gsub("\\$<\\$", "<", x)                      # $<$ -> <
  x <- gsub("\\$>\\$", ">", x)                      # $>$ -> >
  x <- gsub("\\$[^$]*\\$", "", x)                   # remaining $...$ math
  # Un-escape LaTeX special characters (from latex_escape())
  x <- gsub("\\\\_", "_", x, fixed = FALSE)          # \_ -> _
  x <- gsub("\\\\%", "%", x, fixed = FALSE)          # \% -> %
  x <- gsub("\\\\&", "&", x, fixed = FALSE)          # \& -> &
  x <- gsub("\\\\#", "#", x, fixed = FALSE)          # \# -> #
  x <- gsub("\\\\textbackslash\\{\\}", "\\", x, fixed = FALSE)  # \textbackslash{} -> \
  x <- gsub("\\\\textit\\{([^}]*)\\}", "\\1", x)    # \textit{x} -> x
  x <- gsub("\\\\[a-zA-Z]+\\{([^}]*)\\}", "\\1", x) # \cmd{x} -> x
  x <- gsub("\\\\[a-zA-Z]+", "", x)                 # bare \cmd
  trimws(x)
}

# format_ascii_row: col_w is a vector of per-column widths (length nc)
format_ascii_row <- function(label, values, label_w, col_w) {
  label_cell <- formatC(strip_latex(label), width = label_w, flag = "-")
  val_cells  <- mapply(function(v, w) centre_in(strip_latex(v), w),
                       values, col_w, SIMPLIFY = TRUE)
  paste0(label_cell, " ", paste(val_cells, collapse = " "))
}

centre_in <- function(text, width) {
  n   <- nchar(text)
  pad <- max(0L, width - n)
  left  <- floor(pad / 2L)
  right <- pad - left
  paste0(strrep(" ", left), text, strrep(" ", right))
}

pad_to <- function(x, n, fill) {
  if (length(x) >= n) return(x[seq_len(n)])
  c(x, rep(fill, n - length(x)))
}

build_ascii_note <- function(table_data, notes, notes.append) {
  if (notes.append || is.null(notes)) {
    se_raw  <- format_se_note(table_data$se_notes, table_data$col_numbers)
    se_part <- if (!is.null(se_raw)) strip_latex(se_raw) else NULL
    star_part <- strip_latex(table_data$star_note)
    parts <- c(se_part, star_part, notes)
    parts <- Filter(function(x) !is.null(x) && nchar(x) > 0L, parts)
    paste(parts, collapse = "; ")
  } else {
    paste(notes, collapse = "; ")
  }
}

# Word-wrap note text to at most `width` characters per line, breaking only
# at space boundaries.  Returns a character vector of wrapped lines.
# If a single token exceeds `width` it is placed on its own line unbroken.
wrap_note <- function(text, width) {
  if (nchar(text) <= width) return(text)
  tokens <- strsplit(text, " ", fixed = TRUE)[[1L]]
  lines  <- character(0L)
  cur    <- ""
  for (tok in tokens) {
    candidate <- if (nchar(cur) == 0L) tok else paste0(cur, " ", tok)
    if (nchar(candidate) <= width) {
      cur <- candidate
    } else {
      if (nchar(cur) > 0L) lines <- c(lines, cur)
      cur <- tok   # start new line with this token even if it overflows
    }
  }
  if (nchar(cur) > 0L) lines <- c(lines, cur)
  lines
}
