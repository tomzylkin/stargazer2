# LaTeX rendering layer for stargazer2.
#
# render_latex() converts a table_data list (from format_table()) into a
# character string containing a complete LaTeX table environment.
# The output format closely mirrors the original stargazer package.

#' Render a table_data object as a LaTeX string
#'
#' Internal; called by \code{\link{stargazer}}.
#'
#' @param table_data   List returned by \code{\link{format_table}}.
#' @param title        Table caption string.
#' @param label        LaTeX label string (\code{\\label\{...\}}).
#' @param dep.var.caption Caption above the dependent-variable line.
#' @param column.labels Optional character vector of column labels
#'   (overrides model_labels in the header).
#' @param font.size    LaTeX font-size command (e.g. \code{"small"}).
#' @param notes        Additional note strings appended after the SE note.
#' @param notes.append Logical; if \code{FALSE}, replace the default note.
#' @param notes.align  Alignment of the notes cell: \code{"l"}, \code{"c"},
#'   or \code{"r"}.
#' @param notes.label  Label preceding the note text.
#' @keywords internal
render_latex <- function(table_data,
                         title        = "",
                         label        = "",
                         dep.var.caption = "\\textit{Dependent variable:}",
                         column.labels   = NULL,
                         font.size       = NULL,
                         notes           = NULL,
                         notes.append    = TRUE,
                         notes.align     = "r",
                         notes.label     = "\\textit{Note:} ") {

  nc    <- table_data$n_cols   # number of data columns
  ncols <- nc + 1L             # including the label column

  # Helper: build one LaTeX row from a label and a values vector
  make_row <- function(label, values, indent = FALSE) {
    prefix <- if (indent) " " else ""
    cells  <- c(paste0(prefix, label), values)
    paste0(paste(cells, collapse = " & "), " \\\\ ")
  }

  # Helper: blank spacer row — matches original stargazer cell format
  blank_row <- function() {
    paste0("  ", paste(rep("& ", nc), collapse = ""), "\\\\ ")
  }

  lines <- character(0L)

  # --- Opening (leading blank matches original stargazer output) ---
  lines <- c(lines, "")
  lines <- c(lines, "\\begin{table}[!htbp] \\centering ")
  if (!is.null(font.size) && nchar(font.size) > 0L) {
    lines <- c(lines, paste0("  \\", font.size))
  }
  lines <- c(lines, paste0("  \\caption{", title, "} "))
  lines <- c(lines, paste0("  \\label{", label, "} "))

  # Tabular column spec: matches original stargazer (@{\extracolsep{5pt}} style)
  col_spec <- paste0("@{\\extracolsep{5pt}}l", paste(rep("c", nc), collapse = ""))
  lines <- c(lines, paste0("\\begin{tabular}{", col_spec, "} "))

  # Top double rule
  lines <- c(lines, "\\\\[-1.8ex]\\hline ")
  lines <- c(lines, "\\hline \\\\[-1.8ex] ")

  # --- Header ---
  lines <- c(lines, build_latex_header(table_data, column.labels, dep.var.caption))

  # --- Coefficient rows ---
  lines <- c(lines, "\\hline \\\\[-1.8ex] ")
  for (row in table_data$coef_rows) {
    lines <- c(lines, make_row(row$label, row$values, indent = TRUE))
    lines <- c(lines, make_row("", row$se_values, indent = TRUE))
    # Spacer after every covariate pair (matches original stargazer default)
    if (!table_data$no_space) {
      lines <- c(lines, blank_row())
    }
  }

  # --- Fixed-effects indicator rows (if any) ---
  if (length(table_data$fe_rows) > 0L) {
    lines <- c(lines, "\\hline \\\\[-1.8ex] ")
    for (row in table_data$fe_rows) {
      lines <- c(lines, make_row(row$label, row$values))
    }
    if (!table_data$no_space) {
      lines <- c(lines, blank_row())
    }
  }

  # --- Fit-statistic rows ---
  lines <- c(lines, "\\hline \\\\[-1.8ex] ")
  for (row in table_data$stat_rows) {
    lines <- c(lines, make_row(row$label, row$values))
  }

  # Bottom double rule
  lines <- c(lines, "\\hline ")
  lines <- c(lines, "\\hline \\\\[-1.8ex] ")

  # --- Notes ---
  note_lines <- build_latex_notes(
    table_data, notes, notes.append, notes.align, notes.label, nc
  )
  lines <- c(lines, note_lines)

  # Closing
  lines <- c(lines, "\\end{tabular} ")
  lines <- c(lines, "\\end{table} ")

  paste(lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Header builder
# ---------------------------------------------------------------------------

build_latex_header <- function(table_data, column.labels, dep.var.caption) {
  nc          <- table_data$n_cols
  dep_vars    <- latex_escape(table_data$dep_vars)
  col_numbers <- table_data$col_numbers
  lines       <- character(0L)

  # "Dependent variable:" caption spanning all data columns
  lines <- c(lines, paste0(
    " & \\multicolumn{", nc, "}{c}{", dep.var.caption, "} \\\\ "
  ))
  lines <- c(lines, paste0("\\cline{2-", nc + 1L, "} "))

  # Dependent variable name(s) — prefixed with spacing command inline
  if (length(unique(dep_vars)) == 1L) {
    lines <- c(lines, paste0(
      "\\\\[-1.8ex] & \\multicolumn{", nc, "}{c}{", dep_vars[1L], "} \\\\ "
    ))
  } else {
    lines <- c(lines, paste0(
      "\\\\[-1.8ex] & ", paste(dep_vars, collapse = " & "), " \\\\ "
    ))
  }

  # Model-type row (shown when types or dep vars differ, or user specifies labels)
  col_labels <- if (!is.null(column.labels)) {
    column.labels
  } else if (table_data$show_model_row) {
    paste0("\\textit{", table_data$model_labels, "}")
  } else {
    NULL
  }

  if (!is.null(col_labels)) {
    if (length(col_labels) < nc) {
      col_labels <- c(col_labels, rep("", nc - length(col_labels)))
    }
    lines <- c(lines, paste0(
      "\\\\[-1.8ex] & ", paste(col_labels[seq_len(nc)], collapse = " & "), " \\\\ "
    ))
  }

  # Column numbers
  lines <- c(lines, paste0(
    "\\\\[-1.8ex] & ", paste(col_numbers, collapse = " & "), "\\\\ "
  ))

  lines
}

# ---------------------------------------------------------------------------
# Notes builder
# ---------------------------------------------------------------------------

build_latex_notes <- function(table_data, notes, notes.append, notes.align,
                              notes.label, nc) {
  # Build the note text
  if (notes.append || is.null(notes)) {
    # Suppress SE labels that carry no useful info for the reader
    # ("OLS standard errors" is implicit for lm — omit to match original stargazer)
    informative_se <- Filter(function(x) x != "OLS standard errors",
                             table_data$se_notes)
    se_part <- if (length(informative_se) > 0L) {
      paste(informative_se, collapse = "; ")
    } else {
      NULL
    }
    star_part <- table_data$star_note
    parts <- c(se_part, star_part, notes)
    parts <- Filter(function(x) !is.null(x) && nchar(x) > 0L, parts)
    note_text <- paste(parts, collapse = "; ")
  } else {
    note_text <- paste(notes, collapse = "; ")
  }

  # Note content is NOT wrapped in \textit{} — matches original stargazer
  paste0(
    notes.label,
    " & \\multicolumn{", nc, "}{", notes.align, "}{",
    note_text,
    "} \\\\ "
  )
}
