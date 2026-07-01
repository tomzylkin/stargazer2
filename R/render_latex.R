# LaTeX rendering layer for stargazer2.
#
# render_latex() converts a table_data list (from format_table()) into a
# complete LaTeX table environment string.  The output style is controlled by
# the `style` argument; see stargazer() for the user-facing documentation.

#' Render a table_data object as a LaTeX string
#'
#' Internal; called by \code{\link{stargazer}}.
#'
#' @param table_data   List returned by \code{\link{format_table}}.
#' @param title        Table caption string.
#' @param label        LaTeX label string (\code{\\label\{...\}}).
#' @param dep.var.caption Caption above the dependent-variable line (empty
#'   string suppresses the row entirely).
#' @param column.labels Optional character vector of column labels.
#' @param font.size    LaTeX font-size command (e.g. \code{"small"}).
#' @param notes        Additional note strings appended after the SE note.
#' @param notes.append Logical; if \code{FALSE}, replace the default note.
#' @param notes.align  Alignment of the notes cell.
#' @param notes.label  Label preceding the note text.
#' @param style        One of \code{"stargazer2"}, \code{"stargazer"},
#'   \code{"aer"}, \code{"qje"}.
#' @keywords internal
render_latex <- function(table_data,
                         title           = "",
                         label           = "",
                         dep.var.caption = "\\textit{Dependent variable:}",
                         column.labels   = NULL,
                         font.size       = NULL,
                         notes           = NULL,
                         notes.append    = TRUE,
                         notes.align     = "l",
                         notes.label     = "\\textit{Note:} ",
                         style           = "stargazer2") {

  nc    <- table_data$n_cols
  ncols <- nc + 1L                       # all columns including the label col
  orig  <- identical(style, "stargazer") # shorthand for the original style

  make_row <- function(label, values, indent = FALSE) {
    prefix <- if (indent) " " else ""
    cells  <- c(paste0(prefix, label), values)
    paste0(paste(cells, collapse = " & "), " \\\\ ")
  }

  blank_row <- function() {
    paste0("  ", paste(rep("& ", nc), collapse = ""), "\\\\ ")
  }

  lines <- character(0L)

  # --- Opening ---
  lines <- c(lines, "")
  lines <- c(lines, "\\begin{table}[!htbp] \\centering ")
  if (!is.null(font.size) && nchar(font.size) > 0L) {
    lines <- c(lines, paste0("  \\", font.size))
  }
  lines <- c(lines, paste0("  \\caption{", title, "} "))
  lines <- c(lines, paste0("  \\label{", label, "} "))

  col_spec <- paste0("@{\\extracolsep{5pt}}l", paste(rep("c", nc), collapse = ""))
  lines <- c(lines, paste0("\\begin{tabular}{", col_spec, "} "))

  # --- Top rule ---
  if (orig) {
    lines <- c(lines, "\\\\[-1.8ex]\\hline ")
    lines <- c(lines, "\\hline \\\\[-1.8ex] ")
  } else {
    lines <- c(lines, "\\hline ")
  }

  # --- Header ---
  lines <- c(lines,
    build_latex_header(table_data, column.labels, dep.var.caption, style))

  # --- Coefficient rows ---
  lines <- c(lines, if (orig) "\\hline \\\\[-1.8ex] " else "\\hline ")
  n_coef <- length(table_data$coef_rows)
  for (i in seq_len(n_coef)) {
    row <- table_data$coef_rows[[i]]
    lines <- c(lines, make_row(row$label, row$values, indent = TRUE))
    lines <- c(lines, make_row("", row$se_values, indent = TRUE))
    if (orig && !table_data$no_space && i < n_coef) {
      lines <- c(lines, blank_row())
    }
  }

  # --- Fixed-effects indicator rows ---
  if (length(table_data$fe_rows) > 0L) {
    lines <- c(lines, if (orig) "\\hline \\\\[-1.8ex] " else "\\hline ")
    for (row in table_data$fe_rows) {
      lines <- c(lines, make_row(row$label, row$values))
    }
  }

  # --- Fit-statistic rows ---
  lines <- c(lines, if (orig) "\\hline \\\\[-1.8ex] " else "\\hline ")
  for (row in table_data$stat_rows) {
    lines <- c(lines, make_row(row$label, row$values))
  }

  # --- Bottom rule ---
  # aer: single \hline before notes.  stargazer2/qje: double \hline.
  # stargazer (original): double with \\[-1.8ex] trailer.
  if (style == "aer") {
    lines <- c(lines, "\\hline ")
  } else if (orig) {
    lines <- c(lines, "\\hline ")
    lines <- c(lines, "\\hline \\\\[-1.8ex] ")
  } else {
    lines <- c(lines, "\\hline ")
    lines <- c(lines, "\\hline ")
  }

  # --- Notes ---
  note_lines <- build_latex_notes(
    table_data, notes, notes.append, notes.align, notes.label, nc, style
  )
  lines <- c(lines, note_lines)

  lines <- c(lines, "\\end{tabular} ")
  lines <- c(lines, "\\end{table} ")

  paste(lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Header builder
# ---------------------------------------------------------------------------

build_latex_header <- function(table_data, column.labels, dep.var.caption,
                               style = "stargazer2") {
  nc          <- table_data$n_cols
  dep_vars    <- latex_escape(table_data$dep_vars)
  col_numbers <- table_data$col_numbers
  orig        <- identical(style, "stargazer")
  lines       <- character(0L)

  # Row prefix: original style uses \\[-1.8ex] to compress inter-row spacing.
  row_sp <- if (orig) "\\\\[-1.8ex] " else " "

  # "Dependent variable:" caption — omitted when dep.var.caption is empty
  if (!is.null(dep.var.caption) && nchar(dep.var.caption) > 0L) {
    lines <- c(lines, paste0(
      " & \\multicolumn{", nc, "}{c}{", dep.var.caption, "} \\\\ "
    ))
    if (orig) {
      lines <- c(lines, paste0("\\cline{2-", nc + 1L, "} "))
    }
  }

  # Dependent variable name(s)
  if (length(unique(dep_vars)) == 1L && nc > 1L) {
    lines <- c(lines, paste0(
      row_sp, "& \\multicolumn{", nc, "}{c}{", dep_vars[1L], "} \\\\ "
    ))
  } else {
    lines <- c(lines, paste0(
      row_sp, "& ", paste(dep_vars, collapse = " & "), " \\\\ "
    ))
  }

  # Model-type / column-label row
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
      row_sp, "& ", paste(col_labels[seq_len(nc)], collapse = " & "), " \\\\ "
    ))
  }

  # Column numbers (suppressed for single-model tables)
  if (nc > 1L) {
    lines <- c(lines, paste0(
      row_sp, "& ", paste(col_numbers, collapse = " & "), "\\\\ "
    ))
  }

  lines
}

# ---------------------------------------------------------------------------
# Notes builder
# ---------------------------------------------------------------------------

build_latex_notes <- function(table_data, notes, notes.append, notes.align,
                              notes.label, nc, style = "stargazer2") {

  # Choose star note format: text descriptions for aer/qje, p-values otherwise
  the_star_note <- if (style %in% c("aer", "qje")) {
    table_data$star_note_text
  } else {
    table_data$star_note
  }

  if (notes.append || is.null(notes)) {
    se_part   <- format_se_note(table_data$se_notes, table_data$col_numbers)
    sig_parts <- Filter(function(x) !is.null(x) && nchar(x) > 0L,
                        c(se_part, the_star_note))
    sig_block <- paste(sig_parts, collapse = "; ")
    blocks    <- c(sig_block, notes)
  } else {
    blocks <- notes
  }
  blocks <- Filter(function(x) !is.null(x) && nchar(x) > 0L, blocks)
  if (length(blocks) == 0L) return(character(0L))

  if (identical(style, "stargazer")) {
    # Original format: separate label cell + one multicolumn per block.
    return(vapply(seq_along(blocks), function(i) {
      label <- if (i == 1L) notes.label else " "
      paste0(label, " & \\multicolumn{", nc, "}{", notes.align, "}{",
             blocks[i], "} \\\\ ")
    }, character(1L)))
  }

  # Clean format (stargazer2 / aer / qje): single full-width multicolumn
  # spanning all columns.  All blocks joined with "; ".  Trailing period
  # ensured: if the content ends in ";" replace with ".".
  ncols   <- nc + 1L
  content <- paste(blocks, collapse = "; ")
  content <- sub(";\\s*$", ".", content)          # trailing ";" → "."
  if (!grepl("\\.$", content)) content <- paste0(content, ".")

  paste0(
    "\\multicolumn{", ncols, "}{", notes.align, "}{",
    notes.label, content, "} \\\\ "
  )
}
