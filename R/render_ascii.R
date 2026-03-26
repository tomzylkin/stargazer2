# ASCII / plain-text rendering layer for stargazer2.
#
# render_ascii() converts a table_data list into a plain-text table,
# suitable for display in the R console or text files.

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

  # Determine column widths by scanning all cell content
  all_rows   <- c(table_data$coef_rows, table_data$fe_rows, table_data$stat_rows)
  label_w    <- max(nchar(vapply(all_rows, `[[`, character(1L), "label")),
                    20L, na.rm = TRUE)
  val_w      <- 14L  # fixed width for data cells

  sep_line  <- paste0("+", paste(rep("-", label_w + 2L), collapse = ""),
                      paste(rep(paste0("+", strrep("-", val_w + 2L)), nc),
                            collapse = ""), "+")
  dbl_line  <- gsub("-", "=", sep_line)

  lines <- character(0L)

  # Title
  if (nchar(title) > 0L) {
    lines <- c(lines, "", paste0("  ", title))
  }

  lines <- c(lines, dbl_line)

  # Dependent variable caption
  dep_str <- paste(unique(table_data$dep_vars), collapse = ", ")
  lines <- c(lines, centre_cell(
    paste0(dep.var.caption, " ", dep_str),
    label_w + nc * (val_w + 3L) + 1L
  ))

  # Column headers
  col_labels <- if (!is.null(column.labels)) {
    column.labels
  } else if (table_data$show_model_row) {
    table_data$model_labels
  } else {
    table_data$col_numbers
  }
  col_labels <- pad_to(col_labels, nc, "")
  header_row <- format_ascii_row("", col_labels, label_w, val_w)
  lines <- c(lines, sep_line, header_row)

  if (table_data$show_model_row) {
    num_row <- format_ascii_row("", table_data$col_numbers, label_w, val_w)
    lines <- c(lines, num_row)
  }

  lines <- c(lines, sep_line)

  # Coefficient rows
  for (row in table_data$coef_rows) {
    lines <- c(lines, format_ascii_row(row$label, row$values, label_w, val_w))
    if (!is.null(row$se_values)) {
      lines <- c(lines, format_ascii_row("", row$se_values, label_w, val_w))
    }
  }

  # FE rows
  if (length(table_data$fe_rows) > 0L) {
    lines <- c(lines, sep_line)
    for (row in table_data$fe_rows) {
      lines <- c(lines, format_ascii_row(row$label, row$values, label_w, val_w))
    }
  }

  # Stat rows
  lines <- c(lines, sep_line)
  for (row in table_data$stat_rows) {
    lines <- c(lines, format_ascii_row(row$label, row$values, label_w, val_w))
  }

  lines <- c(lines, dbl_line)

  # Notes
  note_parts <- if (notes.append || is.null(notes)) {
    c(table_data$se_notes, table_data$star_note, notes)
  } else {
    notes
  }
  note_parts <- note_parts[nchar(note_parts) > 0L]
  if (length(note_parts) > 0L) {
    lines <- c(lines, paste0("Note: ", paste(note_parts, collapse = "; ")))
  }

  paste(lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# ASCII helpers
# ---------------------------------------------------------------------------

strip_latex <- function(x) {
  x <- gsub("\\$\\^\\{([^}]*)\\}\\$", "\\1", x)  # $^{***}$ -> ***
  x <- gsub("\\$-\\$", "-", x)                     # $-$ -> -
  x <- gsub("\\$[^$]*\\$", "", x)                  # remaining $...$ math
  x <- gsub("\\\\textit\\{([^}]*)\\}", "\\1", x)   # \textit{x} -> x
  x <- gsub("\\\\[a-zA-Z]+\\{([^}]*)\\}", "\\1", x) # \cmd{x} -> x
  x <- gsub("\\\\[a-zA-Z]+", "", x)                # bare \cmd
  trimws(x)
}

format_ascii_row <- function(label, values, label_w, val_w) {
  label_cell <- formatC(strip_latex(label), width = label_w, flag = "-")
  val_cells  <- vapply(values, function(v) {
    # Strip LaTeX markup for ASCII display
    v <- strip_latex(v)
    formatC(v, width = val_w, flag = "-")
  }, character(1L))
  paste0("| ", label_cell, " | ",
         paste(val_cells, collapse = " | "), " |")
}

centre_cell <- function(text, total_width) {
  pad <- max(0L, total_width - nchar(text))
  left  <- floor(pad / 2L)
  right <- pad - left
  paste0(strrep(" ", left), text, strrep(" ", right))
}

pad_to <- function(x, n, fill) {
  if (length(x) >= n) return(x[seq_len(n)])
  c(x, rep(fill, n - length(x)))
}
