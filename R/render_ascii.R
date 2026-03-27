# ASCII / plain-text rendering layer for stargazer2.
#
# render_ascii() converts a table_data list into a plain-text table
# matching the style of the original stargazer text output:
#   - === top/bottom double rules, --- internal single rules
#   - no vertical bars
#   - values centred within each column
#   - column numbers suppressed for single-model tables

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

  # Label column width: widest stripped label, minimum 20
  all_rows <- c(table_data$coef_rows, table_data$fe_rows, table_data$stat_rows)
  label_w  <- max(
    nchar(vapply(all_rows, function(r) strip_latex(r$label), character(1L))),
    20L, na.rm = TRUE
  )

  # Value column width: 24 per column minimum
  val_w    <- 24L
  total_w  <- label_w + nc * val_w

  dbl_line <- strrep("=", total_w)
  sep_line <- strrep("-", total_w)
  # Partial rule spanning only the value-column area (mimics LaTeX \cline)
  cline    <- paste0(strrep(" ", label_w), strrep("-", nc * val_w))

  lines <- character(0L)

  # --- Title ---
  if (nchar(title) > 0L) {
    lines <- c(lines, "", paste0("  ", title))
  }

  lines <- c(lines, dbl_line)

  # --- Dependent variable caption (centred across full width) ---
  dep_str <- paste(unique(strip_latex(table_data$dep_vars)), collapse = ", ")
  lines   <- c(lines, centre_in(paste0(dep.var.caption, " ", dep_str), total_w))
  lines   <- c(lines, cline)

  # --- Dep var name(s) ---
  dep_vars <- strip_latex(table_data$dep_vars)
  if (length(unique(dep_vars)) == 1L) {
    lines <- c(lines, paste0(
      strrep(" ", label_w),
      centre_in(dep_vars[1L], nc * val_w)
    ))
  } else {
    dep_cells <- vapply(dep_vars, function(v) centre_in(v, val_w), character(1L))
    lines <- c(lines, paste0(strrep(" ", label_w), paste(dep_cells, collapse = "")))
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
    type_cells <- vapply(col_labels, function(v) centre_in(v, val_w), character(1L))
    lines <- c(lines, paste0(strrep(" ", label_w), paste(type_cells, collapse = "")))
  }

  # --- Column numbers: only for multi-model tables ---
  if (nc > 1L) {
    num_cells <- vapply(table_data$col_numbers, function(v) centre_in(v, val_w),
                        character(1L))
    lines <- c(lines, paste0(strrep(" ", label_w), paste(num_cells, collapse = "")))
  }

  lines <- c(lines, sep_line)

  # --- Coefficient rows ---
  for (row in table_data$coef_rows) {
    lines <- c(lines, format_ascii_row(row$label, row$values, label_w, val_w))
    if (!is.null(row$se_values)) {
      lines <- c(lines, format_ascii_row("", row$se_values, label_w, val_w))
    }
    lines <- c(lines, strrep(" ", total_w))  # blank spacer between covariates
  }

  # --- Fixed-effects indicator rows ---
  if (length(table_data$fe_rows) > 0L) {
    lines <- c(lines, sep_line)
    for (row in table_data$fe_rows) {
      lines <- c(lines, format_ascii_row(row$label, row$values, label_w, val_w))
    }
  }

  # --- Fit-statistic rows ---
  lines <- c(lines, sep_line)
  for (row in table_data$stat_rows) {
    lines <- c(lines, format_ascii_row(row$label, row$values, label_w, val_w))
  }

  lines <- c(lines, dbl_line)

  # --- Notes ---
  note_text <- build_ascii_note(table_data, notes, notes.append)
  if (nchar(note_text) > 0L) {
    # Right-align note content, matching original stargazer style
    note_prefix <- "Note: "
    pad <- max(0L, total_w - nchar(note_prefix) - nchar(note_text))
    lines <- c(lines, paste0(note_prefix, strrep(" ", pad), note_text))
  }

  paste(lines, collapse = "\n")
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
  x <- gsub("\\\\textit\\{([^}]*)\\}", "\\1", x)    # \textit{x} -> x
  x <- gsub("\\\\[a-zA-Z]+\\{([^}]*)\\}", "\\1", x) # \cmd{x} -> x
  x <- gsub("\\\\[a-zA-Z]+", "", x)                 # bare \cmd
  trimws(x)
}

format_ascii_row <- function(label, values, label_w, val_w) {
  label_cell <- formatC(strip_latex(label), width = label_w, flag = "-")
  val_cells  <- vapply(values, function(v) centre_in(strip_latex(v), val_w),
                       character(1L))
  paste0(label_cell, paste(val_cells, collapse = ""))
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
    informative_se <- Filter(function(x) x != "OLS standard errors",
                             table_data$se_notes)
    se_part   <- if (length(informative_se) > 0L) {
      paste(informative_se, collapse = "; ")
    } else NULL
    star_part <- strip_latex(table_data$star_note)
    parts <- c(se_part, star_part, notes)
    parts <- Filter(function(x) !is.null(x) && nchar(x) > 0L, parts)
    paste(parts, collapse = "; ")
  } else {
    paste(notes, collapse = "; ")
  }
}
