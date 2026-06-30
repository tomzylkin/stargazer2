# HTML rendering layer for stargazer2.
#
# render_html() converts a table_data list (from format_table()) into a
# character string containing a complete, self-contained <table> element.
# Output is clean semantic HTML: a single <table> with <thead>/<tbody>/<tfoot>,
# colspans for spanning header cells, and minimal inline styles for the
# horizontal rules that give results tables their familiar look.
#
# The table_data cells carry LaTeX markup (e.g. "0.092$^{***}$", "R$^{2}$",
# "$-$"), exactly as consumed by the LaTeX renderer.  latex_to_html() converts
# that markup to HTML; html_escape() guards raw user-supplied text.

#' Render a table_data object as an HTML string
#'
#' Internal; called by \code{\link{stargazer}}.
#'
#' @param table_data   List returned by \code{\link{format_table}}.
#' @param title        Table caption string.
#' @param dep.var.caption Caption above the dependent-variable line.
#' @param column.labels Optional character vector of column labels
#'   (overrides model_labels in the header).
#' @param notes        Additional note strings appended after the SE note.
#' @param notes.append Logical; if \code{FALSE}, replace the default note.
#' @param notes.label  Label preceding the note text.
#' @keywords internal
render_html <- function(table_data,
                        title           = "",
                        dep.var.caption = "Dependent variable:",
                        column.labels   = NULL,
                        notes           = NULL,
                        notes.append    = TRUE,
                        notes.label     = "Note:") {

  nc <- table_data$n_cols

  # A data row: left-aligned label cell + nc centred value cells.
  data_row <- function(label, values, border = FALSE) {
    style <- if (border) " style=\"border-top:1px solid black\"" else ""
    tds <- paste0("<td>", vapply(values, latex_to_html, character(1L)), "</td>",
                  collapse = "")
    paste0("<tr", style, "><td style=\"text-align:left\">",
           latex_to_html(label), "</td>", tds, "</tr>")
  }

  lines <- character(0L)
  lines <- c(lines,
    "<table style=\"border-collapse:collapse; text-align:center\">")
  if (!is.null(title) && nchar(title) > 0L) {
    lines <- c(lines, paste0("<caption>", html_escape(title), "</caption>"))
  }

  # --- Header ---
  lines <- c(lines, "<thead>")
  lines <- c(lines, build_html_header(table_data, column.labels, dep.var.caption))
  lines <- c(lines, "</thead>")

  # --- Body: coefficients, FE rows, fit statistics ---
  lines <- c(lines, "<tbody>")
  n_coef <- length(table_data$coef_rows)
  for (i in seq_len(n_coef)) {
    row <- table_data$coef_rows[[i]]
    # First coefficient block gets a top border separating it from the header.
    lines <- c(lines, data_row(row$label, row$values, border = (i == 1L)))
    lines <- c(lines, data_row("", row$se_values))
  }
  if (length(table_data$fe_rows) > 0L) {
    first <- TRUE
    for (row in table_data$fe_rows) {
      lines <- c(lines, data_row(row$label, row$values, border = first))
      first <- FALSE
    }
  }
  first <- TRUE
  for (row in table_data$stat_rows) {
    lines <- c(lines, data_row(row$label, row$values, border = first))
    first <- FALSE
  }
  lines <- c(lines, "</tbody>")

  # --- Notes ---
  note_lines <- build_html_notes(table_data, notes, notes.append, notes.label, nc)
  if (length(note_lines) > 0L) {
    lines <- c(lines, "<tfoot>", note_lines, "</tfoot>")
  }

  lines <- c(lines, "</table>")
  paste(lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Header builder
# ---------------------------------------------------------------------------

build_html_header <- function(table_data, column.labels, dep.var.caption) {
  nc          <- table_data$n_cols
  dep_vars    <- html_escape(table_data$dep_vars)
  col_numbers <- table_data$col_numbers
  lines       <- character(0L)

  empty_label <- "<td style=\"text-align:left\"></td>"

  # "Dependent variable:" caption spanning all data columns
  lines <- c(lines, paste0(
    "<tr>", empty_label,
    "<th colspan=\"", nc, "\">", latex_to_html(dep.var.caption), "</th></tr>"
  ))

  # Dependent variable name(s): span when shared across >1 column
  if (length(unique(dep_vars)) == 1L && nc > 1L) {
    lines <- c(lines, paste0(
      "<tr>", empty_label,
      "<th colspan=\"", nc, "\" style=\"border-bottom:1px solid black\">",
      dep_vars[1L], "</th></tr>"
    ))
  } else {
    cells <- paste0(
      "<th style=\"border-bottom:1px solid black\">", dep_vars, "</th>",
      collapse = "")
    lines <- c(lines, paste0("<tr>", empty_label, cells, "</tr>"))
  }

  # Model-type / column-label row
  col_labels <- if (!is.null(column.labels)) {
    html_escape(column.labels)
  } else if (table_data$show_model_row) {
    html_escape(table_data$model_labels)
  } else {
    NULL
  }
  if (!is.null(col_labels)) {
    if (length(col_labels) < nc) {
      col_labels <- c(col_labels, rep("", nc - length(col_labels)))
    }
    cells <- paste0("<th>", col_labels[seq_len(nc)], "</th>", collapse = "")
    lines <- c(lines, paste0("<tr>", empty_label, cells, "</tr>"))
  }

  # Column numbers (suppressed for single-model tables, as in the original)
  if (nc > 1L) {
    cells <- paste0("<th>", col_numbers, "</th>", collapse = "")
    lines <- c(lines, paste0("<tr>", empty_label, cells, "</tr>"))
  }

  lines
}

# ---------------------------------------------------------------------------
# Notes builder
# ---------------------------------------------------------------------------

build_html_notes <- function(table_data, notes, notes.append, notes.label, nc) {
  # Block 1 combines the SE note and the significance legend (both carry LaTeX
  # markup -> latex_to_html); each custom note is raw user text -> html_escape.
  if (notes.append || is.null(notes)) {
    se_part   <- format_se_note(table_data$se_notes, table_data$col_numbers)
    star_part <- table_data$star_note
    sig_parts <- Filter(function(x) !is.null(x) && nchar(x) > 0L,
                        c(se_part, star_part))
    sig_block <- latex_to_html(paste(sig_parts, collapse = "; "))
    blocks    <- c(sig_block, html_escape(notes))
  } else {
    blocks <- html_escape(notes)
  }
  blocks <- Filter(function(x) !is.null(x) && nchar(x) > 0L, blocks)

  # First block carries the "Note:" label; later notes get an empty label cell.
  # notes.label defaults to the LaTeX "\textit{Note:} ", so route it through
  # latex_to_html (turns \textit{} into <em>) rather than escaping it literally.
  vapply(seq_along(blocks), function(i) {
    label <- if (i == 1L) latex_to_html(notes.label) else ""
    border <- if (i == 1L) " style=\"border-top:1px solid black; text-align:right\"" else " style=\"text-align:right\""
    paste0(
      "<tr><td style=\"text-align:left\">", label, "</td>",
      "<td colspan=\"", nc, "\"", border, ">", blocks[i], "</td></tr>"
    )
  }, character(1L))
}

# ---------------------------------------------------------------------------
# HTML helpers
# ---------------------------------------------------------------------------

# Escape characters that are special in HTML text content.
html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x
}

# Convert the LaTeX markup embedded in table_data cells to HTML.  Mirrors
# strip_latex() (the ASCII converter) but produces HTML.
#
# Cells are a mix of generated LaTeX markup (e.g. "0.092$^{***}$", "R$^{2}$")
# and, for user-supplied covariate.labels, raw text that may contain stray
# HTML-special characters.  We therefore protect the real LaTeX constructs with
# private-use sentinels, HTML-escape whatever stray <, >, & remain, then expand
# the sentinels into their final HTML.  This keeps intentional markup intact
# while still escaping raw user text.
latex_to_html <- function(x) {
  # Private-use sentinels (\u escapes keep this source ASCII; see CRAN check)
  SUP_O <- "\uE000"; SUP_C <- "\uE001"; EM_O <- "\uE002"; EM_C <- "\uE003"
  LT    <- "\uE004"; GT    <- "\uE005"; AMP  <- "\uE006"; MINUS <- "\uE007"
  BS    <- "\uE008"

  # Protect LaTeX constructs as sentinels (no <, >, & emitted yet)
  x <- gsub("\\$\\^\\{([^}]*)\\}\\$", paste0(SUP_O, "\\1", SUP_C), x)  # superscripts
  x <- gsub("\\$<\\$", LT,    x)
  x <- gsub("\\$>\\$", GT,    x)
  x <- gsub("\\$-\\$", MINUS, x)
  x <- gsub("\\$([^$]*)\\$", "\\1", x)                                # other $...$
  x <- gsub("\\\\textit\\{([^}]*)\\}", paste0(EM_O, "\\1", EM_C), x)  # italics
  x <- gsub("\\\\textbackslash\\{\\}", BS,  x)
  x <- gsub("\\\\&", AMP, x)
  x <- gsub("\\\\_", "_",  x)
  x <- gsub("\\\\%", "%",  x)
  x <- gsub("\\\\#", "#",  x)
  x <- gsub("\\\\[a-zA-Z]+\\{([^}]*)\\}", "\\1", x)                   # leftover \cmd{x}
  x <- gsub("\\\\[a-zA-Z]+", "", x)                                  # bare \cmd

  # Escape any stray HTML-special characters from raw user text
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)

  # Expand sentinels to final HTML
  x <- gsub(SUP_O, "<sup>",   x, fixed = TRUE); x <- gsub(SUP_C, "</sup>", x, fixed = TRUE)
  x <- gsub(EM_O,  "<em>",    x, fixed = TRUE); x <- gsub(EM_C,  "</em>",  x, fixed = TRUE)
  x <- gsub(LT,    "&lt;",    x, fixed = TRUE); x <- gsub(GT,    "&gt;",   x, fixed = TRUE)
  x <- gsub(AMP,   "&amp;",   x, fixed = TRUE); x <- gsub(MINUS, "&minus;", x, fixed = TRUE)
  x <- gsub(BS,    "\\",      x, fixed = TRUE)
  x
}
