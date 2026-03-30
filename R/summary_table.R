# Summary statistics table for stargazer2.
#
# Handles data.frame and matrix inputs passed to stargazer().
# Produces summary tables matching the format of the original stargazer.

# Recognised summary statistics and their display labels (in canonical order).
SUMMARY_STAT_LABELS <- c(
  n      = "N",
  mean   = "Mean",
  sd     = "St. Dev.",
  min    = "Min",
  max    = "Max",
  median = "Median",
  p25    = "Pctl(25)",
  p75    = "Pctl(75)"
)

DEFAULT_SUMMARY_STATS <- c("n", "mean", "sd", "min", "max")

# ---------------------------------------------------------------------------
# Entry point (called from stargazer() when input is a data.frame/matrix)
# ---------------------------------------------------------------------------

#' Render a summary statistics table
#'
#' Internal; called by \code{\link{stargazer}} when its first argument is a
#' \code{data.frame} or \code{matrix}.
#' @keywords internal
stargazer_summary <- function(data,
                               type,
                               title        = "",
                               label        = "",
                               font.size    = NULL,
                               covariate.labels = NULL,
                               omit         = NULL,
                               keep         = NULL,
                               digits       = 3L,
                               summary.stat = NULL,
                               median       = FALSE,
                               notes        = NULL,
                               notes.append = TRUE,
                               notes.align  = "l",
                               notes.label  = "",
                               out          = NULL) {

  # --- Determine which statistics to report ---
  if (!is.null(summary.stat)) {
    stats <- summary.stat
    if (isTRUE(median) && !"median" %in% stats) {
      stats <- c(stats, "median")
    }
  } else if (isTRUE(median)) {
    stats <- c("n", "mean", "sd", "min", "median", "max")
  } else {
    stats <- DEFAULT_SUMMARY_STATS
  }
  # Silently drop unrecognised stat identifiers
  stats <- stats[stats %in% names(SUMMARY_STAT_LABELS)]

  # --- Prepare data ---
  if (is.matrix(data)) data <- as.data.frame(data)

  # Keep only numeric columns (silently skip factors / characters)
  numeric_mask <- vapply(data, is.numeric, logical(1L))
  df <- data[, numeric_mask, drop = FALSE]

  var_names <- names(df)

  # Apply omit / keep filters (on raw variable names)
  if (!is.null(omit)) {
    pat <- paste(omit, collapse = "|")
    var_names <- var_names[!grepl(pat, var_names)]
  }
  if (!is.null(keep)) {
    pat <- paste(keep, collapse = "|")
    var_names <- var_names[grepl(pat, var_names)]
  }
  df <- df[, var_names, drop = FALSE]

  # --- Compute stats ---
  rows <- compute_summary_rows(df, stats, digits)

  # --- Apply covariate.labels (positional) ---
  if (!is.null(covariate.labels)) {
    n_lab <- min(length(covariate.labels), length(rows))
    for (j in seq_len(n_lab)) {
      rows[[j]]$label <- covariate.labels[[j]]
    }
  }

  # Display labels for each stat column
  stat_headers <- unname(SUMMARY_STAT_LABELS[stats])

  # --- Render ---
  output <- switch(type,
    latex = render_summary_latex(
      rows, stat_headers, title, label, font.size, notes
    ),
    text = render_summary_ascii(
      rows, stat_headers, title, notes
    ),
    stop("stargazer: type must be one of 'latex', 'text', 'html'.", call. = FALSE)
  )

  if (!is.null(out)) {
    writeLines(output, con = out)
  } else {
    cat(output, "\n")
  }

  invisible(output)
}

# ---------------------------------------------------------------------------
# Statistics computation
# ---------------------------------------------------------------------------

compute_summary_rows <- function(df, stats, digits) {
  var_names <- names(df)
  rows <- vector("list", length(var_names))

  for (j in seq_along(var_names)) {
    x          <- df[[j]]
    x_complete <- x[!is.na(x)]
    n_obs      <- length(x_complete)

    vals <- vapply(stats, function(stat) {
      raw <- switch(stat,
        n      = as.numeric(n_obs),
        mean   = mean(x_complete),
        sd     = sd(x_complete),
        min    = min(x_complete),
        max    = max(x_complete),
        median = median(x_complete),
        p25    = quantile(x_complete, 0.25, names = FALSE),
        p75    = quantile(x_complete, 0.75, names = FALSE),
        NA_real_
      )
      format_summary_val(raw, stat, digits)
    }, character(1L))

    rows[[j]] <- list(
      label  = latex_escape(var_names[[j]]),
      values = vals
    )
  }

  rows
}

# Format one summary statistic value (LaTeX-encoded).
#   n:        plain integer (no decimals, no comma for the diff test).
#   mean, sd: always `digits` decimal places; negative → $-$.
#   others:   integer if whole-number value, else `digits` d.p.; negative → $-$.
format_summary_val <- function(x, stat, digits) {
  if (is.na(x)) return("")

  if (stat == "n") {
    return(as.character(as.integer(x)))
  }

  always_decimal <- stat %in% c("mean", "sd")

  if (!always_decimal && x == floor(x) && abs(x) < 2.1e15) {
    # Integer-valued: display without decimal places
    int_val <- as.integer(x)
    if (x < 0L) return(paste0("$-$", abs(int_val)))
    return(as.character(int_val))
  }

  fmt <- formatC(abs(x), digits = digits, format = "f")
  if (x < 0) paste0("$-$", fmt) else fmt
}

# ---------------------------------------------------------------------------
# LaTeX renderer
# ---------------------------------------------------------------------------

render_summary_latex <- function(rows, stat_headers, title, label, font.size,
                                  notes) {
  n_stats    <- length(stat_headers)
  n_cols_all <- n_stats + 1L  # Statistic label col + stat cols

  col_spec <- paste0(
    "@{\\extracolsep{5pt}}l",
    paste(rep("c", n_stats), collapse = "")
  )

  lines <- character(0L)

  lines <- c(lines, "")
  lines <- c(lines, "\\begin{table}[!htbp] \\centering ")
  if (!is.null(font.size) && nchar(font.size) > 0L) {
    lines <- c(lines, paste0("  \\", font.size))
  }
  lines <- c(lines, paste0("  \\caption{", title, "} "))
  lines <- c(lines, paste0("  \\label{", label, "} "))
  lines <- c(lines, paste0("\\begin{tabular}{", col_spec, "} "))
  lines <- c(lines, "\\\\[-1.8ex]\\hline ")
  lines <- c(lines, "\\hline \\\\[-1.8ex] ")

  # Header row: "Statistic & \multicolumn{1}{c}{N} & ..."
  header_cells <- vapply(stat_headers, function(h) {
    paste0("\\multicolumn{1}{c}{", h, "}")
  }, character(1L))
  lines <- c(lines, paste0(
    "Statistic & ",
    paste(header_cells, collapse = " & "),
    " \\\\ "
  ))
  lines <- c(lines, "\\hline \\\\[-1.8ex] ")

  # Data rows
  for (row in rows) {
    lbl   <- row$label
    cells <- vapply(row$values, function(v) if (is.na(v) || v == "") "" else v,
                    character(1L))
    lines <- c(lines, paste0(
      lbl, " & ", paste(cells, collapse = " & "), " \\\\ "
    ))
  }

  lines <- c(lines, "\\hline \\\\[-1.8ex] ")

  # Notes (if supplied): \multicolumn{n_cols_all}{l}{note text} \\
  if (!is.null(notes) && length(notes) > 0L) {
    note_text <- paste(notes, collapse = "; ")
    lines <- c(lines, paste0(
      "\\multicolumn{", n_cols_all, "}{l}{", note_text, "} \\\\ "
    ))
  }

  lines <- c(lines, "\\end{tabular} ")
  lines <- c(lines, "\\end{table}")

  paste(lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# ASCII renderer
# ---------------------------------------------------------------------------

render_summary_ascii <- function(rows, stat_headers, title, notes) {
  n_stats <- length(stat_headers)

  # Label column width: max of variable names and "Statistic"
  label_w <- max(
    vapply(rows, function(r) nchar(strip_latex(r$label)), integer(1L)),
    nchar("Statistic"),
    1L, na.rm = TRUE
  )

  # Per-stat column widths: max of header and all values in that column
  col_w <- vapply(seq_len(n_stats), function(k) {
    val_widths <- vapply(rows, function(r) nchar(strip_latex(r$values[[k]])),
                         integer(1L))
    max(val_widths, nchar(stat_headers[[k]]), 1L, na.rm = TRUE)
  }, integer(1L))

  val_area <- sum(col_w) + n_stats - 1L   # cols + single-space separators
  total_w  <- label_w + 1L + val_area

  dbl_line <- strrep("=", total_w)
  sep_line <- strrep("-", total_w)

  lines <- character(0L)

  # Leading blank line (matches original stargazer)
  lines <- c(lines, "")

  if (!is.null(title) && nchar(title) > 0L) {
    lines <- c(lines, paste0("  ", title))
  }

  lines <- c(lines, dbl_line)

  # Header row
  header_label <- formatC("Statistic", width = label_w, flag = "-")
  header_cells <- mapply(function(h, w) centre_in(h, w),
                         stat_headers, col_w, SIMPLIFY = TRUE)
  lines <- c(lines,
    paste0(header_label, " ", paste(header_cells, collapse = " "))
  )

  lines <- c(lines, sep_line)

  # Data rows
  for (row in rows) {
    raw_label <- strip_latex(row$label)
    raw_vals  <- vapply(row$values, strip_latex, character(1L))
    label_cell <- formatC(raw_label, width = label_w, flag = "-")
    val_cells  <- mapply(function(v, w) centre_in(v, w),
                         raw_vals, col_w, SIMPLIFY = TRUE)
    lines <- c(lines,
      paste0(label_cell, " ", paste(val_cells, collapse = " "))
    )
  }

  lines <- c(lines, sep_line)

  # Notes: plain text, left-aligned in full table width
  if (!is.null(notes) && length(notes) > 0L) {
    note_text <- paste(notes, collapse = "; ")
    lines <- c(lines, formatC(note_text, width = -total_w))
  }

  paste(lines, collapse = "\n")
}
