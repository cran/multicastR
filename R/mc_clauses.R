
# ----------------------------------------------------------------------

#' Count clauses in a multicastR table
#'
#' \code{mc_clauses} counts the number of clause units (bounded by the
#' \code{<##>} or \code{<#>} GRAID annotation symbols) in a multicastR table.
#'
#' @seealso \code{\link{multicast}}, \code{\link{mc_index}},
#'   \code{\link{mc_metadata}}, \code{\link{mc_referents}},
#'   \code{\link{mc_clauses}}
#'
#' @param x A \code{\link{data.frame}} in multicastR format. This table
#'   minimally requires the \code{corpus} and \code{graid} columns with the
#'   names of the corpora and the GRAID annotation values, respectively, as well
#'   as the \code{text} column if \code{bytext} is set to \code{TRUE}.
#' @param bytext Logical. If \code{FALSE}, calculate the number of clause units
#'   for each corpus. If \code{TRUE}, count for each text separately.
#'   \code{FALSE} by default.
#' @param printToConsole Logical. If \code{TRUE}, prints the table to the
#'   console (using \code{\link{message}}). \code{FALSE} by default.
#'
#' @return A \code{data.frame} with the \code{corpus}, \code{text} (if
#'   \code{bytext} is \code{TRUE}), the number of valid clause units in each
#'   corpus (\code{nClause}), the total number of clause units (\code{nAll}),
#'   the number of clause units not analyzed (\code{nNC}), and the percentage
#'   the later make up of the total (\code{pNC}).
#'
#' @examples
#' \dontrun{
#'   # count clause units in the most recent version
#'   # of the Multi-CAST data, by corpus
#'   n <- mc_clauses(multicast())
#'
#'   # count by text instead
#'   m <- mc_clauses(multicast(), bytext = TRUE)
#'
#'   # number of clauses units in the whole collection
#'   sum(n$nClauses)
#' }
#'
#' @export
mc_clauses <- function(x, bytext = FALSE, printToConsole = FALSE) {

	# count by corpus or by text?
	if (bytext) {
		byCols = c("corpus", "text")
		colOrder = c(1, 2, 5, 3, 4, 6)
	} else {
		byCols = "corpus"
		colOrder = c(1, 4, 2, 3, 5)
	}

	# check whether data has correct format
	if (!all(c(byCols, "graid") %in% names(x))) {
		stop(paste0("Missing required columns in supplied table ('",
					paste0(byCols, collapse = "', '"),
					"', 'graid').\n",
					"       Are you sure this is a multicastR table?"))
	}

	# count clauses
	nAll <- data.frame(table(x[with(x, grepl("^#", graid)), byCols]))
	nNC <- data.frame(table(x[with(x, grepl("^#nc", graid)), byCols]))
	names(nAll)[1] <- "corpus"
	names(nNC)[1] <- "corpus"

	# merge tables
	y <- merge(nAll[with(nAll, Freq != 0), ], nNC[with(nAll, Freq != 0), ], by = byCols)
	names(y) <- c(byCols, "nAll", "nNC")

	# calculate number of valid clauses and proportion of NC clauses
	y$nClause <- y$nAll - y$nNC
	y$pNC <- round(y$nNC * 100 / y$nAll, 2)

	# reorder columns
	y <- y[, colOrder]


	# show message
	if (printToConsole) {
		message(paste0(capture.output(y), collapse = "\n"))
	}

	# return table with clause counts
	return(y)

}


# ----------------------------------------------------------------------
