# ----------------------------------------------------------------------

#' Count clause units in a multicastR table (WIP)
#'
#' Counts the number of clause units (bounded by the \code{<##>}, \code{<#>}, or
#' \code{<\%>} annotation symbols) in a multicastR table.
#'
#' @seealso \code{\link{multicast}}
#'
#' @param text A \code{\link[data.table]{data.table}} in multicastR format,
#'   containing minimally a \code{corpus} column with the names of the corpora
#'   and a \code{graid} column with GRAID annotation values.
#' @param bytext Logical. If \code{FALSE}, calculate the number of clause units
#'   for each corpus. If \code{TRUE}, count for each text separately.
#'
#' @return A \code{\link[data.table]{data.table}} with the number of valid
#'   clause units in each corpus, the total number of clause units, the number
#'   of non-analyzed clause units ("NC"), and the percentage the later make up
#'   of the total.
#'
#' @examples
#'   \dontrun{
#'     # count clause units in the most recent version
#'     # of the Multi-CAST data, by corpus
#'     n <- mc_count_clauses(multicast())
#'
#'     # count by text instead
#'     m <- mc_count_clauses(multicast(), bytext = TRUE)
#'
#'     # number of clauses units in the whole collection
#'     sum(n$nClauses)
#'   }
#'
#' @keywords internal
#' @export
mc_count_clauses <- function(text, bytext = FALSE) {
	# count by corpus or by text?
	if (bytext == TRUE) {
		byX <- c("corpus", "text")
	} else {
		byX <- c("corpus")
	}

	# count all clauses
	cnts <- text[grepl("^#", graid), .N, by = byX]
	setnames(cnts, "N", "nAll")

	# count NC clauses
	cnts[text[grepl("^#nc", graid), .N, by = byX], nNC := N, on = byX]

	# set nNC to 0 if there are no NC clauses (by defauly NA)
	cnts[is.na(nNC), nNC := 0]

	# calculate difference and percentage of NC clauses
	cnts[, c("nClause", "pNC") := list(nAll - nNC, round(100 * nNC / nAll, 2)) ]

	# reorder rows and columns
	if (bytext == TRUE) {
		setorder(cnts, corpus, text)
	} else {
		setorder(cnts, corpus)
	}

	setcolorder(cnts, c(byX, "nClause", "nAll", "nNC", "pNC"))

	# return table
	print(cnts)
}

# stop RMD CHECK from complaining about unbound global variables
if (getRversion() >= "2.15.1") {
	utils::globalVariables(c("graid", "nNC", "nAll"))
}

# ----------------------------------------------------------------------
