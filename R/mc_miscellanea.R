# ----------------------------------------------------------------------

#' Count clause units in a multicastR table
#'
#' Counts number of clause units (bounded by the \code{<##>}, \code{<#>}, or
#' \code{<\%>} annotation symbols) in a multicastR table.
#'
#' @seealso \code{\link{multicast}}
#'
#' @param text A \code{\link[data.table]{data.table}} in multicastR format,
#'   containing minimally a \code{corpus} column with the names of the corpora
#'   and a \code{graid} column with GRAID annotation values.
#'
#' @return A \code{\link[data.table]{data.table}} with the number of valid
#'   clause units in each corpus, the total number of clause units, the number
#'   of non-analyzed clause units ("NC"), and the percentage the later make up
#'   of the total.
#'
#' @examples
#' # count clause units in the most recent version
#' # of the Multi-CAST data, by corpus
#' n <- mc_count_clauses(multicast())
#'
#' # number of clauses units in the whole collection
#' sum(n$nClauses)
#'
#' @keywords internal
#' @export
mc_count_clauses <- function(text) {
	# count all clauses
	cnts <- text[grepl("^#", graid), .N, by = "corpus"]
	setnames(cnts, 2, "nAll")

	# count NC clauses
	cnts[, nNC := text[grepl("^#nc", graid), .N, by = "corpus"]$N]

	# set nNC to 0 if there are no NC clauses (by defauly NA)
	cnts[is.na(nNC), nNC := 0]

	# calculate difference and percentage of NC clauses
	cnts[, c("nClause", "pNC") := list(nAll - nNC, round(100 * nNC / nAll, 2)) ]

	# reorder rows and columns
	setorder(cnts, corpus)
	setcolorder(cnts, c(1, 4, 2, 3, 5))

	# return table
	print(cnts)
}

# stop RMD CHECK from complaining about global variables
if (getRversion() >= "2.15.1") {
	utils::globalVariables(c("graid", "nNC", "nAll"))
}

# ----------------------------------------------------------------------
