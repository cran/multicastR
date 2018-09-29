# ----------------------------------------------------------------------

#' Ascertain GRAID functions of phrasal subconstituents
#'
#' In GRAID, noun phrase subconstituents are marked with \code{<ln>} or
#' \code{<rn>} for elements respectively to the left or right of the phrase
#' head. As, however, GRAID functions are only marked on the phrase head (with
#' the exception of the \code{<:poss>} function), the function of the phrase can
#' only directly be read off its head, not any of of its other constituents. The
#' \code{mc_subfunc} function finds the heads of NPs and copies their GRAID
#' functions to potentially referential subconstituents that - crucially - are
#' continguous with the phrase head in linear order. That is, if a NP is broken
#' up by any elements not belonging to it, this function cannot associate any of
#' its non-contiguous subconstituents with it. Note also that only a subset of
#' subconstituent glosses are assigned functions, specifically those containing
#' the forms in the \code{subforms} argument. By default, these are \code{<np>}
#' for lexical nouns (e.g \code{<ln_np>} and \code{<rn_np>}, but also
#' \code{<ln_pn_np>} etc.), \code{<pro>} for free pronouns, \code{<refl>} for
#' reflexives, and \code{<indef>} for indefinites. This list of forms can be
#' replaced or expanded as necessary, or left empty to match all affixed
#' subconstituent glosses (i.e. \code{<ln_*>} and \code{<rn_*>}, but not simple
#' \code{<ln>} or \code{<rn>}).
#'
#' @seealso \code{\link{multicast}}
#'
#' @param text A \code{\link[data.table]{data.table}} in multicastR format,
#'   containing minimally columns for \code{file}, GRAID function \code{gfunc},
#'   and GRAID form \code{gform}.
#' @param subforms A character vector of GRAID form symbols that should be
#'   assigned the function of the phrase they are subconstituents of.
#'
#' @return The \code{text} with an additional column \code{pfunc} combining the
#'   values of \code{gfunc} with functions copied to phrasal subconstituents.
#'
#' @keywords internal
#' @export
mc_subfunc <- function(text, subforms = c("np", "pro", "refl", "indef")) {
	# mark all phrasal heads (i.e. rows with a GRAID function)
	text[gfunc != "", pform := "head"]

	# mark all phrasal subconstituents (<ln> or <rn>)
	text[grepl("ln", gform), pform := "ln"]
	text[grepl("rn", gform), pform := "rn"]


	# index phrase heads and non-phrase subconstituent elements sequentially
	text[pform == "head" | is.na(pform), pid := seq(1, .N, 1), by = "file"]

	# label subconstituents with the index of the phrase head they are contiguous with,
	# forward if <ln>, backward if <rn>
	text[, pid := pid[which(!is.na(pid))], by = cumsum(!is.na(pid))]
	text[pform == "ln", pid := NA]
	# for forward assignment, column order needs to be reversed!
	text[, pid := rev(pid)]
	text[, pid := pid[which(!is.na(pid))], by = cumsum(!is.na(pid))]
	text[, pid := rev(pid)]


	# select only rows with phrase heads
	hfun <- text[pform == "head", c("file", "gfunc", "pid")]

	# rename function column to properly identify columns on joining
	setnames(hfun, "gfunc", "hfunc")

	# join table with phrase heads with text table
	text[hfun, hfunc := hfunc, on = c("file", "pid")]


	# copy base GRAID functions
	text[, pfunc := gfunc]

	# if row contains a phrasal subconstutient with one of the GRAID forms
	# specified in subforms and does not already have a function (usually <:poss>),
	# then assign it the function of the phrasal head
	text[(pform == "ln" | pform == "rn") &
		 grepl(paste0("[lr]n_\\w*?(", paste(subforms, collapse = "|"), ")"), gform) &
		 gfunc == "",
		 pfunc := hfunc]


	# discard working columns
	text[, c("pform", "pid", "hfunc") := NULL]

	# return table
	return(text)
}

# stop RMD CHECK from complaining about global variables
if (getRversion() >= "2.15.1") {
	utils::globalVariables(c("gfunc", "pform", "pid", "hfunc", "pfunc"))
}

# ----------------------------------------------------------------------
