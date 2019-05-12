# ----------------------------------------------------------------------

#' Ascertain GRAID functions of phrasal subconstituents (WIP)
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
#' @param data A \code{\link[data.table]{data.table}} in multicastR format,
#'   containing minimally columns for \code{text}, GRAID function \code{gfunc},
#'   and GRAID form \code{gform}.
#' @param subforms A character vector of GRAID form symbols that should be
#'   assigned the function of the phrase they are subconstituents of.
#'
#' @return The data with an additional column \code{pfunc} combining the
#'   values of \code{gfunc} with functions copied to phrasal subconstituents.
#'
#' @keywords internal
mc_subfunc <- function(data, subforms = c("np", "pro", "refl", "indef")) {
	# mark all phrasal heads (i.e. rows with a GRAID function)
	data[gfunc != "", pform := "head"]

	# mark all phrasal subconstituents (<ln> or <rn>)
	data[grepl("ln", gform), pform := "ln"]
	data[grepl("rn", gform), pform := "rn"]


	# index phrase heads and non-phrase subconstituent elements sequentially
	data[pform == "head" | is.na(pform), pid := seq(1, .N, 1), by = "text"]

	# label subconstituents with the index of the phrase head they are contiguous with,
	# forward if <ln>, backward if <rn>
	data[, pid := pid[which(!is.na(pid))], by = cumsum(!is.na(pid))]
	data[pform == "ln", pid := NA]
	# for forward assignment, column order needs to be reversed!
	data[, pid := rev(pid)]
	data[, pid := pid[which(!is.na(pid))], by = cumsum(!is.na(pid))]
	data[, pid := rev(pid)]


	# select only rows with phrase heads
	hfun <- data[pform == "head", c("text", "gfunc", "pid")]

	# rename function column to properly identify columns on joining
	setnames(hfun, "gfunc", "hfunc")

	# join table with phrase heads with text table
	data[hfun, hfunc := hfunc, on = c("text", "pid")]


	# copy base GRAID functions
	data[, pfunc := gfunc]

	# if row contains a phrasal subconstutient with one of the GRAID forms
	# specified in subforms and does not already have a function (usually <:poss>),
	# then assign it the function of the phrasal head
	data[(pform == "ln" | pform == "rn") &
		 grepl(paste0("[lr]n_\\w*?(", paste(subforms, collapse = "|"), ")"), gform) &
		 gfunc == "",
		 pfunc := hfunc]


	# discard working columns
	data[, c("pform", "pid", "hfunc") := NULL]

	# return table
	return(data)
}

# stop RMD CHECK from complaining about unbound global variables
if (getRversion() >= "2.15.1") {
	utils::globalVariables(c("gfunc", "pform", "pid", "hfunc", "pfunc"))
}

# ----------------------------------------------------------------------
