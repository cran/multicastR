# ----------------------------------------------------------------------

#' Test for empty arguments
#'
#' \code{mcmissarg} tests whether or not a value was specified as an argument to
#' a function. Unlike \code{\link[base]{missing}}, it also returns \code{FALSE}
#' with zero-length objects such as \code{c()} or \code{data.frame()}.
#'
#' @seealso \code{\link{multicast}}
#'
#' @return A logical vector of length 1.
#' @keywords internal
mc_missarg <- function(x) {
	if ("x" %in% names(match.call())) {
		return(missing(x))
	} else {
		return(TRUE)
	}
}

# ----------------------------------------------------------------------
