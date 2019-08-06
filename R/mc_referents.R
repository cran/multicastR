# ----------------------------------------------------------------------

#' Access the Multi-CAST list of referents
#'
#' \code{mc_referents} downloads the lists of referents for all texts in the
#' Multi-CAST collection that have been annotated with the RefIND scheme
#' (Referent Indexing in Natural-language Discourse, Schiborr et al. 2018). The
#' data is downloaded from the servers of University of Bamberg and presented as
#' a \code{\link[data.table]{data.table}}.
#'
#' @return A \code{\link[data.table]{data.table}} containing lists of referents
#'   for all texts with RefIND annotations in the Multi-CAST collection. The table
#'   has the following eight columns:
#'
#'   \describe{ \item{\code{[, 1] corpus}}{The name of the corpus.}
#'   \item{\code{[, 2] text}}{The title of the text.} \item{\code{[, 3]
#'   refind}}{The four-digit referent index, unique to each referent in a text.}
#'   \item{\code{[, 4] label}}{The label used for the referent.} \item{\code{[,
#'   5] description}}{A short description of the referent.} \item{\code{[, 6]
#'   class}}{The semantic class of the referent. Legend: \code{hum} = human,
#'   \code{anm} = animate, \code{inm} = inanimate, \code{bdp} = body part,
#'   \code{mss} = mass, \code{loc} = location, \code{tme} = time, \code{abs} =
#'   abstract.} \item{\code{[, 7] relations}}{Relations of the referent to other
#'   referents. Legend: \code{<} = set member of (partial co-reference),
#'   \code{>} = includes (split antecedence), \code{M} = part-whole.}
#'   \item{\code{[, 8] notes}}{Annotators' notes on the referent and its
#'   properties.} }
#'
#' @examples
#' \dontrun{
#'   # retrieve and print the Multi-CAST lists of referents
#'   mc_referents()
#' }
#'
#' @export
mc_referents <- function() {
	# construct URL for annotation file
	path <- paste0("https://multicast.aspra.uni-bamberg.de/data/mcr/mc_",
				   "list-of-referents",
				   ".tsv")

	# fetch annotation file
	message("Retrieving list of referents...")
	tryCatch(
		suppressWarnings(
			refs <- data.table::fread(path,
									  sep = "\t",
									  header = TRUE,
									  colClasses = list(factor = 1:2, character = 3:8),
									  encoding = "UTF-8",
									  showProgress = FALSE)
		),
		error = function(e) { stop("failed to download list of referents.") }
	)
	message(paste0("Success!"))

	# return annotation values
	return(refs)
}
