# ----------------------------------------------------------------------

#' Access the Multi-CAST metadata
#'
#' \code{mc_metadata} downloads a table with metadata on the texts and speakers
#' in the Multi-CAST collection. The data is downloaded from the servers of
#' University of Bamberg and presented as a
#' \code{\link[data.table]{data.table}}.
#'
#' @return A \code{\link[data.table]{data.table}} containing metadata on the
#'   Multi-CAST collection. The table has the following eight columns:
#'
#'   \describe{ \item{\code{[, 1] corpus}}{The name of the corpus.}
#'   \item{\code{[, 2] text}}{The title of the text.} \item{\code{[, 3]
#'   type}}{The text type, either \code{TN} 'traditional narrative', \code{AN}
#'   'autobiographical narrative', or \code{SN} 'stimulus-based narrative'.}
#'   \item{\code{[, 4] recorded}}{The year (YYYY) the text was recorded.}
#'   \item{\code{[, 5] speaker}}{The identifier for the speaker.} \item{\code{[,
#'   6] gender}}{The speaker's gender.} \item{\code{[, 7] age}}{The speaker's
#'   age at the time of recording. Approximate values are prefixed with a
#'   \code{c}.} \item{\code{[, 8] born}}{The speaker's birth year. Approximate
#'   values are prefixed with a \code{c}.} }
#'
#' @examples
#' \dontrun{
#'   # retrieve and print the Multi-CAST metadata
#'   mc_metadata()
#' }
#'
#' @export
mc_metadata <- function() {
	# construct URL for annotation file
	path <- paste0("https://multicast.aspra.uni-bamberg.de/data/mcr/mc_",
				   "metadata",
				   ".tsv")

	# fetch annotation file
	message("Retrieving metadata...")
	tryCatch(
		suppressWarnings(
			meta <- data.table::fread(path,
									  sep = "\t",
									  header = TRUE,
									  colClasses = list(factor = c(1:3, 5:6), character = c(4, 7:8)),
									  encoding = "UTF-8",
									  showProgress = FALSE)
		),
		error = function(e) { stop("failed to download metadata.") }
	)
	message(paste0("Success!"))

	# return annotation values
	return(meta)
}
