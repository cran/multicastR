# ----------------------------------------------------------------------

#' Access the Multi-CAST metadata
#'
#' \code{mc_metadata} downloads a table with metadata on the texts and speakers
#' in the Multi-CAST collection. The data is downloaded from the servers of
#' University of Bamberg and presented as a
#' \code{\link[data.table]{data.table}}.
#'
#' @seealso \code{\link{multicast}}, \code{\link{mc_index}},
#'   \code{\link{mc_referents}}, \code{\link{mc_clauses}}
#'
#' @param vkey A numeric or character vector of length 1 specifying the
#'   requested version of the annotation values. Must be one of the four-digit
#'   version keys in the first column of \code{\link{mc_index}}, or empty. If
#'   empty or no value is supplied, the most recent version of the annotations
#'   is retrieved automatically.
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
#'   \code{c}.} \item{\code{[, 8] born}}{The speaker's birth year (YYY).
#'   Approximate values are prefixed with a \code{c}.} }
#'
#' @examples
#' \dontrun{
#'   # retrieve the most recent version of the Multi-CAST metadata
#'   mc_metadata()
#'
#'   # retrieve the lists of referents published in May 2019
#'   mc_metadata(1905)   # or: mc_metadata("1905")
#'
#'   # join the metadata to a table with annotation values
#'   mc <- multicast()
#'   merge(mc, mc_metadata(), by = c("corpus", "text"))
#' }
#'
#' @importFrom curl curl
#' @export
mc_metadata <- function(vkey) {
	# check whether vkey is missing
	if (!mc_missarg(vkey)) {
		# 1A: vkey is not missing
		# check whether vkey is a numeric or character vector
		if (!(is.numeric(vkey) | is.character(vkey))) {
			# 2A: vkey is not a numeric or character vector
			stop(paste0("\n  Argument is not of type numeric or character."))
		} else {
			# 2B: vkey has correct type
			# check whether vkey has length == 1
			if (length(vkey) != 1) {
				# 3A: vkey has length != 1
				stop(paste0("\n  Argument has length > 1."))
			} else {
				# 3B: vkey has length == 1
				# check whether vkey has valid format
				if (!grepl("^\\d\\d\\d\\d$", vkey)) {
					# 4A: vkey has invalid format
					stop(paste0("\n  The supplied version key '",
								vkey,
								"' has invalid format."))
				} else {
					# 4B: vkey has valid format

					# fetch version index
					index <- mc_index()

					# check whether vkey is in index
					if (!any(index[, 1] == vkey)) {
						# 5A: vkey is not in index
						stop(paste0("\n  The requested version '",
									vkey,
									"' does not exist."))
					} else {
						# 5B: vkey found

						# add 'latest' tag to message
						if (vkey==index[1, 1]) {
							lat <- "latest, "
						} else {
							lat <- ""
						}
						message(paste0("Found requested version '",
									   vkey,
									   "' (",
									   lat,
									   "published ",
									   index[version == vkey,2], ")."))
					}
				}
			}
		}
	} else {
		# 1B: vkey is missing

		# fetch version index
		index <- mc_index()

		# select latest vkey from index
		vkey <- as.character(index[1, 1])
		message(paste0("Found latest version '",
					   index[1, 1],
					   "' (published ",
					   index[1, 2], ")."))
	}

	# construct URL for annotation file
	path <- paste0("https://multicast.aspra.uni-bamberg.de/data/docs/general/metadata/",
				   vkey,
				   "__mc_metadata.tsv")

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
#	message(paste0("Success!"))

	# return annotation values
	return(meta)
}
