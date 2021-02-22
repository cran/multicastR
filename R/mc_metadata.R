# ----------------------------------------------------------------------

#' Access the Multi-CAST metadata
#'
#' \code{mc_metadata} downloads a table with metadata on the texts and speakers
#' in the Multi-CAST collection from the servers of the University of Bamberg.
#'
#' @seealso \code{\link{multicast}}, \code{\link{mc_index}},
#'   \code{\link{mc_referents}}, \code{\link{mc_clauses}}
#'
#' @param vkey A four-digit number specifying the requested version of the
#'   metadata. Must be one of the version keys listed in the first column of
#'   \code{\link{mc_index}}, or empty. If empty, the most recent version of the
#'   metadata is retrieved automatically.
#'
#' @return A \code{\link{data.frame}} containing metadata on the Multi-CAST
#'   collection. The table has the following eight columns:
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
#'   # retrieve the lists of referents published in January 2021
#'   mc_metadata(2101)
#'
#'   # join the metadata to a table with annotation values
#'   mc <- multicast()
#'   merge(mc, mc_metadata(),
#'         by = c("corpus", "text"))
#' }
#'
#' @export
mc_metadata <- function(vkey = NULL) {

	# fetch version index
	index <- mc_index()
	useDefaultKey <- FALSE

	# check for invalid input
	if (!(is.null(vkey))) {
		if (length(vkey) > 1) {
			stop("Invalid version key. Argument has length > 1.")
		} else if (!(is.na(vkey) | vkey == "")) {
			if (!(is.numeric(vkey) | is.character(vkey))) {
				stop(paste0("Invalid version key. Argument must be numeric or character."))
			} else {
				if (!(vkey %in% index[[1]])) {
					stop("Invalid version key. Specified version not found.")
				}
			}
		} else {
			useDefaultKey <- TRUE
		}
	} else {
		useDefaultKey <- TRUE
	}

	# if no index was supplied, pick the most recent
	if (useDefaultKey) {
		vkey <- index[1, 1]
		message(paste0("No version specified, defaulting to latest ('",
					   index[1, 1],
					   "', published ",
					   index[1, 2], ")."))
	}


	# download the data
	path <- paste0("https://multicast.aspra.uni-bamberg.de/data/docs/general/metadata/",
				   vkey,
				   "__mc_metadata.tsv")

	message("Retrieving Multi-CAST metadata...")
	meta <- tryCatch(suppressWarnings(read.csv(path,
											   sep = "\t",
											   header = TRUE,
											   colClasses = c(rep("factor", 3),
											   			      "character",
											   			      rep("factor", 2),
											   			      rep("character", 2)),
											   encoding = "UTF-8")),
				   error = function(e) {
				   		      stop(paste0("Failed to download data. Cannot access file.\n",
					  		     	      "       The servers of the University of Bamberg seem to be experiencing problems.\n",
										  "       Please try again later."),
								   call. = FALSE)
					  	   }
			       )
	message(paste0("Downloaded <1 MB."))

	# return table with annotation values
	return(meta)

}


# ----------------------------------------------------------------------
