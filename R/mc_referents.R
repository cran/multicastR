# ----------------------------------------------------------------------

#' Access the Multi-CAST list of referents
#'
#' \code{mc_referents} downloads the lists of referents for all texts in the
#' Multi-CAST collection that have been annotated with the RefIND scheme
#' (Referent Indexing in Natural-language Discourse, Schiborr et al. 2018). The
#' data are downloaded from the servers of University of Bamberg and presented as
#' a \code{\link[data.table]{data.table}}.
#'
#' @seealso \code{\link{multicast}}, \code{\link{mc_index}},
#'   \code{\link{mc_metadata}}, \code{\link{mc_clauses}}
#'
#' @param vkey A numeric or character vector of length 1 specifying the
#'   requested version of the annotation values. Must be one of the four-digit
#'   version keys in the first column of \code{\link{mc_index}}, or empty. If
#'   empty or no value is supplied, the most recent version of the annotations
#'   is retrieved automatically. Note that the first annotations with RefIND
#'   were added with version 1905 (May 2019), and hence no lists of referents
#'   exist for earlier versions (i.e. 1505 and 1606).
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
#'   # retrieve the most recent version of the Multi-CAST lists of referents
#'   mc_referents()
#'
#'   # retrieve the lists of referents published in May 2019
#'   mc_referents(1905)   # or: mc_referents("1905")
#'
#'   # join the list of referents to a table with annotation values
#'   mc <- multicast()
#'   merge(mc, mc_referents(),
#'         by = c("corpus", "text", "refind"),
#'         all.x = TRUE)
#' }
#'
#' @importFrom curl curl
#' @export
mc_referents <- function(vkey) {
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

						# check whether vkey is for version with no RefIND
						if (vkey == "1505" | vkey == "1606") {
							# 6A: version has no RefIND
							stop(paste0("\n No lists of referents exist before version 1905 of Multi-CAST."))
						} else {
							# 6B: version has RefIND

							# add 'latest' tag to message
							if (vkey == index[1, 1]) {
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
	path <- paste0("https://multicast.aspra.uni-bamberg.de/data/docs/general/list-of-referents/",
				   vkey,
				   "__mc_list-of-referents.tsv")

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
#	message(paste0("Success!"))

	# return annotation values
	return(refs)
}
