# ----------------------------------------------------------------------

#' Access the Multi-CAST list of referents
#'
#' \code{mc_referents} downloads a tabular list of all discourse referents
#' occuring in those texts in the Multi-CAST collection that have been annotated
#' with the RefIND scheme (Schiborr et al. 2018). The data are downloaded from
#' the servers of University of Bamberg.
#'
#' @seealso \code{\link{multicast}}, \code{\link{mc_index}},
#'   \code{\link{mc_metadata}}, \code{\link{mc_clauses}}
#'
#' @param vkey A four-digit number specifying the requested version of the list
#'   of referents. Must be one of the version keys listed in the first column of
#'   \code{\link{mc_index}}, or empty. If empty, the most recent version of the
#'   list of referents is retrieved automatically. Note that the first
#'   annotations with RefIND were added with version 1905 (May 2019) of
#'   Multi-CAST, and hence no lists of referents exist for earlier versions
#'   (i.e. 1505 and 1606).
#'
#' @return A \code{\link{data.frame}} containing a list of referents for all
#'   texts with RefIND annotations in the Multi-CAST collection. The table has
#'   the following eight columns:
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
#'   # retrieve the most recent version of the Multi-CAST list of referents
#'   mc_referents()
#'
#'   # retrieve the lists of referents published in January 2021
#'   mc_referents(2021)
#'
#'   # join the list of referents to a table with annotation values
#'   mc <- multicast()
#'   merge(mc, mc_referents(),
#'         by = c("corpus", "text", "refind"),
#'         all.x = TRUE)
#' }
#'
#' @export
mc_referents <- function(vkey = NULL) {

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
				if (vkey == "1505" | vkey == "1606") {
					stop("No lists of referents exist for versions '1505' and '1606' of Multi-CAST.")
				}
				else if (!(vkey %in% index[[1]])) {
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
	path <- paste0("https://multicast.aspra.uni-bamberg.de/data/docs/general/list-of-referents/",
				   vkey,
				   "__mc_list-of-referents.tsv")

	message("Retrieving Multi-CAST list of referents...")
	meta <- tryCatch(suppressWarnings(read.csv(path,
											   sep = "\t",
											   header = TRUE,
											   colClasses = c(rep("factor", 2),
											   			      rep("character", 5)),
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
