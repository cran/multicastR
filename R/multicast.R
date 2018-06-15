# ----------------------------------------------------------------------

#' Accessing Multi-CAST annotation data
#'
#' \code{multicast} downloads the Multi-CAST annotation data from the servers of
#' the Language Archive Cologne (LAC) and outputs it as a
#' \code{\link[data.table]{data.table}}.
#'
#' The Multi-CAST collection is amenable to extension by additional data sets
#' and annotation schemes. In the spirit of scientific accountability and
#' reproducability, \code{multicast} may take an optional argument that allows
#' access to previous versions of the annotation data.
#'
#' @section Licensing: The Multi-CAST annotation data accessed by the
#'   \code{multicast} method is published under a \emph{Create Commons
#'   Attribution 4.0 International} (CC-BY 4.0) licence
#'   (\url{https://creativecommons.org/licenses/by-sa/4.0/}). Please refer to
#'   the collection documentation for information on how to give proper credit
#'   to its contributors.
#'
#' @section Citing Multi-CAST: Data from the Multi-CAST collection should be
#'   cited as: \itemize{ \item Haig, Geoffrey & Schnell, Stefan (eds.). 2018[2015].
#'   \emph{Multi-CAST: Multilinguial Corpus of Annotated Spoken Texts}.
#'   (\url{https://lac.uni-koeln.de/en/multicast/}) (Accessed \emph{date}.) } If
#'   for some reason you need to cite this package on its own, please refer to
#'   \code{citation(multicastR)}.
#'
#' @section References: \itemize{\item Haig, Geoffrey & Schnell, Stefan. 2014.
#'   \emph{Annotations using GRAID (Grammatical Relations and Animacy in
#'   Discourse): Introduction and guidelines for annotators.} Version 7.0.
#'   (\url{https://lac.uni-koeln.de/en/multicast/}) (Accessed 2018-03-14.) \item
#'   Riester, Arndt & Baumann, Stefan. 2017. \emph{The RefLex scheme --
#'   Annotation guidelines.} (SinSpeC: Working papers of the SFB 732, 14.)
#'   Stuttgart: University of Stuttgart.
#'   (\url{http://elib.uni-stuttgart.de/handle/11682/9028}) (Accessed
#'   2018-03-14.) \item Schiborr, Nils N. & Schnell, Stefan & Thiele, Hanna.
#'   2018. \emph{RefIND -- Referent Indexing in Natural-language Discourse:
#'   Annotation guidelines.} Version 1.0. Unpublished Manuscript. Bamberg /
#'   Melbourne: University of Bamberg / University of Melbourne.}
#'
#' @seealso \code{\link{mcindex}}.
#'
#' @param vkey A numeric or character vector of length 1 specifying the
#'   requested version of the annotation values. Must be one of the four-digit
#'   version keys in the first column of \code{\link{mcindex}}, or empty. If
#'   empty, \code{multicast} defaults to the most recent version of the
#'   annotations.
#' @return A \code{\link[data.table]{data.table}} with eleven columns:
#'   \describe{ \item{\code{[, 1] corpus}}{The name of the corpus.}
#'   \item{\code{[, 2] file}}{The title of the file. A single long corpus text
#'   may be split into multiple files.} \item{\code{[, 3] uid}}{The utterance
#'   identifier. Uniquely identifies an utterance within a file.} \item{\code{[,
#'   4] word}}{Grammatical words. The tokenized utterances in the object
#'   language.} \item{\code{[, 5] gloss}}{Morphological glosses following the
#'   Leipzig Glossing Rules.} \item{\code{[, 6] graid}}{Annotations using the
#'   GRAID scheme (Haig & Schnell 2014).} \item{\code{[, 7] gform}}{The form
#'   symbol of a GRAID gloss.} \item{\code{[, 8] ganim}}{The person-animacy
#'   symbol of a GRAID gloss.} \item{\code{[, 9] gfunc}}{The function symbol of
#'   a GRAID gloss.} \item{\code{[, 10] refind}}{Referent tracking using the
#'   RefIND scheme (Schiborr et al. 2018).} \item{\code{[, 11] reflex}}{The
#'   information status of newly introduced referents, using a simplified
#'   version of the RefLex scheme (Riester & Baumann 2017).} }
#' @examples
#' # retrieve and print the most recent version of the
#' # Multi-CAST annotations
#' multicast()
#'
#' # retrieve and print the version of the annotation data
#' # published in June 2016
#' multicast(1606)   # or: multicast("1606")
#' @export
multicast <- function(vkey) {
	# check whether vkey is missing
	if (!mcmissarg(vkey)) {
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
					index <- mcindex()

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
									   index[version==vkey,2], ")."))
					}
				}
			}
		}
	} else {
		# 1B: vkey is missing

		# fetch version index
		index <- mcindex()

		# select latest vkey from index
		vkey <- as.character(index[1, 1])
		message(paste0("Found latest version '",
					   index[1, 1],
					   "' (published ",
					   index[1, 2], ")."))
	}

	# construct URL for annotation file
	path <- paste0("http://bamling-research.de/multicastR/multicast_",
				   vkey,
				   ".txt")

	# fetch annotation file
	message("Retrieving annotations from the LAC...")
	tryCatch(
		suppressWarnings(
			mc <- data.table::fread(path,
									sep="\t",
									header=TRUE,
									colClasses=list(factor=1:2, character=3:11),
									encoding="UTF-8",
									showProgress=FALSE)
		),
		error=function(e) { stop("failed to download annotations.") }
	)
	size <- index[version == vkey, 3]
	message(paste0("Success! Downloaded ", size, "."))

	# return annotation values
	return(mc)
}

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

#' Accessing the Multi-CAST version index
#'
#' \code{mcindex} downloads an index of versions of the Multi-CAST annotation
#' data from the servers of the Language Archive Cologne (LAC) and outputs it as
#' a \code{\link[data.table]{data.table}}. The value in the leftmost \code{version}
#' column may be passed to the \code{\link{multicast}} method for access to
#' earlier versions of the annotations.
#'
#' @seealso \code{\link{multicast}}.
#'
#' @return A \code{\link[data.table]{data.table}} with five columns: \describe{
#'   \item{\code{[, 1] version}}{Version key. YYMM format. Used with \code{\link{multicast}}.}
#'   \item{\code{[, 2] date}}{Publication date. YYYY-MM-DD format.}
#'   \item{\code{[, 3] size}}{Total file size in kilobytes.} \item{\code{[, 4] files}}{Number of
#'   corpus files.} \item{\code{[, 5] corpora}}{Names of the corpora
#'   (languages) included in the version.} }
#' @examples
#' # retrieve and print version index
#' mcindex()
#' @export
mcindex <- function() {
	# fetch version index
	path <- "http://bamling-research.de/multicastR/multicast_index.txt"
	message("Retrieving version index from the LAC...")
	tryCatch(
		suppressWarnings(
			index <- data.table::fread(path,
									   sep="\t",
									   header=TRUE,
									   showProgress=FALSE)),
		error=function(e) { stop("failed to download index.") }
	)
	message(paste0("Success! Downloaded <1 KB."))

	# return index
	return(index)
}

# ----------------------------------------------------------------------

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
mcmissarg <- function(x) {
	if ("x" %in% names(match.call())) {
		return(missing(x))
	} else {
		return(TRUE)
	}
}

# ----------------------------------------------------------------------
