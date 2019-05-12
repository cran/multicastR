# ----------------------------------------------------------------------

#' Access Multi-CAST annotation data
#'
#' \code{multicast} downloads the Multi-CAST annotation data from the servers of
#' the University of Bamberg and outputs them as a
#' \code{\link[data.table]{data.table}}. As the Multi-CAST collection is
#' amenable to extension by additional data sets and annotation schemes,
#' \code{multicast} takes an optional argument to select earlier versions of the
#' annotation data to ensure scientific accountability and reproducability.
#'
#' @section Licensing: The Multi-CAST annotation data accessed by the
#'   \code{multicast} method is published under a \emph{Create Commons
#'   Attribution 4.0 International} (CC-BY 4.0) licence
#'   (\url{https://creativecommons.org/licenses/by-sa/4.0/}). Please refer to
#'   the collection documentation for information on how to give proper credit
#'   to its contributors.
#'
#' @section Citing Multi-CAST: Data from the Multi-CAST collection should be
#'   cited as: \itemize{ \item Haig, Geoffrey & Schnell, Stefan (eds.).
#'   2015. \emph{Multi-CAST: Multilinguial Corpus of Annotated Spoken
#'   Texts}. (\url{https://multicast.aspra.uni-bamberg.de/}) (Accessed
#'   \emph{date}.) } If for some reason you need to cite this package on its
#'   own, please refer to \code{citation(multicastR)}.
#'
#' @section References: \itemize{\item Haig, Geoffrey & Schnell, Stefan. 2014.
#'   \emph{Annotations using GRAID (Grammatical Relations and Animacy in
#'   Discourse): Introduction and guidelines for annotators.} Version 7.0.
#'   (\url{https://multicast.aspra.uni-bamberg.de/})
#'
#'   \item Riester, Arndt & Baumann, Stefan. 2017. The RefLex scheme --
#'   Annotation guidelines. \emph{SinSpeC: Working papers of the SFB 732} 14.
#'   (\url{https://dx.doi.org/10.18419/opus-9011}))
#'
#'   \item Schiborr, Nils N. & Schnell, Stefan & Thiele, Hanna. 2018.
#'   \emph{RefIND -- Referent Indexing in Natural-language Discourse: Annotation
#'   guidelines.} Version 1.1. (\url{https://multicast.aspra.uni-bamberg.de/})}
#'
#' @seealso \code{\link{mc_index}}
#'
#' @param vkey A numeric or character vector of length 1 specifying the
#'   requested version of the annotation values. Must be one of the four-digit
#'   version keys in the first column of \code{\link{mc_index}}, or empty. If
#'   empty or no value is supplied, \code{multicast} automatically retrieves the
#'   most recent version of the annotations. See the examples below for an
#'   illustration.
#' @param legacy.colnames If \code{TRUE}, renames the \code{text} and
#'   \code{gword} columns to what they were called prior to version 1.1.0 of the
#'   package (i.e. \code{file}, \code{word}). This option will be removed in the
#'   future.
#'
#' @return A \code{\link[data.table]{data.table}} with eleven columns:
#'   \describe{ \item{\code{[, 1] corpus}}{The name of the corpus.}
#'   \item{\code{[, 2] text}}{The title of the text. If \code{legacy.colnames}
#'   is \code{TRUE}, this column is named \code{file} instead.} \item{\code{[,
#'   3] uid}}{The utterance identifier. Uniquely identifies an utterance within
#'   a text.} \item{\code{[, 4] gword}}{Grammatical words. The tokenized
#'   utterances in the object language. If \code{legacy.colnames} is
#'   \code{TRUE}, this column is named \code{word} instead.} \item{\code{[, 5]
#'   gloss}}{Morphological glosses following the Leipzig Glossing Rules.}
#'   \item{\code{[, 6] graid}}{Annotations using the GRAID scheme (Haig &
#'   Schnell 2014).} \item{\code{[, 7] gform}}{The form symbol of a GRAID
#'   gloss.} \item{\code{[, 8] ganim}}{The person-animacy symbol of a GRAID
#'   gloss.} \item{\code{[, 9] gfunc}}{The function symbol of a GRAID gloss.}
#'   \item{\code{[, 10] refind}}{Referent tracking using the RefIND scheme
#'   (Schiborr et al. 2018).} \item{\code{[, 11] reflex}}{The information status
#'   of newly introduced referents, using a simplified version of the RefLex
#'   scheme (Riester & Baumann 2017).} }
#'
#' @examples
#'   \dontrun{
#'     # retrieve and print the most recent version of the
#'     # Multi-CAST annotations
#'     multicast()
#'
#'     # retrieve and print the version of the annotation data
#'     # published in June 2016
#'     multicast(1606)   # or: multicast("1606")
#'   }
#'
#' @export
multicast <- function(vkey, legacy.colnames = FALSE) {
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
	path <- paste0("https://multicast.aspra.uni-bamberg.de/data/mcr/multicast_",
				   vkey,
				   ".tsv")

	# fetch annotation file
	message("Retrieving annotations...")
	tryCatch(
		suppressWarnings(
			mc <- data.table::fread(path,
									sep = "\t",
									header = TRUE,
									colClasses = list(factor = 1:2, character = 3:11),
									encoding = "UTF-8",
									showProgress = FALSE)
		),
		error=function(e) { stop("failed to download annotations.") }
	)
	size <- index[version == vkey, 3]
	message(paste0("Success! Downloaded ", size, "."))

	if (legacy.colnames == TRUE) {
		colnames(c("text", "gword"), c("file", "gword"), mc)
	}

	# return annotation values
	return(mc)
}

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

#' Access the Multi-CAST version index
#'
#' \code{mc_index} downloads an index of versions of the Multi-CAST annotation
#' data from the servers of the Language Archive Cologne (LAC) and outputs it as
#' a \code{\link[data.table]{data.table}}. The value in the leftmost
#' \code{version} column may be passed to the \code{\link{multicast}} method for
#' access to earlier versions of the annotations.
#'
#' @seealso \code{\link{multicast}}.
#'
#' @return A \code{\link[data.table]{data.table}} with five columns: \describe{
#'   \item{\code{[, 1] version}}{Version key. YYMM format. Used for
#'   \code{\link{multicast}}'s \code{vkey} argument.} \item{\code{[, 2]
#'   date}}{Publication date. YYYY-MM-DD format.} \item{\code{[, 3] size}}{Total
#'   file size in kilobytes.} \item{\code{[, 4] texts}}{Number of texts.}
#'   \item{\code{[, 5] corpora}}{Names of the corpora (languages) included in
#'   the version.} }
#'
#' @examples
#'   \dontrun{
#'     # retrieve and print version index
#'     mc_index()
#'   }
#'
#' @export
mc_index <- function() {
	# fetch version index
	path <- "https://multicast.aspra.uni-bamberg.de/data/mcr/multicast_index.tsv"
	message("Retrieving version index...")
	tryCatch(
		suppressWarnings(
			index <- data.table::fread(path,
									   sep = "\t",
									   header = TRUE,
									   showProgress = FALSE)),
		error=function(e) { stop("failed to download index.") }
	)
	message(paste0("Success! Downloaded <1 KB."))

	# return index
	return(index)
}

# ----------------------------------------------------------------------

#' Access the Multi-CAST version index
#'
#' Deprecated with multicastR 1.1.0. Use \code{\link{mc_index}} instead.
#'
#' @export
mcindex <- function() {
	.Deprecated("mc_index")
	mc_index()
}
