
# ----------------------------------------------------------------------

#' Access Multi-CAST annotation data
#'
#' \code{multicast} downloads corpus data from the Multi-CAST collection (Haig &
#' Schnell 2015) from the servers of the University of Bamberg. As the
#' Multi-CAST collection is continuously evolving through the addition of
#' further data sets and the revision of older annotations, the \code{multicast}
#' function takes an optional argument \code{vkey} to select earlier versions of
#' the annotation data, ensuring scientific accountability and the
#' reproducibility of results.
#'
#' @section Licensing: The Multi-CAST annotation data accessed by this package
#'   are published under a \emph{Create Commons Attribution 4.0 International}
#'   (CC-BY 4.0) licence
#'   (\url{https://creativecommons.org/licenses/by-sa/4.0/}). Please refer to
#'   the Multi-CAST website for information on how to give proper credit to its
#'   contributors.
#'
#' @section Citing Multi-CAST: Data from the Multi-CAST collection should be
#'   cited as: \itemize{ \item Haig, Geoffrey & Schnell, Stefan (eds.). 2015.
#'   \emph{Multi-CAST: Multilinguial Corpus of Annotated Spoken Texts}.
#'   (\url{https://multicast.aspra.uni-bamberg.de/}) (Accessed \emph{date}.) } If
#'   for some reason you need to cite this package specifically, please refer to
#'   \code{citation(multicastR)}.
#'
#' @section References: \itemize{\item Haig, Geoffrey & Schnell, Stefan. 2014.
#'   \emph{Annotations using GRAID (Grammatical Relations and Animacy in
#'   Discourse): Introduction and guidelines for annotators.} Version 7.0.
#'   (\url{https://multicast.aspra.uni-bamberg.de/#annotations})
#'
#'   \item Schiborr, Nils N. & Schnell, Stefan & Thiele, Hanna. 2018.
#'   \emph{RefIND -- Referent Indexing in Natural-language Discourse: Annotation
#'   guidelines.} Version 1.1.
#'   (\url{https://multicast.aspra.uni-bamberg.de/#annotations})}
#'
#' @seealso \code{\link{mc_index}}, \code{\link{mc_metadata}},
#'   \code{\link{mc_referents}}, \code{\link{mc_clauses}}
#'
#' @param vkey A four-digit number specifying the requested version of the
#'   metadata. Must be one of the version keys listed in the first column of
#'   \code{\link{mc_index}}, or empty. If empty, the most recent version of the
#'   metadata is retrieved automatically.
#'
#' @return A \code{\link{data.frame}} with eleven columns: \describe{
#'   \item{\code{[, 1] corpus}}{The name of the corpus.} \item{\code{[, 2]
#'   text}}{The name of the text.} \item{\code{[, 3] uid}}{The utterance
#'   identifier. Uniquely identifies an utterance within a text.} \item{\code{[,
#'   4] gword}}{Grammatical words. The tokenized utterances in the object
#'   language.} \item{\code{[, 5] gloss}}{Morphological glosses following the
#'   Leipzig Glossing Rules.} \item{\code{[, 6] graid}}{Annotations with the
#'   GRAID scheme (Haig & Schnell 2014).} \item{\code{[, 7] gform}}{The form
#'   symbol of a GRAID gloss.} \item{\code{[, 8] ganim}}{The person-animacy
#'   symbol of a GRAID gloss.} \item{\code{[, 9] gfunc}}{The function symbol of
#'   a GRAID gloss.} \item{\code{[, 10] refind}}{Referent tracking using the
#'   RefIND scheme (Schiborr et al. 2018).} \item{\code{[, 11]
#'   isnref}}{Annotations of the information status of newly introduced
#'   referents.} }
#'
#' @examples
#' \dontrun{
#'   # retrieve and print the most recent version of the
#'   # Multi-CAST annotations
#'   multicast()
#'
#'   # retrieve the version of the annotation data published
#'   # in January 2021
#'   multicast(2021)
#' }
#'
#' @export
multicast <- function(vkey = NULL) {

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
	path <- paste0("https://multicast.aspra.uni-bamberg.de/data/",
				   vkey,
				   "/multicast.tsv")

	message("Retrieving Multi-CAST corpus data...")
	mc <- tryCatch(suppressWarnings(read.csv(path,
											 sep = "\t",
											 header = TRUE,
											 colClasses = c(rep("factor", 2),
											 			    rep("character", 9)),
											 encoding = "UTF-8")),
				   error = function(e) {
				   		      stop(paste0("Failed to download data. Cannot access file.\n",
					  		     	      "       The servers of the University of Bamberg seem to be experiencing problems.\n",
										  "       Please try again later."),
								   call. = FALSE)
					  	   }
			       )
	message(paste0("Downloaded ", index[index$version == vkey, "size"], "."))

	# return table with annotation values
	return(mc)

}


# ----------------------------------------------------------------------
