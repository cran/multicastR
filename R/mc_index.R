
# ----------------------------------------------------------------------

#' Access the Multi-CAST version index
#'
#' \code{mc_index} downloads a tabular index of the versions of the Multi-CAST
#' corpus data from the servers of the University of Bamberg. The value in the
#' leftmost \code{version} column may be passed to the \code{\link{multicast}}
#' method for access to earlier versions of the annotations.
#'
#' @seealso \code{\link{multicast}}, \code{\link{mc_metadata}},
#'   \code{\link{mc_referents}}, \code{\link{mc_clauses}}
#'
#' @return A \code{\link{data.frame}} with five columns: \describe{
#'   \item{\code{[, 1] version}}{Version key. Used for the \code{vkey} argument
#'   of other functions in this package.} \item{\code{[, 2] date}}{Publication
#'   date in YYYY-MM-DD format.} \item{\code{[, 3] corpora}}{Number of corpora
#'   (languages).} \item{\code{[, 4] texts}}{Number of texts.} \item{\code{[, 5]
#'   size}}{Total file size in kilobytes.} }
#'
#' @examples
#' \dontrun{
#'   # retrieve version index
#'   mc_index()
#' }
#'
#' @export
mc_index <- function() {
	path <- "https://multicast.aspra.uni-bamberg.de/data/mcr/mc_index.tsv"

	message("Retrieving Multi-CAST version index...")
	index <- tryCatch(suppressWarnings(read.csv(path,
												sep = "\t",
											#	colClasses = c("factor",
											#				   "character",
											#				   rep("numeric", 2),
											#				   "character"),
												colClasses = c("factor",
															   "character",
															   "character",
															   rep("numeric", 2)),
												header = TRUE)),
					  error = function(e) {
					  		     stop(paste0("Failed to download index. Cannot access file.\n",
					  		     	         "       The University of Bamberg servers seem to be experiencing problems.\n",
											 "       Please try again later."),
					  		     	  call. = FALSE)
					  		  }
			 )
	message("Downloaded <1 KB.")

	return(index)
}


# ----------------------------------------------------------------------
