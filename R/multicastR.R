# ----------------------------------------------------------------------

#' multicastR: A companion to the Multi-CAST collection.
#'
#' The \code{multicastR} package provides a basic interface for accessing
#' annotation data in the Multi-CAST collection (edited by Geoffrey Haig and
#' Stefan Schnell), a database of spoken natural language texts that draws from
#' a diverse set of languages and has been annotated across multiple levels.
#' Annotation data is downloaded on command from the servers of the University
#' of Bamberg via the \code{\link{multicast}} method. Details on the
#' Multi-CAST project and a list of publications can be found online at
#' \url{https://multicast.aspra.uni-bamberg.de/}.
#'
#' @seealso \code{\link{multicast}}, \code{\link{mcindex}}.
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
#'   2018[2015]. \emph{Multi-CAST: Multilinguial Corpus of Annotated Spoken
#'   Texts}. (\url{http://multicast.aspra.uni-bamberg.de/}) (Accessed
#'   \emph{date}.) } If for some reason you need to cite this package
#'   specifically, please refer to \code{citation(multicastR)}.
#'
#' @docType package
#' @name multicastR

#' @import data.table
NULL

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

# .onAttach <- function(libname, pkgname) {
# 	packageStartupMessage("This is multicastR v1.1.0.
#						  All corpus data CC-BY 4.0 International.")
# }

# ----------------------------------------------------------------------
