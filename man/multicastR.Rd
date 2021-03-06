% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multicastR.R
\docType{package}
\name{multicastR}
\alias{multicastR}
\title{multicastR: A companion to the Multi-CAST collection.}
\description{
The \code{multicastR} package provides a basic interface for accessing the
annotated corpus data in the Multi-CAST collection (edited by Geoffrey Haig
and Stefan Schnell), a database of spoken natural language texts that draws
from a diverse set of languages.
The corpus data are downloaded on command from the servers of the University
of Bamberg via the \code{\link{multicast}} method. Details on the
Multi-CAST project and a list of publications can be found online at
\url{https://multicast.aspra.uni-bamberg.de/}.
}
\section{Licensing}{
 The Multi-CAST annotation data accessed by this package
  are published under a \emph{Create Commons Attribution 4.0 International}
  (CC-BY 4.0) licence (\url{https://creativecommons.org/licenses/by-sa/4.0/}).
  Please refer to the Multi-CAST website for information on how to give
  proper credit to its contributors.
}

\section{Citing Multi-CAST}{
 Data from the Multi-CAST collection should be
  cited as: \itemize{ \item Haig, Geoffrey & Schnell, Stefan (eds.).
  2015. \emph{Multi-CAST: Multilinguial Corpus of Annotated Spoken
  Texts}. (\url{https://multicast.aspra.uni-bamberg.de/}) (Accessed
  \emph{date}.) } If for some reason you need to cite this package
  specifically, please refer to \code{citation(multicastR)}.
}

\seealso{
\code{\link{multicast}}, \code{\link{mc_index}},
  \code{\link{mc_metadata}}, \code{\link{mc_referents}},
  \code{\link{mc_clauses}}
}
