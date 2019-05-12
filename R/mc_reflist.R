# ----------------------------------------------------------------------

#' Convert TSV referent lists to TEX
#'
#' \code{mc_reflist} reads lists of referents in TSV format and outputs them as
#' files with TEX markup that can be rendered as a multi-column parallel text by
#' the TeX package \code{paracol}. Regular users of \code{multicastR} will
#' likely find no use for this function; it's sole purpose is to facilitate the
#' creation of the 'List of referents' supplementary materials included with
#' Multi-CAST.
#'
#' @param readfrom Directory from which to read EAF files. Defaults to
#'   \code{getwd}.
#' @param recursive Logical. If \code{TRUE}, the function recurses into
#'   subdirectories.
#' @param writeto A directory to which to write output. Defaults to
#'   \code{getwd}.
#'
#' @return Nothing.
#'
#' @examples
#'   \dontrun{
#'     # read all TSV files in the current working directory
#'     # and write one TEX file for each TSV file to the same
#'     # location
#'     mc_reflist()
#'   }
#'
#' @export
mc_reflist <- function(readfrom = getwd(), recursive = FALSE, writeto = getwd()) {

	# get list of files to read
	filelist <- list.files(path = readfrom, pattern = "\\.tsv", full.names = TRUE,
						   recursive = recursive)

	# if no files are found, abort
	if (length(filelist) < 1) { stop("No EAF files found.") }

	# read each TSV file and convert it to TEX
	message(paste0("Reading ", length(filelist), " files."))

	for (i in 1:length(filelist)) {
		mc_write_refs(filelist[i], writeto = writeto)
	}

	message("All done!")
}

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

#' Write referent lists as TEX
#'
#' Called by \code{\link{mc_reflist}}. Prepares the TSV lists of referents by
#' adding LaTeX markup, then writes them to file.
#'
#' @param tsvfile A TSV file to read.
#' @param writeto A directory to which to write output. Defaults to
#'   \code{getwd}.
#'
#' @return Nothing.
#'
#' @keywords internal
mc_write_refs <- function(tsvfile, writeto) {
	# read TSV file
	refs <- data.table::fread(tsvfile,
							  sep = "\t",
							  header = TRUE,
							  colClasses = list(character = 1:6),
							  encoding = "UTF-8")

	# get file name
	filename <- stringi::stri_split_fixed(utils::tail(stringi::stri_split_fixed(tsvfile, "/", n = -1)[[1]], n = 1), ".", n = -1)[[1]][1]

	# get corpus and text name
	corpusname <- stringi::stri_split_fixed(filename, "_", n = -1)[[1]][2]
	textname <- stringi::stri_split_fixed(filename, "_", n = -1)[[1]][3]

	# add LaTeX markup
	refs[, refind := gsub("^(.*)$", "\t\\\\tttR{\\1}\\\\switchcolumn", refind)]
	refs[, label := gsub("(\\d\\d\\d\\d)", "\\\\tcode{\\1}", label)]
	refs[, label := gsub("^(.*)$", "\t\\\\tit{\\1}\\\\switchcolumn", label)]
	refs[, description := gsub("(\\d\\d\\d\\d)", "\\\\tcode{\\1}", description)]
	refs[, description := gsub("^(.*)$", "\t\\1\\\\switchcolumn", description)]
	refs[, class := gsub("^(.*)$", "\t\\\\tcode{\\1}\\\\switchcolumn", class)]

	refs[, relations := gsub("(<|>|M) ", "\\1~", relations)]
	refs[relations != "", relations := gsub("^", "{\\\\raggedleft\\\\tcode{", relations)]
	refs[relations != "", relations := gsub("$", "~}\\\\par}", relations)]


#	refs[, relations := gsub(", ", ",\\\\\\\\\ ", relations)]
#	refs[, relations := gsub("; ", ";\\\\\\\\\ ", relations)]
#	refs[relations != "", relations := gsub("$", "~}}", relations)]
#	refs[relations != "", relations := gsub("^", "{\\\\raggedleft\\\\tcode{", relations)]

#	refs[relations != "", relations := gsub("^", "{\\\\raggedright\\\\tcode{", relations)]
#	refs[, relations := gsub("\\\\\\\\ ", "}\\\\\\\\\\\\tcode{", relations)]
#	refs[relations != "", relations := gsub("$", "}}", relations)]

	refs[, relations := gsub("^(.*)$", "\t\\1\\\\switchcolumn", relations)]
	refs[, notes := gsub("(\\d\\d\\d\\d)", "\\\\tcode{\\1}", notes)]
	refs[, notes := gsub("^(.*)$", "\t\\1\\\\switchcolumn\\*\n", notes)]

	# compose subsection title
	subsection <- paste0("\\subsection{\\textit{", textname, "}}\n")

	# compose table header
	header <- paste0("\t\\microtypesetup{protrusion=false}",
					 "\t\\raggedright\n",
					 "\t{\\itshape\\color{colourM}index\\switchcolumn\n",
					 "\tlabel\\switchcolumn\n",
					 "\tdescription\\switchcolumn\n",
					 "\tclass\\switchcolumn\n",
					 "\trelations\\switchcolumn\n",
					 "\tnotes\\smallskip}\\switchcolumn*\n")

	# collapse data table into single vector
	refs <- c(t(refs))

	# append subsection heading, environments, text, and a divider line
	rtex <- subsection
	rtex <- append(rtex, "\\begin{paracol}{6}")
	rtex <- append(rtex, header)
	rtex <- append(rtex, refs)
	rtex <- append(rtex, "\\end{paracol}\n")
	rtex <- append(rtex, "\n% ------------------------------------------------------------------------------------------ %")

	# construct file name
	outname <- paste0("/mc_", corpusname, "_", textname, "_referents.tex")

	# output finished TEX file
	stringi::stri_write_lines(rtex, fname = paste0(writeto, outname), encoding = "UTF-8")

	message(paste0("Finished writing 'mc_", corpusname, "_", textname, "_referents.tex'."))
}

# stop RMD CHECK from complaining about unbound global variables
if (getRversion() >= "2.15.1") {
	utils::globalVariables(c("refind", "label", "description", "relations", "notes"))
}
