# ----------------------------------------------------------------------

#' Convert TSV referent lists to TSV and TEX
#'
#' \code{mc_reflist} reads lists of referents in TSV format and outputs them (i)
#' as TSV files bundled for the whole collection and (ii) by corpus, and (iii)
#' as files with TEX markup that can be rendered as a multi-column parallel text
#' by the TeX package \code{paracol}. Regular users of \code{multicastR} will
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
#' \dontrun{
#'   # read all TSV files in the current working directory
#'   # and write one TEX file for each TSV file to the same
#'   # location
#'   mc_reflist()
#' }
#'
#' @keywords internal
mc_reflist <- function(readfrom = getwd(), recursive = FALSE, writeto = getwd()) {
	# get list of files to read
	filelist <- list.files(path = readfrom, pattern = "\\.tsv", full.names = TRUE,
						   recursive = recursive)

	# if no files are found, abort
	if (length(filelist) < 1) { stop("No TSV files found.") }

	# read each TSV file and bundle them into a list
	message(paste0("Reading ", length(filelist), " files."))
	refslist <- lapply(filelist, function(x) {
									data.table::fread(x,
													  sep = "\t",
													  header = TRUE,
													  colClasses = list(character = 1:8),
													  encoding = "UTF-8")
								 })
	message("All done!\n")
	bundle <- data.table::rbindlist(refslist, fill = TRUE)

	# construct file path
	if (!grepl("/$", writeto)) {
		fpath <- paste0(writeto, "/")
	} else {
		fpath <- writeto
	}

	# write list for whole collection to a TSV file
	mc_write_refs_tsv(bundle, fpath)
	message("----------")

	# write list for each corpus to a TSV file
	invisible(lapply(split(bundle, f = bundle$corpus), mc_write_refs_tsv, fpath))
	message("----------")

	# convert list for each text to TEX
	invisible(lapply(split(bundle, f = bundle$text), mc_write_refs_tex, fpath))

	# for (i in 1:length(filelist)) {
	# 	mc_write_refs_tex(filelist[i], writeto = writeto)
	# }

	message("All done!")
}

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

#' Write referent lists as TSV
#'
#' Called by \code{\link{mc_reflist}}. Writes bundled lists of referents.
#'
#' @param refdata A \code{data.table} containing lists of referents.
#' @param writeto A directory to which to write output. Defaults to
#'   \code{getwd}.
#'
#' @return Nothing.
#'
#' @keywords internal
mc_write_refs_tsv <- function(refdata, writeto) {
	# construct file name and path
	if (length(unique(refdata$corpus)) > 1) {
		fname <- "mc_list-of-referents.tsv"
	} else {
		fname <- paste0("mc_", refdata[1, corpus], "_list-of-referents.tsv")
	}

	# write data as TSV
	fwrite(refdata,
		   file = paste0(writeto, fname),
		   sep = "\t",
		   quote = FALSE,
		   col.names = TRUE,
		   showProgress = FALSE,
		   verbose = FALSE)

	message(paste0("Finished writing '", fname, "'."))
}

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

#' Write referent lists as TEX
#'
#' Called by \code{\link{mc_reflist}}. Prepares the TSV lists of referents by
#' adding LaTeX markup, then writes them to file.
#'
#' @param refdata A \code{data.table} containing a list of referents.
#' @param writeto A directory to which to write output. Defaults to
#'   \code{getwd}.
#'
#' @return Nothing.
#'
#' @keywords internal
mc_write_refs_tex <- function(refdata, writeto) {
#	# read TSV file
#	refs <- data.table::fread(tsvfile,
#							  sep = "\t",
#							  header = TRUE,
#							  colClasses = list(character = 1:6),
#							  encoding = "UTF-8")

#	# get file name
#	filename <- stringi::stri_split_fixed(
#					utils::tail(
#						stringi::stri_split_fixed(
#							tsvfile, "/", n = -1)[[1]], n = 1), ".", n = -1)[[1]][1]

	# get corpus and text name
#	corpusname <- stringi::stri_split_fixed(filename, "_", n = -1)[[1]][2]
#	textname <- stringi::stri_split_fixed(filename, "_", n = -1)[[1]][3]
	corpusname <- refdata[1, corpus]
	textname <- refdata[1, text]

	# remove corpus and text columns
	refdata$corpus <- NULL
	refdata$text <- NULL

	# add LaTeX markup
	refdata[, refind := gsub("^(.*)$", "\t\\\\tttR{\\1}\\\\switchcolumn", refind)]
	refdata[, label := gsub("(\\d\\d\\d\\d)", "\\\\tcode{\\1}", label)]
	refdata[, label := gsub("^(.*)$", "\t\\\\tit{\\1}\\\\switchcolumn", label)]
	refdata[, description := gsub("(\\d\\d\\d\\d)", "\\\\tcode{\\1}", description)]
	refdata[, description := gsub("^(.*)$", "\t\\1\\\\switchcolumn", description)]
	refdata[, class := gsub("^(.*)$", "\t\\\\tcode{\\1}\\\\switchcolumn", class)]

	refdata[, relations := gsub("(<|>|M) ", "\\1~", relations)]
	refdata[relations != "", relations := gsub("^", "{\\\\raggedleft\\\\tcode{", relations)]
	refdata[relations != "", relations := gsub("$", "~}\\\\par}", relations)]

	refdata[, relations := gsub("^(.*)$", "\t\\1\\\\switchcolumn", relations)]
	refdata[is.na(notes), notes := ""]
	refdata[, notes := gsub("(\\d\\d\\d\\d)", "\\\\tcode{\\1}", notes)]
	refdata[, notes := gsub("^(.*)$", "\t\\1\\\\switchcolumn\\*\n", notes)]

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

	# append subsection heading, environments, text, and a divider line
	rtex <- subsection
	rtex <- append(rtex, "\\begin{paracol}{6}")
	rtex <- append(rtex, header)
	rtex <- append(rtex, c(t(refdata)))	## collapse data table into single vector
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
