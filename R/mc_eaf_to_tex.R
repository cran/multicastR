# ----------------------------------------------------------------------

#' Format object language texts and translations for TeX (WIP)
#'
#' \code{mc_eaf_to_tex} reads Multi-CAST EAF files and transforms the contents
#' of the \code{utterance_id}, \code{utterance}, and
#' \code{utterance_translation} tiers into a file with LaTeX markup that can
#' be rendered as a multi-column parallel text by the TeX package
#' \code{paracol}. Regular users of \code{multicastR} will likely find no use
#' for this function; it's sole purpose is to facilitate the creation of the
#' 'Translated texts' supplementary materials included with Multi-CAST.
#'
#' @param readfrom Directory from which to read EAF files. Defaults to the
#'   current working directory.
#' @param recursive Logical. If \code{TRUE}, the function recurses into
#'   subdirectories.
#' @param writeto A directory to which to write output. Defaults to
#'   \code{getwd}.
#'
#' @return Nothing.
#'
#' @examples
#'   \dontrun{
#'     # read all EAF files in the current working directory,
#'     # then write TeX files to the same location
#'     mc_eaf_to_tex()
#'   }
#'
#' @export
mc_eaf_to_tex <- function(readfrom = getwd(), recursive = FALSE, writeto = getwd()) {

	# get list of files to read
	filelist <- list.files(path = readfrom, pattern = "\\.eaf", full.names = TRUE,
						   recursive = recursive)

	# if no files are found, abort
	if (length(filelist) < 1) { stop("No EAF files found.") }

	# read each EAF file and convert it to a data table
	message(paste0("Converting ", length(filelist), " files."))
	mclist <- lapply(filelist, mc_prep_tex)
	message("All done!")

	# write TEX files
	invisible(mc_write_tex(rbindlist(mclist, fill = TRUE), writeto))
}

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

mc_prep_tex <- function(eaffile) {
	# read EAF file
	prep <- mc_read_eaf(eaffile)

	# split utterance_id
	prep[, c("corpus", "text", "uid") := transpose(stringi::stri_split_fixed(uttid_val, "_", n = 3))]

	# give a status update
	message(paste0("Finished reading text '",
				   prep[1, corpus], "_",
				   prep[1, text],
				   "' from file '",
				   prep[5, meta], ".eaf'"))

	# remove duplicate rows
	prep <- unique(prep[, c("corpus", "text", "uid", "utter_val", "utttr_val")])

	# return finished table
	return(prep)
}


# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

#' Generate a TeX'ed parallel text from an EAF and write it to file
#'
#' Generates a TeX file from an EAF file. Make sure to eliminate or escape all
#' special characters considered active by TeX before conversion!
#'
#' @param eaffile Path to and filename of an EAF file to be converted.
#' @param writeto Directory to which to write output to.
#'
#' @keywords internal
mc_write_tex <- function(texts, writeto) {

	# loop through every text
	for (i in 1:length(unique(texts$text))) {
		# take subset
		mctext <- texts[text == unique(texts$text)[i], ]

		# remember corpus and file names
		corpus <- mctext[1, corpus]
		textn <- mctext[1, text]

		# file name of output
		outname <- paste0("mc_", corpus, "_", textn, "_translation.tex")

		# compose vector containing output, start with section heading
		tex <- paste0("\\section{\\textit{", textn, "}}\n")
		tex <- append(tex, "\n% ------------------------------------------------------------------------------------------ %\n")

		# escape active characters
		mctext[, utter_val := gsub("(\\[)", "{\\[", utter_val)]
		mctext[, utter_val := gsub("(\\])", "\\]}", utter_val)]
		mctext[, utttr_val := gsub("(\\[)", "{\\[", utttr_val)]
		mctext[, utttr_val := gsub("(\\])", "\\]}", utttr_val)]
	#	mctext[, utter_val := gsub("([&#%])", "\\\\\\1", utter_val)]
	#	mctext[, utttr_val := gsub("([&#%])", "\\\\\\1", utter_val)]

		# add TeX markup
		mctext[, ulab := gsub("(.*)", "\t\\\\tcolm{\\1}\\\\switchcolumn", uid)]
		mctext[, utter_val := gsub("(.*)", "\t\\\\begin{hyphenrules}{nohyphenation}{\\\\itshape\\\\rmfamily \\1}\\\\end{hyphenrules}\\\\switchcolumn", utter_val)]
		mctext[, utttr_val := gsub("(.*)", "\t\\1\\\\switchcolumn\\*\n", utttr_val)]
		mctext <- mctext[, c("uid", "ulab", "utter_val", "utttr_val")]

		# split text into chunks of size n
		n <- 50
		chunks <- split(mctext, ceiling(seq(1, max(mctext$uid), 1) / n))

		# loop through each chunk
		for (i in 1:length(chunks)) {
			# select chunk
			chunk <- chunks[[i]]

			# compose subsection title
			subsection <- paste0("\\subsection{Utterances~~", min(chunk$uid), "\\textendash", max(chunk$uid), "}\n")

			# remove uid column
			chunk$uid <- NULL

			# merge data table into single vector
			tmp <- c(t(chunk))

			# append subsection heading, environments, text, and a divider line
			tex <- append(tex, subsection)
			tex <- append(tex, "\\begin{paracol}{3}")
			tex <- append(tex, tmp)
			tex <- append(tex, "\\end{paracol}\n")
			tex <- append(tex, "\n% ------------------------------------------------------------------------------------------ %\n")
		}

		# add another divider line
		tex <- append(tex, "\n% ------------------------------------------------------------------------------------------ %\n")

		# output finished TEX file
		stringi::stri_write_lines(tex, fname = paste0(writeto, "/", outname), encoding = "UTF-8")

		# write status message
		message(paste0("Finished converting '", outname, "'."))
	}

	message("All done!")
}

# stop RMD CHECK from complaining about unbound global variables
if (getRversion() >= "2.15.1") {
	utils::globalVariables(c("N", "gword", "gloss", "xkey", "ulab", "text"))
}
