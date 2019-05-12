# ----------------------------------------------------------------------

#' Convert EAF files to TSV (WIP)
#'
#' \code{mc_eaf_to_tsv} converts EAF files produced by the linguistic annotation
#' software ELAN into one or multiple tab-separated values (TSV) tables. The EAF
#' files must have the correct tier structure with the correct tier names, or
#' conversion fails. See the Multi-CAST documentation for details. File are
#' added to the TSV table in the alphabetical order of their file names.
#'
#' @param readfrom Directory from which to read EAF files. Defaults to the
#'   current working directory.
#' @param recursive Logical. If \code{TRUE}, the function recurses into
#'   subdirectories.
#' @param split Logical. If \code{FALSE}, all EAF files that are read are bound
#'   into a single data table. If \code{TRUE}, a list of data tables is returned
#'   instead, with one list item per text (which may be split across multiple
#'   EAF files). If \code{write} is \code{TRUE}, written output is either a
#'   single TSV file (for \code{split == TRUE}) or one TSV file per text read
#'   (for \code{split == FALSE}). In the latter case TSV files combining all
#'   texts from each corpus are also produced.
#' @param write Logical. If \code{TRUE}, also creates output in \code{TSV}
#'   format.
#' @param writeto A directory to which to write output. Defaults to
#'   \code{getwd}. Ignored if \code{write} is \code{FALSE}.
#' @param filename A length 1 character vector containing the name of the
#'   written output. If empty, defaults to "multicast_YYMM", where 'YY' are the
#'   last two digits of the current year and 'MM' the current month. Ignored if
#'   \code{write} is \code{FALSE} and/or if \code{split} is \code{TRUE}, as in
#'   the latter case file names are instead generated from text metadata.
#'
#' @return Either a \code{\link[data.table]{data.table}} or \code{list} of
#'   \code{data.table}s of the form produced by \code{\link{multicast}},
#'   containing the annotation values of the EAF files read.
#'
#' @examples
#'   \dontrun{
#'     # read all EAF files in the current working directory,
#'     # returns a data table of the kind accessed by multicast()
#'     mc_eaf_to_tsv()
#'
#'     # also produce a file 'mydata.tsv' containing all read data
#'     mc_eaf_to_tsv(write = TRUE, filename = "mydata")
#'
#'     # instead of a single monolithic table, return a list
#'     # of tables and produce one TSV file for each text
#'     mc_eaf_to_tsv(write = TRUE, split = TRUE)
#'   }
#'
#' @export
mc_eaf_to_tsv <- function(readfrom = getwd(), recursive = FALSE, split = FALSE,
						  write = FALSE, writeto = getwd(), filename = "") {

	# get list of files to read
	filelist <- list.files(path = readfrom, pattern = "\\.eaf", full.names = TRUE,
						   recursive = recursive)

	# if no files are found, abort
	if (length(filelist) < 1) { stop("No EAF files found.") }

	# read each EAF file and convert it to a data table
	message(paste0("Reading ", length(filelist), " files."))
	mclist <- lapply(filelist, mc_prep_tsv)
	message("All done!")
	mcall <- rbindlist(mclist, fill = TRUE)

	# write data to file?
	if (write == TRUE) {
		# split output into multiple files?
		if (split == TRUE) {
			# write each text to a separate file
			invisible(lapply(mclist, mc_write_tsv, writeto = writeto, filename = "", split = TRUE))

			# write all texts of each corpus to a file
			mccorp <- split(mcall, f = mcall$corpus)
			invisible(lapply(mccorp, mc_write_tsv, writeto = writeto, filename = "", split = TRUE, bycorpus = TRUE))
			message("All done!")
		} else {
			# write all texts to a single file
			invisible(mc_write_tsv(mcall, writeto, filename, split = FALSE))
			message("All done!")
		}
	}

	# remove metadata column
	mclist <- lapply(mclist, function(x) { x <- mclist[[1]] })
	mcall$meta <- NULL

	# return as single table or as list of tables?
	if (split == TRUE) {
		return(mclist)
	} else {
		return(mcall)
	}
}

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

#' Shape input from EAF files for TSV output
#'
#' Called by \code{\link{mc_eaf_to_tsv}} for each EAF file in
#' the list. While reading the EAFs themselves is handled by
#' \code{\link{mc_read_eaf}}, this function brings the output into
#' its proper shape.
#'
#' @param eaffile Path to and filename of an EAF file to be converted.
#'
#' @return A \code{\link[data.table]{data.table}} containing the annotation
#'   values of a single EAF file.
#'
#' @keywords internal
mc_prep_tsv <- function(eaffile) {
	# read and convert EAF file
	mcastr <- mc_read_eaf(eaffile)

	# select columns
	mcastr <- mcastr[, c("uttid_val", "gwords_val", "gloss_val", "graid_val",
						 "refind_val", "reflex_val", "meta")]

	# split utterance_id into corpus, text, and uid
	mcastr[, c("corpus", "text", "uid") := transpose(stringi::stri_split_fixed(uttid_val, "_", n = 3))]

#	# add column with text part
#	tmp <- stringi::stri_split_fixed(
#				tail(stringi::stri_split_fixed(eaffile, "/", n = -1)[[1]], n = 1),
#				"_", n = -1)
#
#	if (length(tmp[[1]]) == 4) {
#		mcastr[, part := stringi::stri_sub(tmp[[1]][4], from = 1L, to = 1L)]
#	} else {
#		mcastr[, part := ""]
#	}

	# split graid into form, animacy, and function
	mcastr[, c("gformanim", "gfunc") := transpose(stringi::stri_split_fixed(graid_val, ":", n = 2))]
	mcastr[, c("gform", "ganim") := transpose(stringi::stri_split_fixed(gformanim, ".", n = 2))]

	# restore split clause boundaries
#	mcastr[grepl("#|%", graid_val) & ganim != "", ganim := ""]
	mcastr[grepl("#|%", graid_val) & ganim != "" & !grepl("[12hd]", ganim), gform := paste0(gform, ".", ganim)]
	mcastr[grepl("#|%", graid_val) & ganim != "" & !grepl("[12hd]", ganim), ganim := ""]

	# replace NAs with empty strings
	mcastr[is.na(mcastr)] <- ""

	# select columns
	mcastr <- mcastr[, c("corpus", "text", "uid", "gwords_val", "gloss_val",
						 "graid_val", "gform", "ganim", "gfunc", "refind_val", "reflex_val", "meta")]

	# rename columns
	setnames(mcastr, c(4:6, 10:11), c("gword", "gloss", "graid", "refind", "reflex"))

	# !!!!!!!!!! #
	# REMOVE ROWS THAT HAVE NO MATERIAL IN THE GWORDS, GLOSS, AND GRAID COLUMNS
	# these should by taken care of in the EAF files instead!
	mcastr <- mcastr[!(gword == "" & gloss == "" & graid == ""), ]
	# !!!!!!!!!! #


	# give a status update
	message(paste0("Finished reading text '",
				   mcastr[1, corpus], "_",
				   mcastr[1, text],
				   "' from file '",
				   mcastr[5, meta], ".eaf'"))


	# return finished table
	return(mcastr)
}

# stop RMD CHECK from complaining about unbound global variables
if (getRversion() >= "2.15.1") {
	utils::globalVariables(c("uttid_val", "graid_val",
							 "gformanim", "gform", "ganim",
							 "corpus", "filename", "text"))
}

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

#' Write a TSV table to file
#'
#' Writes a table generated by \code{\link{mc_prep_tsv}} to a TSV file.
#'
#' @param mcctext A \code{\link[data.table]{data.table}}.
#' @param writeto Directory to which to write output to. Defaults to
#'   \code{getwd}.
#' @param filename A length 1 character vector containing the name of the
#'   written output. If empty and \code{split} is \code{FALSE}, defaults to
#'   "multicast_YYMM". Ignored if \code{split} is \code{TRUE}, as file names are
#'   automatically constructed.
#' @param split Logical. If \code{TRUE}, generates file names from text
#'   metadata.
#' @param bycorpus Logical. If \code{TRUE}, generates file names only from
#'   corpus names.
#'
#' @keywords internal
mc_write_tsv <- function(mctext, writeto, filename, split = FALSE, bycorpus = FALSE) {
	# should the texts be split into separate files?
	if (split == FALSE) {
		# check for empty filename
		if (filename == "") {
			filename <- paste0("multicast_",
							   sub(".*?(\\d\\d)$", "\\1", date()),
							   stringi::stri_pad(match(sub("^.*? (.*?) .*$", "\\1",
							   						   date()), month.abb),
							   				     2, pad = "0"))
		}
	} else if (bycorpus == FALSE) {
		filename <- mctext[5, meta]
	} else {
		filename <- paste0("mc_", mctext[1, corpus])
	}

	# remove metadata column
	mctext$meta <- NULL

	# write TSV file
	if (grepl("/$", writeto)) {
		fpath <- paste0(writeto, filename, ".tsv")
	} else {
		fpath <- paste0(writeto, "/", filename, ".tsv")
	}

	fwrite(mctext,
		   file = fpath,
		   sep = "\t",
		   quote = FALSE,
		   col.names = TRUE,
		   showProgress = FALSE,
		   verbose = FALSE)

	# write confirmation message to console
	message(paste0("TSV written to '", fpath, "'."))
}
