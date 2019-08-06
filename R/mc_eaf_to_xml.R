# ----------------------------------------------------------------------

#' Convert EAF files to XML (WIP)
#'
#' \code{mc_eaf_to_xml} converts EAF files produced by the linguistic annotation
#' software ELAN into one or multiple XML files. The EAF files must have the
#' correct tier structure and names dictated by the Multi-CAST design, else
#' conversion fails. Refer to the Multi-CAST documentation for details about the
#' necessary structure of the EAF files, as well as about the structure of the
#' XML files produced by this function.
#'
#' @param vkey Character. Version of the annotations. This information is not
#'   part of the EAF files, so it needs to be specified manually.
#' @param readfrom Directory from which to read EAF files. Defaults to
#'   \code{getwd}.
#' @param recursive Logical. If \code{TRUE}, the function recurses into
#'   subdirectories.
#' @param split Logical. If \code{FALSE}, all EAF files that are read are bound
#'   into a single XML file. If \code{TRUE}, output consists of one XML file for
#'   each text read (which may be split across multiple EAF files), plus one XML
#'   file bundling all texts from each Multi-CAST corpus. Files combining all
#'   texts from each corpus are also produced.
#' @param writeto A directory to which to write output. Defaults to
#'   \code{getwd}.
#' @param filename A length 1 character vector containing the name of the
#'   written output. If empty, defaults to "multicast_YYMM", where 'YY' are the
#'   last two digits of the current year and 'MM' the current month. Ignored if
#'   \code{split} is \code{TRUE}, as in the latter case file names are instead
#'   generated from text metadata.
#' @param skipempty Logical. If \code{TRUE}, empty leaf nodes in the XML will not be
#'   drawn.
#'
#' @examples
#' \dontrun{
#'   # read all EAF files in the current working directory
#'   # and write one XML file for each text to the same
#'   # location
#'   mc_eaf_to_xml()
#'
#'   # same as above, but bundle all data into one large XML file
#'   # for entire collection plus one XML file for each corpus
#'   mc_eaf_to_xml(split = TRUE)
#' }
#'
#' @keywords internal
mc_eaf_to_xml <- function(vkey = "", readfrom = getwd(), recursive = FALSE,
						  split = FALSE, writeto = getwd(), filename = "",
						  skipempty = TRUE) {

	# get list of files to read
	filelist <- list.files(path = readfrom, pattern = "\\.eaf", full.names = TRUE,
						   recursive = recursive)

	# if no files are found, abort
	if (length(filelist) < 1) { stop("No EAF files found.") }

	# read each EAF file and convert it to a data table
	message(paste0("Reading ", length(filelist), " files."))
	mclist <- lapply(filelist, mc_prep_xml)
	mcall <- data.table::rbindlist(mclist, fill = TRUE)

	# check for empty vkey
	# if missing, generate from current year and month
	if (vkey == "") {
		vkey <- paste0(sub(".*?(\\d\\d)$", "\\1", date()),
					   stringi::stri_pad(match(sub("^.*? (.*?) .*$", "\\1",
					   						   date()), month.abb),
					   				     2, pad = "0"))
	}

	# split output into multiple files?
	if (split == TRUE) {
		# get file names from pre-XML tables
		f_names <- unlist(lapply(mclist, "[", 5, meta))
		c_names <- paste0("mc_", unique(mcall[, corpus]))

		# write each text to a separate file
		mcfile_xml <- lapply(mclist, mc_build_xml, vkey = vkey, skipempty = skipempty)
		message("Writing to file...")
		invisible(mapply(mc_write_xml, mctext = mcfile_xml, writeto = writeto, filename = f_names))

		# write all texts of each corpus to a file
		mccorp <- split(mcall, f = mcall$corpus)
		mccorp_xml <- lapply(mccorp, mc_build_xml, vkey = vkey, skipempty = skipempty)
		message("Writing to file...")
		invisible(mapply(mc_write_xml, mctext = mccorp_xml, writeto = writeto, filename = c_names))

	} else {
		# check for empty filename
		if (filename == "") {
			filename <- paste0("multicast_", vkey)
		}

		# write all texts to a single file
		mcall_xml <- mc_build_xml(mcall, vkey, skipempty)
		message("Writing to file...")
		invisible(mc_write_xml(mcall_xml, writeto = writeto, filename = filename))
	}
}

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

#' Shape input from EAF files for XML output
#'
#' Called by \code{\link{mc_eaf_to_xml}} for each EAF file in
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
mc_prep_xml <- function(eaffile) {
	# read and convert EAF file
	mcr <- mc_read_eaf(eaffile)

	# select columns
	mcr <- mcr[, c("timeslot_val.x", "timeslot_val.y",
				   "uttid_val", "utter_val", "utttr_val", "gwords_val", "gloss_val",
				   "graid_val", "refind_val", "reflex_val", "comnt_val", "meta")]

	# split utterance_id into corpus, text, and uid
	mcr[, c("corpus", "text", "uid") := transpose(stringi::stri_split_fixed(uttid_val, "_", n = 3))]

	# split graid into form, animacy, and function
	mcr[, c("gformanim", "gfunc") := transpose(stringi::stri_split_fixed(graid_val, ":", n = 2))]
	mcr[, c("gform", "ganim") := transpose(stringi::stri_split_fixed(gformanim, ".", n = 2))]

	# restore split clause boundaries
	mcr[grepl("#|%", graid_val) & ganim != "" & !grepl("[12hd]", ganim), gform := paste0(gform, ".", ganim)]
	mcr[grepl("#|%", graid_val) & ganim != "" & !grepl("[12hd]", ganim), ganim := ""]

	# add column with file name
	mcr[, file := mcr[5, meta]]

	# give a status update
	message(paste0("Finished reading text '",
				   mcr[1, corpus], "_",
				   mcr[1, text],
				   "' from file '",
				   mcr[5, meta], ".eaf'"))

	# return finished table
	return(mcr)
}

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

#' Construct XML tree
#'
#' Uses the \code{\link[XML:xmlTree]{XML}} package to construct the XML tree.
#'
#' @param eaffile Path to and filename of an EAF file to be converted.
#'
#' @return An XML tree object of classes \code{XMLInternalDocument} and
#'   \code{XMLAbstractDocument}, as defined by the
#'   \code{\link[XML:xmlTree]{XML}} package.
#'
#' @keywords internal
mc_build_xml <- function(mctext, vkey, skipempty = TRUE) {
	time_root <- proc.time()[[3]]

	# add row IDs
	mctext[, I := .I]

	# build XML tree
	xmldoc <- XML::newXMLDoc()

	# - ROOT - - - - - - - - - -
	n_multicast <- XML::newXMLNode(doc = xmldoc,
								   name = "multicast",
								   attrs = c(version = vkey))
	message("-- Starting XML generation.")


	# - CORPUS - - - - - - - - - -
	corpora <- unique(mctext[, corpus])

	for(i_corpus in 1:length(corpora)) {
		time_corpus <- proc.time()[[3]]

		# corpus name
		corpus_name <- corpora[i_corpus]
		message(paste0("|  |- Writing corpus '", corpus_name, "' (", i_corpus, "/", length(corpora), ")."))

		# create corpus node
		n_corpus <- XML::newXMLNode(parent = n_multicast,
									name = "corpus",
									attrs = c(c_name = corpus_name))


		# - TEXT - - - - - - - - - -
		texts <- unique(mctext[corpus == corpora[i_corpus], text])

		for (i_text in 1:length(texts)) {
			time_text <- proc.time()[[3]]

			# text name
			text_name <- texts[i_text]
			message(paste0("|  |  |- Writing text '", text_name, "' (", i_text, "/", length(texts), ")."))

			# annotators
			annotators <- mctext[text == texts[i_text], meta][1]
			annotators <- gsub("amp;", "", annotators)

			# speaker
			speaker_id <- mctext[text == texts[i_text], meta][2]

			# create file node
			n_text <- XML::newXMLNode(parent = n_corpus,
									  name = "text",
									  attrs = c(t_name = text_name,
									  		    annotators = annotators,
									  		    speaker = speaker_id))

				# - FILE - - - - - - - - - -
				files <- unique(mctext[text == texts[i_text], file])

				for (i_file in 1:length(files)) {
					time_file <- proc.time()[[3]]

					# file name
					file_name <- files[i_file]
					message(paste0("|  |  |  |- Writing file '", file_name, "' (", i_file, "/", length(files), ")."))

					# audio
					audio_name <- mctext[3, meta]

					# updated
					updated <- mctext[4, meta]

					# create file node
					n_file <- XML::newXMLNode(parent = n_text,
											  name = "file",
											  attrs = c(f_name = file_name,
											  		    audio = audio_name,
											  		    updated = updated))


						# - UTTERANCE UNIT - - - - - - - - - -
						for (i_utterance in 1:length(unique(mctext[file == files[i_file], uid]))) {
							# utterance ID
							utt_uid <- unique(mctext[file == files[i_file], uid])[i_utterance]

							# start time
							utt_start <- mctext[file == files[i_file] & uid == utt_uid, timeslot_val.x][1]

							# end time
							utt_end <- mctext[file == files[i_file] & uid == utt_uid, timeslot_val.y][1]

							# create unit node
							n_unit <- XML::newXMLNode(parent = n_file,
													  name = "unit",
													  attrs = c(uid = utt_uid,
													  		   start_time = utt_start,
													  		   end_time = utt_end))

							# - UTTERANCE ID - - - - - - - - - -
							# utterance ID
							utt_id <- mctext[file == files[i_file] & uid == utt_uid, uttid_val][1]

							# create utterance ID node
							n_uttid <- XML::newXMLNode(parent = n_unit,
													   name = "utterance_id",
													   utt_id)

							# - UTTERANCE TEXT - - - - - - - - - -
							# utterance text
							utt_text <- mctext[file == files[i_file] & uid == utt_uid, utter_val][1]

							# create utterance text node
							n_utterance <- XML::newXMLNode(parent = n_unit,
														   name = "utterance",
														   utt_text)

							# - UTTERANCE TRANSLATION - - - - - - - - - -
							# translation
							utt_trans <- mctext[file == files[i_file] & uid == utt_uid, utttr_val][1]

							# create utterance translation node
							n_translation <- XML::newXMLNode(parent = n_unit,
															 name = "utterance_translation",
															 utt_trans)

							# - COMMENTS - - - - - - - - - -
							# comments
							utt_comments <- mctext[file == files[i_file] & uid == utt_uid, comnt_val][1]

							# check if current comments slot is empty,
							# do not create empty nodes if skipempty = TRUE
							if (skipempty == FALSE | !is.na(utt_comments)) {
								# create comments node
								n_comments <- XML::newXMLNode(parent = n_unit,
															  name = "add_comments",
															  utt_comments)
							}

							# - ANNOTATIONS - - - - - - - - - -
							# create annotations node
							n_annotations <- XML::newXMLNode(parent = n_unit,
															 name = "annotations")

							# - SEGMENT - - - - - - - - - -
							for (i_segment in 1:nrow(mctext[file == files[i_file] & uid == utt_uid])) {
								# create segment node
								n_segment <- XML::newXMLNode(parent = n_annotations,
															 name = "segment")

								# - GRAMMATICAL WORDS - - - - - - - - - -
								# grammatical words
								ann_word <- mctext[file == files[i_file] & uid == utt_uid][i_segment, gwords_val]

								# create grammatical words node
								n_word <- XML::newXMLNode(parent = n_segment,
														  name = "gword",
														  ann_word)

								# - GLOSS - - - - - - - - - -
								# glosses
								ann_gloss <- mctext[file == files[i_file] & uid == utt_uid][i_segment, gloss_val]

								# create gloss node
								n_gloss <- XML::newXMLNode(parent = n_segment,
														   name = "gloss",
														   ann_gloss)

								# - GRAID - - - - - - - - - -
								# GRAID
								ann_graid <- mctext[file == files[i_file] & uid == utt_uid][i_segment, graid_val]

								# create GRAID node
								n_graid <- XML::newXMLNode(parent = n_segment,
														   name = "graid",
														   ann_graid)

								# - REFIND - - - - - - - - - -
								# RefIND
								ann_refind <- mctext[file == files[i_file] & uid == utt_uid][i_segment, refind_val]

								# check if current RefIND slot is empty,
								# do not create empty nodes if skipempty = TRUE
								if (skipempty == FALSE | !is.na(ann_refind)) {
									# create RefIND node
									n_refind <- XML::newXMLNode(parent = n_segment,
																name = "refind",
																ann_refind)
								}

								# - RefLex - - - - - - - - - -
								# RefLex
								ann_reflex <- mctext[file == files[i_file] & uid == utt_uid][i_segment, reflex_val]

								# check if current RefLex slot is empty,
								# do not create empty nodes if skipempty = TRUE
								if (skipempty == FALSE | !is.na(ann_reflex)) {
									# create RefLex node
									n_reflex <- XML::newXMLNode(parent = n_segment,
																name = "reflex",
																ann_reflex)
								}
							}
						}

						message(paste0("|  |  |  `- File finished in ", round(proc.time()[[3]] - time_file, 0), "s!"))
					}

			message(paste0("|  |  `- Text finished in ", round(proc.time()[[3]] - time_text, 0), "s!"))
		}

		message(paste0("|  `- Corpus finished in ", round(proc.time()[[3]] - time_corpus, 0), "s!"))
	}

	message("`- All done in ", round(proc.time()[[3]] - time_root, 0), "s!")

	return(xmldoc)
}

# stop RMD CHECK from complaining about unbound global variables
if (getRversion() >= "2.15.1") {
	utils::globalVariables(c("meta", "uid", "timeslot_val.x", "timeslot_val.y",
							 "utter_val", "utttr_val", "gwords_val", "gloss_val",
							 "refind_val", "reflex_val", "text", "comnt_val"))
}

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

#' Write an XML table to file
#'
#' Writes an XML tree generated by \code{\link{mc_prep_xml}} to an XML file.
#'
#' @param mctext An XML document generated with \code{\link[XML:xmlTree]{XML}}.
#' @param writeto Directory to which to write output to. Defaults to
#'   \code{getwd}.
#' @param filename A length 1 character vector containing the name of the
#'   written output.
#'
#' @keywords internal
mc_write_xml <- function(mctext, writeto, filename) {
	# write XML file
	tmp <- XML::saveXML(mctext, encoding = "UTF-8")
	tmp <- strsplit(tmp, "\n")[[1]]
	tmp <- gsubfn::gsubfn("^( +)", x ~ sprintf("%*s", 2 * nchar(x), " "), tmp)	# add indentation

	if (grepl("/$", writeto)) {
		fpath <- paste0(writeto, filename, ".xml")
	} else {
		fpath <- paste0(writeto, "/", filename, ".xml")
	}

	write(tmp, file = fpath)

	# write confirmation message to console
	message(paste0("XML written to '", fpath, "'."))
}
