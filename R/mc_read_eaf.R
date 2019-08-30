# ----------------------------------------------------------------------
#' Read EAF file content
#'
#' Reads a specified EAF file. As EAF files are XML files, this function relies
#' on the \code{xml2} package and XPath to do its job. The EAF file to be read
#' must have a certain internal structure as dictated by the Multi-CAST design;
#' see the Multi-CAST documentation for more information.
#'
#' @param eaffile Path to and filename of an EAF file to be converted.
#'
#' @return A \code{\link[data.table]{data.table}} containing all data fields of
#'   the EAF file.
#'
#' @keywords internal
mc_read_eaf <- function(eaffile) {

	# read EAF file
	raweaf <- xml2::read_xml(eaffile, encoding = "UTF-8")

	# time slots
	timeslot_id <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIME_SLOT"), "TIME_SLOT_ID")
	timeslot_val <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIME_SLOT"), "TIME_VALUE")

	tab_timeslot <- data.table(timeslot_id, timeslot_val)


	# tier utterance_id
	# root tier, aligned with time slots
	uttid_id <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='utterance_id']/ANNOTATION/ALIGNABLE_ANNOTATION"), "ANNOTATION_ID")
	uttid_tsbegin <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='utterance_id']/ANNOTATION/ALIGNABLE_ANNOTATION"), "TIME_SLOT_REF1")
	uttid_tsend <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='utterance_id']/ANNOTATION/ALIGNABLE_ANNOTATION"), "TIME_SLOT_REF2")
	uttid_val <- xml2::xml_text(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='utterance_id']/ANNOTATION/ALIGNABLE_ANNOTATION/ANNOTATION_VALUE"))

	tab_uttid <- data.table(uttid_id, uttid_tsbegin, uttid_tsend, uttid_val)


	# tier utterance
	# child of utterance_id, 1-to-1 relation
	utter_id <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='utterance']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_ID")
	utter_ref <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='utterance']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_REF")
	utter_val <- xml2::xml_text(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='utterance']/ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE"))

	tab_utter <- data.table(utter_id, utter_ref, utter_val)


	# tier grammatical_words
	# child of utterance, 1-to-n relation
	gwords_id <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='grammatical_words']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_ID")
	gwords_ref <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='grammatical_words']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_REF")
	gwords_val <- xml2::xml_text(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='grammatical_words']/ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE"))

	tab_gwords <- data.table(gwords_id, gwords_ref, gwords_val)


	# tier gloss
	# child of grammatical_words, 1-to-1 relation
	gloss_id <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='gloss']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_ID")
	gloss_ref <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='gloss']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_REF")
	gloss_val <- xml2::xml_text(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='gloss']/ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE"))

	tab_gloss <- data.table(gloss_id, gloss_ref, gloss_val)


	# tier graid
	# child of gloss, 1-to-1 relation
	graid_id <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='graid']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_ID")
	graid_ref <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='graid']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_REF")
	graid_val <- xml2::xml_text(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='graid']/ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE"))

	tab_graid <- data.table(graid_id, graid_ref, graid_val)


	# tier refind
	# child of graid, 1-to-1 relation
	refind_id <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='refind']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_ID")
	refind_ref <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='refind']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_REF")
	refind_val <- xml2::xml_text(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='refind']/ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE"))

	tab_refind <- data.table(refind_id, refind_ref, refind_val)


	# tier isnref
	# child of refind, 1-to-1 relation
	reflex_id <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='isnref']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_ID")
	reflex_ref <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='isnref']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_REF")
	reflex_val <- xml2::xml_text(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='isnref']/ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE"))

	tab_reflex <- data.table(reflex_id, reflex_ref, reflex_val)


	# utterance_translation
	# child of utterance, 1-to-1 relation
	utttr_id <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='utterance_translation']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_ID")
	utttr_ref <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='utterance_translation']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_REF")
	utttr_val <- xml2::xml_text(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='utterance_translation']/ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE"))

	tab_utttr <- data.table(utttr_id, utttr_ref, utttr_val)


	# comments
	# child of utterance, 1-to-1 relation
	comnt_id <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='add_comments']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_ID")
	comnt_ref <- xml2::xml_attr(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='add_comments']/ANNOTATION/REF_ANNOTATION"), "ANNOTATION_REF")
	comnt_val <- xml2::xml_text(xml2::xml_find_all(raweaf, "//TIER[@TIER_ID='add_comments']/ANNOTATION/REF_ANNOTATION/ANNOTATION_VALUE"))

	tab_comnt <- data.table(comnt_id, comnt_ref, comnt_val)


	# set a key for preserving order of annotations
	tab_gwords[, key := seq(0, nrow(tab_gwords) - 1, 1)]

	# merge tables by shared IDs and refs
	tiers <- merge(tab_gwords, tab_gloss, by.x = "gwords_id", by.y = "gloss_ref", all = TRUE)
	tiers <- merge(tiers, tab_graid, by.x = "gloss_id", by.y = "graid_ref", all = TRUE)
	tiers <- merge(tiers, tab_refind, by.x = "graid_id", by.y = "refind_ref", all = TRUE)
	tiers <- merge(tiers, tab_reflex, by.x = "refind_id", by.y = "reflex_ref", all = TRUE)
	tiers <- merge(tiers, tab_utter, by.x = "gwords_ref", by.y = "utter_id", all = TRUE)
	tiers <- merge(tiers, tab_uttid, by.x = "utter_ref", by.y = "uttid_id", all = TRUE)
	tiers <- merge(tiers, tab_utttr, by.x = "gwords_ref", by.y = "utttr_ref", all = TRUE)
	tiers <- merge(tiers, tab_comnt, by.x = "gwords_ref", by.y = "comnt_ref", all = TRUE)
	tiers <- merge(tiers, tab_timeslot, by.x = "uttid_tsbegin", by.y = "timeslot_id", all = FALSE)
	tiers <- merge(tiers, tab_timeslot, by.x = "uttid_tsend", by.y = "timeslot_id", all = FALSE)


	# !!!!!!!!!!
	# MAKE SURE ROWS WITHOUT GWORDS ARE PLACED PROPERLY
	# this should be taken care of in the EAF files instead!
	setorder(tiers, by = "gwords_ref")
	tiers[, xkey := shift(key, n = 1, type = "lag")]
	tiers[is.na(key), key := xkey + 0.5]
	# !!!!!!!!!!


	# sort rows by key
	setorder(tiers, by = "key")


	# add EAF metadata
	tiers[1, meta := xml2::xml_attr(xml2::xml_find_first(raweaf, "//TIER"), "ANNOTATOR")]
	tiers[2, meta := xml2::xml_attr(xml2::xml_find_first(raweaf, "//TIER"), "PARTICIPANT")]
	tiers[3, meta := gsub("^.*/", "", xml2::xml_attr(xml2::xml_find_first(raweaf, "//MEDIA_DESCRIPTOR"), "MEDIA_URL"))]
	tiers[4, meta := gsub("T.*$", "", xml2::xml_attr(xml2::xml_find_first(raweaf, "//ANNOTATION_DOCUMENT"), "DATE"))]
	tiers[5, meta := stringi::stri_split_fixed(utils::tail(stringi::stri_split_fixed(eaffile, "/", n = -1)[[1]], n = 1), ".", n = -1)[[1]][1]]

	if (is.na(tiers[3, meta])) {
		tiers[3, meta := "NA"]
	}

	# !!!!!!!!!!
	# REMOVE SPURIOUS LINEBREAKS AND TABSTOPS
	# these should be taken care of in the EAF files!
	tiers[, (names(tiers)) := lapply(.SD, function(x) gsub("\\n|\\t", "", x)), .SDcols = names(tiers)]
	# !!!!!!!!!!


	# return table
	return(tiers)
}

# stop RMD CHECK from complaining about unbound global variables
if (getRversion() >= "2.15.1") {
	utils::globalVariables(c("meta"))
}

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------

#' Read external file metadata
#'
#' Reads extra metadata not included in the EAF files from an external source.
#' To be used by the EAF-to-XML converter. Not implemented yet.
#'
#' @param metadata A file.
#'
#' @return Nothing yet.
#'
#' @keywords internal
mc_meta_eaf <- function(metadata = NULL) {
	return(NULL)
}

# ----------------------------------------------------------------------
