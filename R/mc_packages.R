# ----------------------------------------------------------------------

#' Load requisite packages for internal functions
#'
#' @return Nothing.
#'
#' @keywords internal
# mc_packages <- function() {
#
# 	# xml2 (>= 1.1.0),
# 	# XML (>= 3.98.0),
# 	# xtable (>= 1.8.0),
# 	# gsubfn (>= 0.7)
#
# 	load <- c("xml2", "XML", "xtable", "gsubfn")
# 	pkgs <- rownames(installed.packages())
#
# 	for (i in 1:length(load)) {
# 		if (load[i] %in% pkgs == FALSE) {
# 			install.packages(load[i])
# 		}
# 	}
#
# 	library(xml2)
# 	library(XML)
# 	library(xtable)
# 	library(gsubfn)
# }
