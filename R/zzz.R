# ----------------------------------------------------------------------

.onAttach <- function(libname, pkgname) {
	packageStartupMessage(paste0("----------\n",
								 "multicastR (v", packageVersion("multicastR"), ")\n",
								 "All corpus data are published under a CC-BY 4.0 licence.\n",
								 "Visit the Multi-CAST website for more information.\n",
								 "<multicast.aspra.uni-bamberg.de/>\n",
								 "----------"))
 }

# ----------------------------------------------------------------------
