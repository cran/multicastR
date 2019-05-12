## multicastR news

##### 1.1.0 [released 19-05-12]
*  the `file` column in *multicastR* tables has been renamed to `text`
*  the `word` column in *multicastR* tables has been renamed to `gword`
*  for backwards compatibility, the `multicast` function now has a
   `legacy.colnames` argument that, if set to `TRUE`, outputs tables with the
   old column names; this option will be removed in the future
*  new function `mc_count_clauses` (WIP): produces a table with the number of clause units
   in each corpus or text
*  new function `mc_table` (WIP): tabulate counts of GRAID symbols
*  new function `mc_eaf_to_xml` (WIP): convert EAF files (with a particular structure)
   into XML files
*  new function `mc_eaf_to_tsv`(WIP): convert EAF files (with a particular structure)
   into TSV files
*  note that these functions have only been tested with the actual Multi-CAST data; there
   is no guarantee they will work with data that do not meet its exact specifications!
*  new function `mc_index`: replaces function `mcindex`
*  function `mcindex` is now deprecated
*  updated repository links
*  repository files now have the file extension TSV (rather than TXT)
*  updated description
*  updated citation

##### 1.0.1 [released 18-09-29]
*  updated repository links

##### 1.0.0 [released 18-06-13]
*  initial publication
