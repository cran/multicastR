## *multicastR* news

#### 1.3.0 [released 19-08-30]
*  the `reflex` column in *multicastR* tables has been renamed to `isnref`
*  function `mc_referents` now takes an optional argument `vkey` to select
   specific versions of the lists of referents
*  function `mc_metadata` now takes an optional argument `vkey` to select
   specific versions of the metadata
*  updated repository links
*  updated documentation
*  removed the `legacy.colnames` argument from function `multicast`
*  removed function `mcindex` (use `mc_index` instead)
*  removed function `mc_count_clauses` (use `mc_clauses` instead)

#### 1.2.0 [released 19-07-30]
*  new function `mc_metadata`: download a table containing all corpus metadata
   from the repository
*  new function `mc_referents`: download a bundled list of all discourse referents
   (as annotated with the RefIND scheme, Schiborr et al. 2018) from the repository
*  new function `mc_clauses`: replaces function `mc_count_clauses`
*  function `mc_count_clauses` is now deprecated
*  fixed an issue with `mc_clauses` (and `mc_count_clauses`) when `bytext = TRUE`

#### 1.1.0 [released 19-05-12]
*  the `file` column in *multicastR* tables has been renamed to `text`
*  the `word` column in *multicastR* tables has been renamed to `gword`
*  for backwards compatibility, the `multicast` function now has a
   `legacy.colnames` argument that, if set to `TRUE`, outputs tables with the
   old column names; this option will be removed in the future
*  new function `mc_count_clauses` (WIP): produces a table containing the number of
   clause units in each corpus or text
*  new function `mc_table` (WIP): tabulate counts of GRAID symbols
*  new function `mc_index`: replaces function `mcindex`
*  function `mcindex` is now deprecated
*  updated repository links
*  repository files now have the file extension TSV (rather than TXT)
*  updated description
*  updated citation

#### 1.0.1 [released 18-09-29]
*  updated repository links

#### 1.0.0 [released 18-06-13]
*  initial publication
