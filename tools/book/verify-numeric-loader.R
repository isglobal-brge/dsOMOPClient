# Administrator-only parity check against the pinned public CSV source.
args <- commandArgs(TRUE)
stopifnot(length(args)==1L)
directory <- normalizePath(args[[1L]])
local({
  manifest <- jsonlite::read_json(file.path(directory,"manifest.json"))$numeric
  stopifnot(manifest$sha256 == "cdd3971be325e17b0e872bf4560711ab4c346917ecb079e2faf32badb55cb693")
  csvdir <- tempfile("book3-source-")
  dir.create(csvdir)
  on.exit(unlink(csvdir,recursive=TRUE),add=TRUE)
  unzip(file.path(directory,"Synthea27Nj_5.4.zip"),exdir=csvdir)
  db <- DBI::dbConnect(RSQLite::SQLite(),file.path(directory,"numeric.sqlite"))
  on.exit(DBI::dbDisconnect(db),add=TRUE)
  paths <- list.files(csvdir,"[.]csv$",full.names=TRUE,recursive=TRUE,ignore.case=TRUE)
  stopifnot(length(paths)==39L)
  for (path in paths) {
    table <- tolower(tools::file_path_sans_ext(basename(path)))
    source <- read.csv(path,colClasses="character",na.strings=c("","NA"),check.names=FALSE)
    names(source) <- tolower(names(source))
    actual <- DBI::dbReadTable(db,table)
    stopifnot(nrow(source)==nrow(actual),identical(names(source),names(actual)),
      nrow(actual)==manifest$tables[[table]])
    # Check every scalar, with numeric comparison only for typed numeric fields.
    for (col in names(source)) {
      expected <- source[[col]]
      if (is.numeric(actual[[col]])) expected <- as.numeric(expected)
      stopifnot(isTRUE(all.equal(actual[[col]],expected,check.attributes=FALSE)))
    }
  }
  m <- DBI::dbReadTable(db,"measurement")
  stopifnot(is.numeric(m$value_as_number), sum(!is.na(m$value_as_number))==9107L,
    sum(!is.na(m$value_as_number) & m$unit_concept_id!=0)==8403L,
    all(is.na(m$range_high)),all(is.na(m$range_low)))
  cat("PASS: pinned loader -> DuckDB -> SQLite; all 39 tables, every scalar and manifest count; 9,107 numeric values, 8,403 with units\n")
})
