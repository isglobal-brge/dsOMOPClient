# Administrator-side loader for the two public synthetic practice resources.
# No patient rows are returned by the analyst examples.
load_book_practice <- function(directory) {
  dir.create(directory,recursive=TRUE,showWarnings=FALSE)
  revision <- "3d349dcdd79cc346b8e36eb837e49feffe7f7497"
  specs <- list(gibleed=list(dataset="GiBleed",version="5.3",sha="081b82e84c90e2904e02c26546e46412b01cce566071c77e0c4a355c2d0612f9"),
    numeric=list(dataset="Synthea27Nj",version="5.4",
      sha="cdd3971be325e17b0e872bf4560711ab4c346917ecb079e2faf32badb55cb693"))
  manifest <- list()
  for (name in names(specs)) {
    spec <- specs[[name]]
    filename <- paste0(spec$dataset,"_",spec$version,".zip")
    url <- paste0("https://raw.githubusercontent.com/OHDSI/EunomiaDatasets/",revision,
      "/datasets/",spec$dataset,"/",filename)
    archive <- file.path(directory,filename)
    if (!file.exists(archive)) download.file(url,archive,mode="wb",quiet=TRUE)
    hash <- local({stream<-file(archive,"rb");on.exit(close(stream));paste(openssl::sha256(stream))})
    if (!is.null(spec$sha) && hash!=spec$sha) stop("Archive checksum mismatch: ",name)
    database <- file.path(directory,paste0(name,".sqlite"))
    if (name == "numeric") {
      # Use the numeric workstream's checksum-verified loader unchanged, then
      # adapt its typed DuckDB tables to the book's portable SQLite resources.
      source("tools/book/load-numeric.R", local=TRUE)
      stage <- tempfile("book-numeric-loader-", tmpdir=directory)
      dir.create(stage)
      stopifnot(file.copy(archive, file.path(stage, filename)))
      withr::with_envvar(c(NUMERIC_POSTGRES="0", NUMERIC_DUCKDB="1"),
        load_numeric_cdm(stage))
      input <- DBI::dbConnect(duckdb::duckdb(), file.path(stage,"numeric_cdm.duckdb"), read_only=TRUE)
      output <- DBI::dbConnect(RSQLite::SQLite(), database)
      counts <- list()
      tryCatch({
        for (table in DBI::dbListTables(input)) {
          d <- DBI::dbReadTable(input, table)
          for (col in names(d)) {
            if (inherits(d[[col]], "Date") || inherits(d[[col]], "POSIXt")) d[[col]] <- as.character(d[[col]])
          }
          DBI::dbWriteTable(output, table, d, overwrite=TRUE)
          stopifnot(DBI::dbGetQuery(output,paste0("SELECT COUNT(*) AS n FROM ",
            DBI::dbQuoteIdentifier(output,table)))$n == nrow(d))
          counts[[table]] <- nrow(d)
        }
        m <- DBI::dbGetQuery(output, paste("SELECT COUNT(*) AS rows,",
          "SUM(value_as_number IS NOT NULL) AS numeric_values,",
          "SUM(value_as_number IS NOT NULL AND unit_concept_id != 0) AS numeric_with_unit FROM measurement"))
        manifest[[name]] <- list(dataset=spec$dataset, version=spec$version, revision=revision,
          url=url, sha256=hash, archive_bytes=unname(file.info(archive)$size), tables=counts,
          measurements=as.list(m[1,]), database=basename(database),
          resource_url=paste0("omop+dbi:sqlite://",normalizePath(database)),format="omop.dbi.db")
      }, finally={DBI::dbDisconnect(input, shutdown=TRUE); DBI::dbDisconnect(output); unlink(stage,recursive=TRUE)})
      next
    }
    csvdir <- tempfile("book-csv-",tmpdir=directory);dir.create(csvdir)
    unzip(archive,exdir=csvdir)
    db <- DBI::dbConnect(RSQLite::SQLite(),database)
    counts <- list()
    tryCatch({
      DBI::dbBegin(db)
      for (path in sort(list.files(csvdir,"[.]csv$",full.names=TRUE,recursive=TRUE,ignore.case=TRUE))) {
        table <- tolower(tools::file_path_sans_ext(basename(path)))
        d <- read.csv(path,colClasses="character",na.strings=c("","NA"),check.names=FALSE)
        names(d)<-tolower(names(d))
        text_ids<-c("vocabulary_id","domain_id","concept_class_id","relationship_id",
          "source_vocabulary_id","target_vocabulary_id","reverse_relationship_id","unique_device_id")
        for (col in names(d)) {
          if ((grepl("_id$",col) && !col %in% text_ids) || col %in% c(
            "year_of_birth","month_of_birth","day_of_birth","days_supply","refills",
            "min_levels_of_separation","max_levels_of_separation")) {
            x<-suppressWarnings(as.numeric(d[[col]]))
            if (any(is.na(x)&!is.na(d[[col]]))) stop("Invalid numeric ID in ",table,".",col)
            d[[col]]<-x
          } else if (col %in% c("value_as_number","range_low","range_high","quantity",
            "numerator_value","denominator_value","amount_value")) d[[col]]<-as.numeric(d[[col]])
          # SQLite stores calendar dates as ISO text, including header-only tables.
        }
        DBI::dbWriteTable(db,table,d,overwrite=TRUE)
        counts[[table]]<-nrow(d)
      }
      DBI::dbCommit(db)
      m<-DBI::dbGetQuery(db,paste("SELECT COUNT(*) AS rows,",
        "SUM(CASE WHEN value_as_number IS NOT NULL THEN 1 ELSE 0 END) AS numeric_values,",
        "SUM(CASE WHEN value_as_number IS NOT NULL AND unit_concept_id != 0 THEN 1 ELSE 0 END) AS numeric_with_unit",
        "FROM measurement"))
      manifest[[name]]<-list(dataset=spec$dataset,version=spec$version,revision=revision,
        url=url,sha256=hash,archive_bytes=unname(file.info(archive)$size),tables=counts,
        measurements=as.list(m[1,]),database=basename(database),
        resource_url=paste0("omop+dbi:sqlite://",normalizePath(database)),format="omop.dbi.db")
    },finally={DBI::dbDisconnect(db);unlink(csvdir,recursive=TRUE)})
  }
  jsonlite::write_json(manifest,file.path(directory,"manifest.json"),auto_unbox=TRUE,pretty=TRUE)
  print(lapply(manifest,function(x)x[c("dataset","sha256","measurements")]))
  invisible(manifest)
}
if (sys.nframe()==0L) {
  args<-commandArgs(TRUE)
  if(length(args)!=1L) stop("Usage: Rscript tools/book/load-practice.R DIRECTORY")
  load_book_practice(args[[1L]])
}
