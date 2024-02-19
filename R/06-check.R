#' @title Check Database Connection
#' @name OMOPCDMDatabase-checkConnection
#'
#' @description This function checks if the OMOPCDMDatabase object points to a valid connection to the OMOP CDM database
#' by attempting to retrieve its list of tables.
#'
#' @return A boolean value indicating whether the connection to the database is valid (TRUE) or not (FALSE).
#'
OMOPCDMDatabase$set("public", "checkConnection", function() {
  tables <- self$tables()
  return(!is.null(tables))
})
