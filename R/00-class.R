OMOPCDMDatabase <- R6::R6Class(
  "OMOPCDMDatabase",
  public = list(
    connections = NULL,
    resource = NULL,
    resourceSymbol = NULL,
    initialize = function(connections, resource) {
      self$connections = connections
      self$resource = resource
      self$resourceSymbol = generateResourceSymbol(resource)
    }
  )
)

#' @export
ds.omop <- function(connections, resource) {
  return(OMOPCDMDatabase$new(connections, resource))
}
