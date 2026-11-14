#' Describe output tables without querying patient data
#' @param x A bound recipe or execution result carrying release metadata.
#' @param output Optional exact output name.
#' @param component Optional exact component name.
#' @return An ordered named list of codebook-1 records, with a columns data frame.
#' @details This is a pure client operation. Prospective records explicitly mark
#' unresolved schemas; published records use only admitted release metadata.
#' @export
recipe_codebook <- function(x,output=NULL,component=NULL) {
  receipt<-attr(x,"omop_codebook")
  if(inherits(x,"omop_recipe")) {
    plan<-recipe_to_plan(x,version=2);status<-"prospective";schemas<-list()
    for(o in names(plan$outputs)) {
      node<-plan$outputs[[o]]
      # Dynamic generated names and nested schemas remain unresolved until an
      # admitted release; this does not invent a patient table for a matrix.
      components<-switch(node$format,sparse=c(o,"covariateRef"),temporal_covariates=c(o,"covariateRef","timeRef"),o)
      for(cn in components) {
        vars<-unlist(node$columns %||% if(node$format=="wide")names(plan$variables) else character(),use.names=FALSE)
        cols<-lapply(vars,function(n) {
          v<-plan$variables[[n]]
          list(stable_key=n,name=n,meaning=paste("Declared variable",n),semantic_type="unresolved",r_type="unresolved",storage_type="unresolved",unit="unresolved",
            derivation=list(variable=v),privacy_class=NA_integer_,aggregate_policy="unresolved",missing_policy="Declared policies; unavailable remains NA",provenance=list(source="public plan"),completeness="unresolved")
        })
        schemas[[paste(o,cn,sep="/")]]<-list(schema=list(columns=cols,row_keys=list()),categories=list(),status=status)
      }
    }
  } else {
    if(is.null(receipt)||is.null(receipt$plan)||is.null(receipt$components)) .categoryReject("This handle has no admitted codebook metadata; supply its bound recipe for a prospective codebook.","codebook_metadata_missing")
    plan<-receipt$plan;schemas<-receipt$components;status<-"published"
  }
  if(!is.null(output)&&(!is.character(output)||length(output)!=1L||!output %in% names(plan$outputs))) .categoryReject("Unknown exact codebook output.","codebook_metadata_missing")
  available<-vapply(strsplit(names(schemas),"/",fixed=TRUE),tail,"",1L)
  if(!is.null(component)&&(!is.character(component)||length(component)!=1L||!component %in% available)) .categoryReject("Unknown exact codebook component.","codebook_metadata_missing")
  digest<-.planNormalizedDigest(plan)$sha256
  result<-list()
  for(key in names(schemas)) {
    pair<-strsplit(key,"/",fixed=TRUE)[[1L]]
    if(!is.null(output)&&pair[1L]!=output||!is.null(component)&&pair[2L]!=component)next
    entry<-schemas[[key]];rows<-entry$schema$columns
    scalar<-c("stable_key","name","meaning","semantic_type","r_type","storage_type","unit","privacy_class","aggregate_policy","missing_policy","completeness")
    columns<-as.data.frame(setNames(lapply(scalar,function(field)vapply(rows,function(r)as.character(r[[field]] %||% NA_character_),"")),scalar),stringsAsFactors=FALSE)
    for(field in c("levels","aliases","ordered","reference","contrasts","derivation","provenance"))columns[[field]]<-I(lapply(rows,function(r) {
      category<-entry$categories[[r$stable_key]]
      if(field=="levels") {
        if(is.null(category))return(NULL)
        return(list(keys=category$keys %||% category$ids,labels=category$levels,completeness=if(status=="published")"reconciled" else "declared"))
      }
      category[[field]] %||% r[[field]]
    }))
    result[[key]]<-list(schema_version="codebook-1",plan_digest=digest,output=pair[1L],component=pair[2L],status=status,
      row_grain=plan$outputs[[pair[1L]]]$grain %||% "unresolved",row_keys=entry$schema$row_keys,
      release_contract_digest=if(status=="published")paste0(as.character(openssl::sha256(charToRaw(.jcsCanonical(entry))))),columns=columns)
  }
  result
}
