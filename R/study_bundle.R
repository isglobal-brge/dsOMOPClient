# Local public study artifacts; no connection/session lookup is used here.
.bundleReject <- function(path,remedy) stop("study_bundle_invalid: ",path,"; ",remedy,call.=FALSE)
.bundleObject <- function(x,fields,required=character(),path="/study") {
  if(!is.list(x)||is.object(x)||is.null(names(x))||anyNA(names(x))||anyDuplicated(names(x))||
    any(!nzchar(names(x)))||length(setdiff(names(x),fields))||length(setdiff(required,names(x))))
    .bundleReject(path,"use a uniquely named declarative list with documented fields.")
}
.bundleText <- function(x,path) {
  if(!is.character(x)||length(x)!=1L||is.na(x)||!nzchar(x)||nchar(x,type="bytes")>2048L||!validUTF8(x)||
    grepl("[[:cntrl:]]",x)) .bundleReject(path,"supply one bounded public UTF-8 identifier.")
  if(grepl("[a-z][a-z0-9+.-]*://[^/]*@|[?&](password|token|secret|key|credential)=",x,ignore.case=TRUE))
    .bundleReject(path,"remove credentials and secret query parameters from public provenance.")
  x
}
.bundlePublicMap <- function(x,path) {
  if(is.null(x))return(list(status="unresolved"))
  .bundleObject(x,names(x),path=path)
  forbidden<-c("password","token","secret","credentials","connection","connection_string","dp_key","pseudonym_key","patient_rows","selected_dates","noise_fingerprint")
  if(any(tolower(names(x)) %in% forbidden)) .bundleReject(path,"include only public reference versions or origins.")
  lapply(x,function(v).bundleText(v,path))
}
.bundleStudy <- function(study,plan) {
  .bundleObject(study,c("id","protocol_version","roles","protocol_uri","phenotype_origins","reference_versions","source_snapshot_id"),c("id","protocol_version","roles"))
  for(k in intersect(names(study),c("id","protocol_version","protocol_uri")))study[[k]]<-.bundleText(study[[k]],paste0("/study/",k))
  .bundleObject(study$roles,names(study$roles),path="/study/roles")
  if(!length(study$roles)) .bundleReject("/study/roles","declare at least one population/output role.")
  study$roles<-lapply(study$roles,function(role) {
    .bundleObject(role,c("population","output","cohort_definition_id","outcome_kind"),path="/study/roles")
    refs<-intersect(names(role),c("population","output"))
    if(length(refs)!=1L) .bundleReject("/study/roles","name exactly one declared population or output reference per role.")
    ref<-.bundleText(role[[refs]],"/study/roles")
    if(!ref %in% names(plan[[if(refs=="population")"populations" else "outputs"]]))
      .bundleReject("/study/roles","use a reference declared by the bound plan.")
    if(!is.null(role$outcome_kind)&&(!is.character(role$outcome_kind)||length(role$outcome_kind)!=1L||is.na(role$outcome_kind)||!role$outcome_kind %in% c("concept-filter","cohort")))
      .bundleReject("/study/roles/outcome_kind","declare concept-filter or cohort; integer IDs do not identify a definition kind.")
    if(!is.null(role$cohort_definition_id)) {
      id<-role$cohort_definition_id
      if(is.numeric(id)&&!is.object(id)&&length(id)==1L&&!is.na(id)&&is.finite(id)&&id==trunc(id)&&id>=0&&id<=9007199254740991)id<-format(id,scientific=FALSE,trim=TRUE,digits=22)
      if(!is.character(id)||length(id)!=1L||is.na(id)||!grepl("^(0|[1-9][0-9]{0,18})$",id)||
        nchar(id)==19L&&id>"9223372036854775807") .bundleReject("/study/roles/cohort_definition_id","use exact nonnegative signed-64 decimal text; unsafe binary64 IDs refuse.")
      role$cohort_definition_id<-id
    }
    role
  })
  for(k in c("phenotype_origins","reference_versions"))if(!is.null(study[[k]]))study[[k]]<-.bundlePublicMap(study[[k]],paste0("/study/",k))
  if(!is.null(study$source_snapshot_id)) {
    .bundleObject(study$source_snapshot_id,c("id","provenance"),c("id","provenance"),"/study/source_snapshot_id")
    study$source_snapshot_id<-lapply(study$source_snapshot_id,function(x).bundleText(x,"/study/source_snapshot_id"))
  }
  study
}
.bundleOrigins <- function(recipe) {
  lapply(Filter(function(p)!is.null(p$circe_provenance),recipe$populations),function(p) {
    p<-.circe_refresh_provenance(p)$circe_provenance
    # Explicit fields only; original JSON remains in the separately saved
    # public phenotype. No arbitrary handle or source metadata is traversed.
    p[intersect(names(p),c("original_sha256","original_json_sha256","origin_form","origin","adapter_version","supported_subset_version","original_semantic_digest","original_recipe_digest","recipe_digest","derivative"))]
  })
}
#' Save a deterministic public study bundle without contacting sites
#' @param x A bound recipe or execution result with admitted codebook metadata.
#' @param study Named declaration with id, protocol_version and roles. Each role
#'   names one population or output; optional cohort_definition_id is exact
#'   decimal text, and outcome_kind is concept-filter or cohort. Optional fields
#'   are protocol_uri, phenotype_origins, reference_versions, source_snapshot_id.
#'   The latter is a named list with id and public provenance.
#' @param file Explicit client JSON path, or NULL to return the bundle.
#' @return A study-bundle-1 list, containing the exact executable bound plan,
#'   digest, codebooks, public provenance and known/unknown snapshot identity.
#' @details No connections, network access, automatic snapshot or execution.
#' The executable bound recipe is stored in plan-v2 format. Published bundles
#' contain only retained codebook components; absent metadata stays unresolved.
#' A bundle does not certify equal data across runs or a complete Strategus study.
#' @export
recipe_study_bundle <- function(x,study,file=NULL) {
  receipt<-attr(x,"omop_codebook")
  prospective<-inherits(x,"omop_recipe")
  if(!prospective&&(is.null(receipt$plan)||is.null(receipt$components)))
    .bundleReject("/x","supply a bound recipe or result with admitted codebook metadata.")
  plan<-if(prospective)recipe_to_plan(x,version=2) else receipt$plan
  .v2_assert_schema(plan)
  study<-.bundleStudy(study,plan)
  books<-.bundleCodebooks(x,plan)
  library<-if(prospective)lapply(x$meta$recipe_library,function(e)e[intersect(names(e),c("id","template_version","origin_template_version","definition_checksum","documentation_revision","package_version","derivative_policy"))]) else receipt$study_provenance$library %||% list(status="unresolved")
  out<-list(schema_version="study-bundle-1",study=study,
    bound_recipe=list(format="plan-v2",plan=.pnv2Plain(plan)),semantic_digest=.planNormalizedDigest(plan)$sha256,
    library=library,formulas=if(prospective).formulaInventory(x) else receipt$formulas,
    phenotype_origins=if(prospective).bundleOrigins(x) else receipt$study_provenance$phenotype_origins %||% list(status="unresolved"),
    codebooks=books,versions=list(client=as.character(utils::packageVersion("dsOMOPClient")),server="unresolved",
      required_capabilities=as.list(.plan_v2_operations(plan)),cdm="unresolved",vocabulary="unresolved",references=study$reference_versions %||% list(status="unresolved")),
    publication=list(status=if(prospective)"prospective" else "published",components=as.list(names(books)),sites=if(prospective)"unresolved" else .bundlePublicationSites(receipt$publication_sites,names(books))),
    snapshot=if(is.null(study$source_snapshot_id))list(status="unknown") else c(list(status="known"),study$source_snapshot_id))
  # Reconstruct a plain public artifact, dropping all R attributes/environments.
  out<-.categoryMetadataPlain(out)
  if(!is.null(file)) {
    if(!is.character(file)||length(file)!=1L||is.na(file)||!nzchar(file)) .bundleReject("/file","name an explicit client file path.")
    writeLines(enc2utf8(.numberJson(out)),file,useBytes=TRUE)
  }
  out
}

.bundleRecipeProvenance <- function(recipe)list(
  library=lapply(recipe$meta$recipe_library,function(e)e[intersect(names(e),c("id","template_version","origin_template_version","definition_checksum","documentation_revision","package_version","derivative_policy"))]),
  phenotype_origins=.bundleOrigins(recipe))

# Reconstruct declaration-bearing fields from the bound public plan. A receipt
# can supply admitted types/levels, never arbitrary nested handle metadata.
.bundleCodebooks <- function(x,plan) {
  books<-recipe_codebook(x)
  declarations<-.codebookPlanSchemas(plan)
  for(key in names(books)) {
    columns<-books[[key]]$columns
    declared<-declarations[[key]]$schema$columns
    keys<-vapply(declared,function(r)r$stable_key,"")
    for(i in seq_len(nrow(columns))) {
      at<-match(columns$stable_key[i],keys)
      row<-if(is.na(at))list() else declared[[at]]
      columns$provenance[[i]]<-row$provenance %||% list(source="unresolved")
      columns$derivation[[i]]<-row$derivation %||% list(status="unresolved")
      levels<-columns$levels[[i]]
      if(is.list(levels))columns$levels[[i]]<-levels[intersect(names(levels),c("keys","labels","completeness"))]
    }
    books[[key]]$columns<-columns
  }
  books
}

.bundlePublicationSites <- function(sites,allowed) {
  if(is.null(sites))return("unresolved")
  lapply(sites,function(s) {
    status<-s$status
    if(!is.character(status)||length(status)!=1L||is.na(status)||!status %in% c("ok","partial","refused","error","skipped","unavailable","unresolved"))status<-"unresolved"
    list(status=status,components=as.list(intersect(as.character(s$components %||% character()),allowed)))
  })
}
