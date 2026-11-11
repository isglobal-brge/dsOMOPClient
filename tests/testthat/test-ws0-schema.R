# WS-0 (design-2.10.3 section 12.1): the vendored schema artefacts are
# byte-identical to the server's and validate the same way.

.ws0_schema_dir <- function() {
  d <- system.file("schema", package = "dsOMOPClient")
  if (!nzchar(d)) d <- file.path("inst", "schema")
  d
}
.ws0_sha256_file <- function(path) {
  paste(as.character(openssl::sha256(readBin(path, "raw", file.info(path)$size))))
}

test_that("vendored schema files match SHA256SUMS", {
  d <- .ws0_schema_dir()
  sums <- read.table(file.path(d, "SHA256SUMS"), header = FALSE,
                     col.names = c("sha256", "path"), stringsAsFactors = FALSE)
  expect_true(all(c("plan-v2.json", "mutate-v1.json", "digest-vectors.json",
                    "examples/plan-v2-example-A.json", "probes/plan-v2-probes.json") %in% sums$path))
  expect_false("ir-v2.json" %in% sums$path)
  for (i in seq_len(nrow(sums))) {
    expect_identical(.ws0_sha256_file(file.path(d, sums$path[i])), sums$sha256[i], info = sums$path[i])
  }
  server <- system.file("schema", "SHA256SUMS", package = "dsOMOP")
  if (nzchar(server)) {
    ssums <- read.table(server, header = FALSE, col.names = c("sha256", "path"), stringsAsFactors = FALSE)
    m <- merge(sums, ssums, by = "path")
    expect_identical(nrow(m), nrow(sums))
    expect_identical(m$sha256.x, m$sha256.y)
  }
})

test_that("examples and probes behave identically on the client validator", {
  d <- .ws0_schema_dir()
  plan <- dsOMOPClient:::.jsonSchemaPackaged("plan-v2", package = "dsOMOPClient")
  mut <- dsOMOPClient:::.jsonSchemaPackaged("mutate-v1", package = "dsOMOPClient")
  for (ex in c("plan-v2-example-A.json", "plan-v2-example-B.json")) {
    inst <- dsOMOPClient:::.jsonSchemaLoad(file.path(d, "examples", ex))
    expect_true(dsOMOPClient:::.jsonSchemaValidate(plan, inst)$valid, info = ex)
  }
  probes <- c(dsOMOPClient:::.jsonSchemaLoad(file.path(d, "probes", "plan-v2-probes.json")),
              dsOMOPClient:::.jsonSchemaLoad(file.path(d, "probes", "mutate-v1-probes.json")))
  schemas <- list(`plan-v2` = plan, `mutate-v1` = mut)
  n <- 0L
  for (p in probes) {
    if (!p$schema %in% names(schemas)) next   # ir-v2 probes are server-only
    res <- dsOMOPClient:::.jsonSchemaValidate(schemas[[p$schema]], p$instance)
    expect_identical(res$valid, identical(p$expect, "valid"), info = paste(p$id, p$label))
    n <- n + 1L
  }
  expect_gte(n, 47L)
})

test_that("the digest vectors reproduce on the client", {
  d <- .ws0_schema_dir()
  vec <- jsonlite::fromJSON(file.path(d, "digest-vectors.json"), simplifyVector = FALSE)
  for (v in vec$vectors) {
    got <- dsOMOPClient:::.planSemanticDigest(v$instance)
    expect_identical(got$payload, v$payload, info = v$label)
    expect_identical(got$sha256, v$payload_sha256, info = v$label)
  }
})
