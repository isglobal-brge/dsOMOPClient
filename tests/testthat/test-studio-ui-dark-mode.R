# ==============================================================================
# Test: Studio UI Dark Mode
# Verifies dark mode CSS overrides are present
# ==============================================================================

test_that("CSS contains dark mode overrides for all major elements", {
  css <- .studio_css()

  # Core dark mode selectors
  expect_true(grepl("\\[data-bs-theme='dark'\\]", css))

  # Navbar
  expect_true(grepl("\\[data-bs-theme='dark'\\] .navbar", css))
  # Card borders
  expect_true(grepl("\\[data-bs-theme='dark'\\] .card", css))
  # Code output
  expect_true(grepl("\\[data-bs-theme='dark'\\] .code-output", css))
  # DT table headers
  expect_true(grepl("\\[data-bs-theme='dark'\\] table.dataTable thead", css))
  # DT selected rows
  expect_true(grepl("\\[data-bs-theme='dark'\\] table.dataTable tbody tr.selected", css))
  # Accordion
  expect_true(grepl("\\[data-bs-theme='dark'\\] .accordion-button", css))
  # Nav pills
  expect_true(grepl("\\[data-bs-theme='dark'\\] .nav-pills", css))
  # Empty states
  expect_true(grepl("\\[data-bs-theme='dark'\\] .empty-state", css))
  # Breadcrumbs
  expect_true(grepl("\\[data-bs-theme='dark'\\] .breadcrumb-nav", css))
})

test_that("CSS contains animations keyframe", {
  css <- .studio_css()
  expect_true(grepl("@keyframes fadeIn", css))
  expect_true(grepl("\\.fade-in", css))
})

test_that("CSS contains empty-state class", {
  css <- .studio_css()
  expect_true(grepl("\\.empty-state", css))
})

test_that("CSS contains card hover elevation", {
  css <- .studio_css()
  expect_true(grepl("\\.card:hover", css))
  expect_true(grepl("box-shadow", css))
})

test_that("CSS contains value box hover lift", {
  css <- .studio_css()
  expect_true(grepl("bslib-value-box.*:hover", css))
  expect_true(grepl("translateY\\(-2px\\)", css))
})

test_that("CSS contains DT table styling", {
  css <- .studio_css()
  expect_true(grepl("dataTable thead th", css))
  expect_true(grepl("text-transform: uppercase", css))
})

test_that("CSS contains navbar gradient", {
  css <- .studio_css()
  expect_true(grepl("linear-gradient.*1e293b.*0f172a", css))
})
