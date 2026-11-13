test_that("nearly disconnected transport retains both displayed margins", {
  # Finite IPF followed by row scaling can leave a 1e-4 column error on
  # this table; Dykstra also reaches its iteration limit. The deterministic
  # feasible fallback must retain both parents without claiming an L2 optimum.
  X <- diag(rep(1e4, 2L))
  rows <- c(1, 1)
  cols <- c(1.0001, .9999)
  for (iterations in c(1L, 20000L)) {
    out <- .dp_transport(X, rows, cols, max_iter = iterations)
    expect_true(all(is.finite(out) & out >= 0))
    expect_equal(rowSums(out), rows, tolerance = 1e-9)
    expect_equal(colSums(out), cols, tolerance = 1e-9)
    expect_true(all(out <= outer(rows, cols, pmin) + 1e-9))
    expect_equal(.dp_transport(X, rows, cols, max_iter = iterations), out,
                 tolerance = 0)
  }
  # Removing zero-margin rows/columns must preserve exact zero support when
  # the remaining table requires the same finite-iteration fallback.
  padded <- matrix(1e4, 3L, 3L)
  padded[1:2, 1:2] <- X
  out <- .dp_transport(padded, c(rows, 0), c(cols, 0), max_iter = 1L)
  expect_equal(rowSums(out), c(rows, 0), tolerance = 1e-9)
  expect_equal(colSums(out), c(cols, 0), tolerance = 1e-9)
  expect_true(all(out >= 0))
  expect_identical(as.numeric(out[3L, ]), rep(0, 3L))
  expect_identical(as.numeric(out[, 3L]), rep(0, 3L))
})

test_that("transport fallback matches the server's versioned display", {
  skip_if_not_installed("dsOMOP")
  server <- get(".dsomopReleaseTransport", asNamespace("dsOMOP"))
  X <- diag(rep(1e4, 2L))
  for (iterations in c(1L, 20000L)) {
    expect_equal(.dp_transport(X, c(1, 1), c(1.0001, .9999),
                               max_iter = iterations),
                 server(X, c(1, 1), c(1.0001, .9999), max_iter = iterations),
                 tolerance = 0)
  }
})
