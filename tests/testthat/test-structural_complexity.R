test_that("Structural Complexity Correctly", {
  edge_list_1 <- data.frame(ffrom = integer(), to = integer())
  node_list_1 <-
    data.frame(
      id = 1:4,
      label = c("v1", "v2", "v3", "v4"),
      term = c(1, 2, 2, 3)
    )

  sc_list <- structural_complexity(node_list_1, edge_list_1)

  expect_equal(sc_list$bynode$sc , rep(1, 4))
  expect_equal(sc_list$total, 4)

  edge_list_2 <- data.frame(from = c(1, 1, 2, 2), to = c(3, 4, 3, 4))
  node_list_2 <-
    data.frame(
      id = 1:4,
      label = c("v1", "v2", "v3", "v4"),
      term = c(1, 1, 2, 3)
    )

  sc_list <- structural_complexity(node_list_2, edge_list_2)

  expect_equal(sc_list$bynode$sc, c(4, 4, 2, 2))
  expect_equal(sc_list$total, 12)
})
