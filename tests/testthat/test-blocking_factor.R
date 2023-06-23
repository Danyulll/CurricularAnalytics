test_that("Blocking Factor Calculated Correctly", {
  edge_list_1 <- data.frame(from = c(1, 2, 1), to = c(2, 4, 3))
  node_list_1 <- data.frame(id = 1:4, label = c("v1","v2","v3","v4"), term = c(1,2,2,3))

  bf_list <- blocking_factor(node_list_1,edge_list_1)

  expect_equal(bf_list$bynode$bf , c(3,1,0,0))
  expect_equal(bf_list$total , 4)

  edge_list_2 <- data.frame(from = c(1, 2, 3), to = c(3, 3, 4))
  node_list_2 <- data.frame(id = 1:4, label = c("v1","v2","v3","v4"), term = c(1,1,2,3))

  bf_list <- blocking_factor(node_list_2,edge_list_2)

  expect_equal(bf_list$bynode$bf, c(2,2,1,0))
  expect_equal(bf_list$total, 5)
})
