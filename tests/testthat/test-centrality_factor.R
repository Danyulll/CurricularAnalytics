test_that("Centrality Calculated Correctly", {
  edge_list_1 <- data.frame(from = c(1, 2, 1), to = c(2, 4, 3))
  node_list_1 <- data.frame(id = 1:4, label = c("v1","v2","v3","v4"), term = c(1,2,2,3))

  cf_df <- centrality_factor(node_list_1,edge_list_1)

  expect_equal(cf_df$cf , c(0,3,0,0))

  edge_list_2 <- data.frame(from = c(1, 2, 3), to = c(3, 3, 4))
  node_list_2 <- data.frame(id = 1:4, label = c("v1","v2","v3","v4"), term = c(1,1,2,3))

  cf_df <- centrality_factor(node_list_2,edge_list_2)

  expect_equal(cf_df$cf, c(0,0,6,0))
})
