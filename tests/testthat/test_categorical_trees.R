library(data.table)
library(testthat)

# Test identify_impossible_agg() ------------------------------------------

# missing one province that has not changed historically
locations_present1 <- iran_mapping[!grepl("[0-9]+", child), child]
locations_present1 <- locations_present1[locations_present1 !=
                                           "Sistan and Baluchistan"]
iran_agg_tree1 <- create_agg_tree(iran_mapping, locations_present1,
                                  col_type = "categorical")

expected1 <- "Sistan and Baluchistan"

# missing one present day province
locations_present2 <- iran_mapping[!grepl("[0-9]+", child), child]
locations_present2 <- locations_present2[locations_present2 != "Tehran"]
iran_agg_tree2 <- create_agg_tree(iran_mapping, locations_present2,
                                  col_type = "categorical")

expected2 <- "Tehran"

# have one historical location in place of modern day provinces
locations_present3 <- iran_mapping[!grepl("[0-9]+", child), child]
locations_present3 <- locations_present3[!locations_present3 %in%
                                           c("Tehran", "Alborz", "Qom")]
locations_present3 <- c(locations_present3, "Tehran 1986-1995")
iran_agg_tree3 <- create_agg_tree(iran_mapping, locations_present3,
                                  col_type = "categorical")

expected3 <- c("Alborz", "Qom", "Tehran")

# all aggregates can be made
locations_present4 <- iran_mapping[!grepl("[0-9]+", child), child]
iran_agg_tree4 <- create_agg_tree(iran_mapping, locations_present4,
                                  col_type = "categorical")

expected4 <- NULL

test_that("impossible aggregates are correctly identified", {
  output1 <- identify_missing_agg(iran_agg_tree1)
  expect_identical(output1, expected1)

  output2 <- identify_missing_agg(iran_agg_tree2)
  expect_identical(output2, expected2)

  output3 <- identify_missing_agg(iran_agg_tree3)
  expect_identical(output3, expected3)

  output4 <- identify_missing_agg(iran_agg_tree4)
  expect_identical(output4, expected4)
})

# Test identify_missing_scale() -------------------------------------------

# all provinces in mapping included
locations_present1 <- unique(c(iran_mapping[, parent], iran_mapping[, child]))
iran_scale_tree1 <- create_scale_tree(iran_mapping, locations_present1,
                                      col_type = "categorical")

expected1 <- NULL

# all present day provinces in mapping included plus Iran
locations_present2 <- c(iran_mapping[!grepl("[0-9]+", child), child],
                        "Iran (Islamic Republic of)")
iran_scale_tree2 <- create_scale_tree(iran_mapping, locations_present2,
                                      col_type = "categorical")

# historical provinces
expected2 <- sort(iran_mapping[grepl("[0-9]+", child), child])

# all present day provinces in mapping included plus Iran with collapse missing
# nodes equal to TRUE
locations_present3 <- copy(locations_present2)
iran_scale_tree3 <- create_scale_tree(iran_mapping, locations_present3,
                                      col_type = "categorical", collapse = T)
expected3 <-NULL

test_that("missing scale nodes are correctly identified", {
  output1 <- identify_missing_scale(iran_scale_tree1)
  expect_identical(output1, expected1)

  output2 <- identify_missing_scale(iran_scale_tree2)
  expect_identical(output2, expected2)

  output3 <- identify_missing_scale(iran_scale_tree3)
  expect_identical(output3, expected3)
})
