context("util")
library(H5weaver)

test_that(
  "stm() generates a message",
  {
    test_message <- "hello there"

    test_capture <- capture.output(stm(test_message), type = "message")

    expect_true(grepl(test_message, test_capture))
  }
)

test_list <- list(forest = list(country = "USA",
                                maple = list(height = 100),
                                dogwood = list(height = 30,
                                               branch = list(seeds = 15))))

test_that(
  "get_list_path() retrieves values from a nested list",
  {
    forest <- get_list_path(test_list,
                            "/forest")
    expect_true(class(forest) == "list")
    expect_equal(length(forest), 3)
    expect_equal(forest, test_list$forest)

    dogwood <- get_list_path(test_list,
                             "/forest/dogwood")

    expect_true(class(dogwood) == "list")
    expect_equal(length(dogwood), 2)
    expect_equal(dogwood, test_list$forest$dogwood)

    dogwood_height <- get_list_path(test_list,
                                    "/forest/dogwood/height")
    expect_true(class(dogwood_height) == "numeric")
    expect_equal(dogwood_height, test_list$forest$dogwood$height)
  }
)

test_that(
  "set_list_path() adds values to a nested list",
  {
    test_list_result <- set_list_path(test_list,
                                      target = "/valley/dandelion/leaves",
                                      value = 8)
    expect_true(class(test_list_result) == "list")
    expect_true(class(test_list_result$valley) == "list")
    expect_equal(test_list_result$valley$dandelion$leaves, 8)
  }
)

test_that(
  "h5_list_cell_metadata() retrieves metadata from an h5_list object",
  {
    test_h5 <- system.file("testdata/well1.h5",
                           package = "H5weaver")

    test_h5_list1 <- h5dump(test_h5)

    meta_result1 <- h5_list_cell_metadata(test_h5_list1)

    expect_true(class(meta_result1) == "data.frame")
    expect_equal(nrow(meta_result1), length(test_h5_list1$matrix$barcodes))
    expect_equal(length(meta_result1), 1)
    expect_identical(names(meta_result1), "barcodes")

    test_h5_list2 <- add_well_metadata(test_h5_list1,
                                       "B000-P1C1W1")

    meta_result2 <- h5_list_cell_metadata(test_h5_list2)

    expect_true(class(meta_result2) == "data.frame")
    expect_equal(nrow(meta_result2), length(test_h5_list2$matrix$barcodes))
    expect_equal(length(meta_result2), length(test_h5_list2$matrix$observations) + 1)
    expect_identical(names(meta_result2), c("barcodes", names(test_h5_list2$matrix$observations)))

  }
)

test_that(
  "h5_list_transpose() transposes an h5_list object and swaps observations and features",
  {
    test_h5 <- system.file("testdata/well1.h5",
                           package = "H5weaver")

    test_h5_list1 <- h5dump(test_h5)

    test_h5_list2 <- add_well_metadata(test_h5_list1,
                                       "B000-P1C1W1")

    transpose_result <- h5_list_transpose(test_h5_list2,
                                          sparse_matrices = "matrix")

    expect_true(class(transpose_result) == "list")
    expect_identical(names(transpose_result), names(test_h5_list2))
    expect_identical(transpose_result$features[names(transpose_result$features) != "id"],
                     test_h5_list2$observations)
    expect_identical(transpose_result$observations,
                     test_h5_list2$features[names(test_h5_list2$features) != "id"])
    expect_identical(transpose_result$features$id,
                     test_h5_list2$barcodes)
    expect_identical(transpose_result$barcodes,
                     test_h5_list2$features$id)
    expect_equal(transpose_result$matrix$shape,
                 rev(test_h5_list2$matrix$shape))

  }
)
