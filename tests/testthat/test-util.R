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
