context("write_rna_h5")
library(H5weaver)

test_that(
  "stm() generates a message",
  {
    test_message <- "hello there"

    test_capture <- capture.output(stm(test_message), type = "message")

    expect_true(grepl(test_message, test_capture))
  }
)
