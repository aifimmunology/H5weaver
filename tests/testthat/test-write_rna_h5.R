context("write_rna_h5")
library(H5weaver)

test_that(
  "choose_integer_bits() determines how many bits are required to store a vector of integers",
  {
    expect_equal(choose_integer_bits(5), 16)
    expect_equal(choose_integer_bits(60000), 16)
    expect_equal(choose_integer_bits(70000), 32)
    expect_equal(choose_integer_bits(c(5, 60000, 100000)), 32)
    expect_equal(choose_integer_bits(2^33), 64)
  }
)

test_that(
  "choose_chunk_size() determines how large chunks should be for vectors of various sizes",
  {
    expect_equal(choose_chunk_size(runif(5)), 5)
    expect_equal(choose_chunk_size(runif(500)), 500)
    expect_equal(choose_chunk_size(runif(5000)), 100)
    expect_equal(choose_chunk_size(runif(70000)), 1000)
    expect_equal(choose_chunk_size(runif(300000)), 10000)
    expect_equal(choose_chunk_size(runif(9000000)), 100000)
    expect_equal(choose_chunk_size(runif(80000000)), 100000)
  }
)

test_that(
  "write_h5_list() can write results of h5_dump() to an h5 file",
  {
    test_h5 <- system.file("testdata/well1.h5", package = "H5weaver")
    h5_list <- h5dump(test_h5)

    h5_temp <- tempfile(pattern = ".h5")

    write_h5_list(h5_list,
                  h5_temp)

    test_ls <- h5ls(h5_temp)
    control_ls <- h5ls(system.file("testdata/well1.h5", package = "H5weaver"))
    expect_equal(test_ls, control_ls)
    expect_identical(h5read(h5_temp, "/matrix/barcodes"), h5read(test_h5, "/matrix/barcodes"))
    expect_identical(h5read(h5_temp, "/matrix/data"), h5read(test_h5, "/matrix/data"))

    expect_true(file.remove(h5_temp))

  }
)
