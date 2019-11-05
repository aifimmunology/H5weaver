context("read_rna_h5")
library(H5weaver)

test_h5 <- system.file("testdata/well1.h5", package = "H5weaver")


test_that(
  "h5ls provides an additional full_name column.",
  {
    test_ls <- h5ls(test_h5)

    expect_true(class(test_ls) == "data.frame")
    expect_true("full_name" %in% names(test_ls))
    expect_equal(ncol(test_ls),6)

    expect_identical(test_ls[,names(test_ls) != "full_name"], rhdf5::h5ls(test_h5))

  }
)

test_that(
  "h5dims retrieves dimensions of an hdf5 object.",
  {
    test_ls <- h5ls(test_h5)

    test_dims <- h5dims(test_h5, "/matrix/barcodes")

    expect_true(class(test_dims) == "numeric")
    expect_true(length(test_dims) == 1)

    expect_equal(test_dims, as.numeric(test_ls$dim[test_ls$full_name == "/matrix/barcodes"]))
  }
)


test_that(
  "read_h5_dgCMatrix reads the sparse matrix from an .h5 file.",
  {
    test_mat <- read_h5_dgCMatrix(test_h5,
                                  feature_names = "id",
                                  sample_names = "barcodes")

    expect_true(class(test_mat) == "dgCMatrix")
    expect_equal(dim(test_mat), as.vector(h5read(test_h5, "/matrix/shape")))
    expect_equal(test_mat@x, as.vector(h5read(test_h5, "/matrix/data")))
    expect_identical(colnames(test_mat), as.vector(h5read(test_h5, "/matrix/barcodes")))
    expect_identical(rownames(test_mat), as.vector(h5read(test_h5, "/matrix/features/id")))
  }
)
