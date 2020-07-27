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

test_that(
  "read_h5_cell_meta() reads cell metadata from a .h5 file.",
  {
    h5_list <- h5dump(test_h5)
    h5_list <- add_well_metadata(h5_list, "X000-P1W1C1")

    h5_list_meta <- h5_list_cell_metadata(h5_list)

    temp_h5 <- tempfile(fileext = ".h5")

    write_h5_list(h5_list, temp_h5)

    h5_test_meta <- read_h5_cell_meta(temp_h5)

    expect_true(class(h5_test_meta) == "data.frame")
    expect_identical(h5_test_meta[, names(h5_list_meta)], h5_list_meta)

  }
)

test_that(
  "read_h5_feature_meta() reads feature metadata from a .h5 file.",
  {
    h5_list <- h5dump(test_h5)
    h5_list_meta <- as.data.frame(h5_list$matrix$features[!grepl("^_", names(h5_list$matrix$features))],
                                  stringsAsFactors = FALSE)

    h5_test_meta <- read_h5_feature_meta(test_h5)

    expect_true(class(h5_test_meta) == "data.frame")
    expect_identical(h5_test_meta[, names(h5_list_meta)], h5_list_meta)
  }
)

test_that(
  "strip_1d_array_recursive() needs tests",
  {
    h5_list <- rhdf5::h5dump(test_h5)
    h5_list_matrix_classes <- unlist(lapply(h5_list$matrix, class))
    h5_list_feature_classes <- unlist(lapply(h5_list$matrix$features, class))

    h5_list_no_array <- strip_1d_array_recursive(h5_list)
    h5_list_no_array_matrix_classes <- unlist(lapply(h5_list_no_array$matrix, class))
    h5_list_no_array_feature_classes <- unlist(lapply(h5_list_no_array$matrix$features, class))

    expect_true(class(h5_list_no_array) == class(h5_list))
    expect_equal(sum(h5_list_no_array_matrix_classes == "list"),
                 sum(h5_list_matrix_classes == "list"))
    expect_equal(sum(h5_list_no_array_matrix_classes != "list"),
                 sum(h5_list_matrix_classes == "array"))
    expect_equal(sum(h5_list_no_array_matrix_classes == "array"), 0)

    expect_equal(sum(h5_list_no_array_feature_classes == "list"),
                 sum(h5_list_feature_classes == "list"))
    expect_equal(sum(h5_list_no_array_feature_classes != "list"),
                 sum(h5_list_feature_classes == "array"))
    expect_equal(sum(h5_list_no_array_feature_classes == "array"), 0)

  }
)

test_that(
  "convert_char_na_recursive() needs tests",
  {
    h5_list <- h5dump(test_h5)
    h5_list$matrix$barcodes[sample(1:length(h5_list$matrix$barcodes), 100)] <- "NA"
    h5_list$matrix$features$name[sample(1:length(h5_list$matrix$features$name), 100)] <- "NA"

    h5_list_na <- convert_char_na_recursive(h5_list)

    expect_true(class(h5_list_na) == class(h5_list))
    expect_identical(which(is.na(h5_list_na$matrix$barcodes)),
                     which(h5_list$matrix$barcodes == "NA"))
    expect_identical(which(is.na(h5_list_na$matrix$features$name)),
                     which(h5_list$matrix$features$name == "NA"))

  }
)

test_that(
  "h5_dump() needs tests",
  {
    rhdf5_h5_list <- rhdf5::h5dump(test_h5)

    fixed_rhdf5_h5_list <- strip_1d_array_recursive(rhdf5_h5_list)
    fixed_rhdf5_h5_list <- convert_char_na_recursive(fixed_rhdf5_h5_list)

    h5weaver_h5_list <- h5dump(test_h5)

    expect_identical(h5weaver_h5_list,
                     fixed_rhdf5_h5_list)
  }
)
