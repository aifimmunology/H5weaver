context("filter_rna_h5")
library(H5weaver)

test_h5 <- system.file("testdata/well1.h5", package = "H5weaver")
test_h5l <- h5dump(test_h5)

category_table <- data.table::fread(system.file("testdata/well1_category_table.csv.gz", package = "H5weaver"))
hash_count_table <- data.table::fread(system.file("testdata/well1_count_matrix.csv.gz", package = "H5weaver"))
hash_count_matrix <- as.matrix(hash_count_table[,-1])
rownames(hash_count_matrix) <- hash_count_table[[1]]

test_that(
  "add_cell_ids() adds ids to h5_list objects",
  {
    id_result <- add_cell_ids(test_h5l,
                              add_uuid = TRUE,
                              replace_barcode = FALSE,
                              retain_original_barcode = FALSE,
                              add_name = FALSE)

    expect_false("observations" %in% names(test_h5l$matrix))
    expect_true("observations" %in% names(id_result$matrix))
    expect_true(class(id_result$matrix$observations) == "list")

    expect_true("cell_uuid" %in% names(id_result$matrix$observations))
    expect_true(class(id_result$matrix$observations$cell_uuid) == "character")
    expect_equal(length(id_result$matrix$observations$cell_uuid), length(test_h5l$matrix$barcodes))
    expect_equal(length(id_result$matrix$observations$cell_uuid), length(unique(id_result$matrix$observations$cell_uuid)))

    id_result2 <- add_cell_ids(test_h5l,
                               add_uuid = TRUE,
                               replace_barcode = TRUE,
                               retain_original_barcode = FALSE,
                               add_name = FALSE)

    expect_true("observations" %in% names(id_result2$matrix))
    expect_true(class(id_result2$matrix$observations) == "list")

    expect_true("cell_uuid" %in% names(id_result2$matrix$observations))
    expect_true(class(id_result2$matrix$observations$cell_uuid) == "character")
    expect_equal(length(id_result2$matrix$observations$cell_uuid), length(test_h5l$matrix$barcodes))
    expect_identical(id_result2$matrix$barcodes, id_result2$matrix$observations$cell_uuid)
    expect_equal(length(intersect(id_result2$matrix$barcodes, test_h5l$matrix$barcodes)), 0)

    id_result3 <- add_cell_ids(test_h5l,
                               add_uuid = TRUE,
                               replace_barcode = TRUE,
                               retain_original_barcode = TRUE,
                               add_name = FALSE)

    expect_true("observations" %in% names(id_result3$matrix))
    expect_true(class(id_result3$matrix$observations) == "list")

    expect_true("cell_uuid" %in% names(id_result3$matrix$observations))
    expect_true(class(id_result3$matrix$observations$cell_uuid) == "character")
    expect_equal(length(id_result3$matrix$observations$cell_uuid), length(test_h5l$matrix$barcodes))
    expect_identical(id_result3$matrix$barcodes, id_result3$matrix$observations$cell_uuid)
    expect_equal(length(intersect(id_result3$matrix$barcodes, test_h5l$matrix$barcodes)), 0)

    expect_true("original_barcodes" %in% names(id_result3$matrix$observations))
    expect_true(class(id_result3$matrix$observations$original_barcodes) == "character")
    expect_equal(length(id_result3$matrix$observations$original_barcodes), length(test_h5l$matrix$barcodes))
    expect_equal(length(intersect(id_result3$matrix$observations$original_barcodes, test_h5l$matrix$barcodes)), length(test_h5l$matrix$barcodes))

    id_result4 <- add_cell_ids(test_h5l,
                               add_uuid = FALSE,
                               replace_barcode = FALSE,
                               retain_original_barcode = FALSE,
                               add_name = TRUE)

    expect_true("observations" %in% names(id_result4$matrix))
    expect_true(class(id_result4$matrix$observations) == "list")

    expect_true("cell_name" %in% names(id_result4$matrix$observations))
    expect_true(class(id_result4$matrix$observations$cell_name) == "character")
    expect_equal(length(id_result4$matrix$observations$cell_name), length(test_h5l$matrix$barcodes))
    expect_equal(length(id_result4$matrix$observations$cell_name), length(unique(id_result4$matrix$observations$cell_name)))

  }
)

test_that(
  "h5_list_convert_to_dgCMatrix() needs tests",
  {
    to_mat <- h5_list_convert_to_dgCMatrix(test_h5l,
                                           target = "matrix")

    expect_true("matrix_dgCMatrix" %in% names(to_mat))
    expect_true(class(to_mat$matrix_dgCMatrix) == "dgCMatrix")

    expect_equal(to_mat$matrix_dgCMatrix@x, test_h5l$matrix$data)
    expect_identical(to_mat$matrix_dgCMatrix@i, test_h5l$matrix$indices)
    expect_identical(to_mat$matrix_dgCMatrix@p, test_h5l$matrix$indptr)
    expect_identical(dim(to_mat$matrix_dgCMatrix), test_h5l$matrix$shape)
    expect_identical(colnames(to_mat$matrix_dgCMatrix), test_h5l$matrix$barcodes)
    expect_identical(rownames(to_mat$matrix_dgCMatrix), test_h5l$matrix$features$id)

  }
)


test_that(
  "h5_list_convert_from_dgCMatrix() needs tests",
  {
    to_mat <- h5_list_convert_to_dgCMatrix(test_h5l,
                                           target = "matrix")
    from_mat <- h5_list_convert_from_dgCMatrix(to_mat,
                                               target = "matrix")

    expect_true("matrix" %in% names(from_mat))
    expect_false("matrix_dgCMatrix" %in% names(from_mat))

    expect_equal(from_mat$matrix$data, test_h5l$matrix$data)
    expect_identical(from_mat$matrix$indices, test_h5l$matrix$indices)
    expect_identical(from_mat$matrix$indptr, test_h5l$matrix$indptr)
    expect_identical(from_mat$matrix$shape, test_h5l$matrix$shape)
    expect_identical(from_mat$matrix$barcodes, test_h5l$matrix$barcodes)
    expect_identical(from_mat$matrix$features$id, test_h5l$matrix$features$id)
  }
)

test_that(
  "h5_list_add_dgCMatrix() - not currently used by pipeline",
  {

  }
)

test_that(
  "subset_h5_list_by_observations() needs tests",
  {
    subset_barcodes <- sample(test_h5l$matrix$barcodes, 100)

    subset_result <- subset_h5_list_by_observations(test_h5l,
                                                    match_values = subset_barcodes,
                                                    match_target = "barcodes",
                                                    sparse_matrices = "matrix")

    expect_equal(length(subset_result$matrix$barcodes), length(subset_barcodes))
    expect_equal(length(subset_result$matrix$indptr), length(subset_barcodes) + 1)
    expect_equal(subset_result$matrix$shape[2], length(subset_barcodes))
    expect_identical(subset_result$matrix$barcodes, subset_barcodes)

    test_h5l_names <- add_cell_ids(test_h5l,
                                   add_uuid = FALSE,
                                   add_name = TRUE)

    subset_names <- sample(test_h5l_names$matrix$observations$cell_name, 75)

    subset_result2 <- subset_h5_list_by_observations(test_h5l_names,
                                                    match_values = subset_names,
                                                    match_target = "cell_name",
                                                    sparse_matrices = "matrix")

    expect_equal(length(subset_result2$matrix$barcodes), length(subset_names))
    expect_equal(length(subset_result2$matrix$indptr), length(subset_names) + 1)
    expect_equal(subset_result2$matrix$shape[2], length(subset_names))
    expect_identical(subset_result2$matrix$observations$cell_name, subset_names)

  }
)

test_that(
  "split_h5_list_by_hash() needs tests",
  {

    unique_hto_barcodes <- unique(category_table$hto_barcode[category_table$hto_category == "singlet"])

    test_split <- split_h5_list_by_hash(test_h5l,
                                        hash_category_table = category_table,
                                        hash_count_matrix = NULL,
                                        add_uuid = FALSE,
                                        add_name = FALSE,
                                        well_id = "R20191105-C1-W1")

    filtered_category_table <- category_table[category_table$cell_barcode %in% sub("-1","",test_h5l$matrix$barcodes),]

    expect_true(class(test_split) == "list")
    expect_equal(length(test_split), length(unique_hto_barcodes) + 1)
    expect_equal(length(test_split[[unique_hto_barcodes[1]]]$matrix$barcodes), sum(filtered_category_table$hto_barcode == unique_hto_barcodes[1]))
    expect_equal(length(test_split[["multiplet"]]$matrix$barcodes), sum(filtered_category_table$hto_category != "singlet"))

    expect_identical(names(test_split[[1]]), c("matrix","hash"))
    expect_true(class(test_split[[1]]$hash) == "list")
    expect_true(class(test_split[[1]]$hash$observations$hash_category) == "character")
    expect_equal(length(test_split[[unique_hto_barcodes[1]]]$hash$observations$hash_category), sum(filtered_category_table$hto_barcode == unique_hto_barcodes[1]))

    test_split2 <- split_h5_list_by_hash(test_h5l,
                                        hash_category_table = category_table,
                                        hash_count_matrix = hash_count_matrix,
                                        add_uuid = TRUE,
                                        add_name = TRUE,
                                        well_id = "R20191105-C1-W1")

    expect_true(class(test_split2) == "list")

    expect_identical(names(test_split2[[1]]), c("matrix","hash"))
    expect_true(class(test_split2[[1]]$hash) == "list")
    expect_equal(length(test_split2[[unique_hto_barcodes[1]]]$hash$barcodes), sum(filtered_category_table$hto_barcode == unique_hto_barcodes[1]))
    expect_equal(length(test_split2[["multiplet"]]$hash$barcodes), sum(filtered_category_table$hto_category != "singlet"))

  }
)

test_that(
  "cat_h5_list() needs tests.",
  {
    test_split <- split_h5_list_by_hash(test_h5l,
                                        hash_category_table = category_table,
                                        hash_count_matrix = NULL,
                                        add_uuid = FALSE,
                                        add_name = FALSE,
                                        well_id = "R20191105-C1-W1")


    test_split_1 <- h5_list_convert_to_dgCMatrix(test_split[[1]], target = "matrix")
    test_split_2 <- h5_list_convert_to_dgCMatrix(test_split[[2]], target = "matrix")

    test_cat <- cat_h5_list(test_split_1, test_split_2)
    test_cat <- h5_list_convert_from_dgCMatrix(test_cat,
                                               target = "matrix")

    expect_true(class(test_cat) == "list")
    expect_equal(test_cat$matrix$shape[2], test_split[[1]]$matrix$shape[2] + test_split[[2]]$matrix$shape[2])

    expect_equal(test_cat$matrix$data, c(test_split[[1]]$matrix$data, test_split[[2]]$matrix$data))
    expect_equal(test_cat$matrix$indices, c(test_split[[1]]$matrix$indices, test_split[[2]]$matrix$indices))
    expect_equal(test_cat$matrix$barcodes, c(test_split[[1]]$matrix$barcodes, test_split[[2]]$matrix$barcodes))

  }
)

test_that(
  "reduce_h5_list() needs tests.",
  {
    test_split <- split_h5_list_by_hash(test_h5l,
                                        hash_category_table = category_table,
                                        hash_count_matrix = NULL,
                                        add_uuid = FALSE,
                                        add_name = FALSE,
                                        well_id = "R20191105-C1-W1")

    filtered_category_table <- category_table[category_table$cell_barcode %in% sub("-1","",test_h5l$matrix$barcodes),]

    test_reduce <- reduce_h5_list(test_split)

    expect_true(class(test_reduce) == "list")
    expect_equal(test_reduce$matrix$shape[2], nrow(filtered_category_table))

  }
)
