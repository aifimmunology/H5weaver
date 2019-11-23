library(rhdf5)
library(H5weaver)

well1_h5 <- "G:/Shared drives/Imm - Molecular Biology/Analysis/pipeline_longitudinal_pilot/data/cellranger/PB7626W4-01-RNA/filtered_feature_bc_matrix.h5"
well2_h5 <- "G:/Shared drives/Imm - Molecular Biology/Analysis/pipeline_longitudinal_pilot/data/cellranger/PB7626W6-01-RNA/filtered_feature_bc_matrix.h5"

well1_list <- h5dump(well1_h5)
well1_list <- h5_list_convert_to_dgCMatrix(well1_list)

set.seed(3030)
keep_genes <- c(sample(well1_list$matrix$features$name, 1000))

well1_keep <- match(keep_genes, well1_list$matrix$features$name)

well1_list$matrix_dgCMatrix <- well1_list$matrix_dgCMatrix[well1_keep,]

well1_list$matrix$features$name <- well1_list$matrix$features$name[well1_keep]
well1_list$matrix$features$genome <- well1_list$matrix$features$genome[well1_keep]
well1_list$matrix$features$feature_type <- well1_list$matrix$features$feature_type[well1_keep]

well1_list <- h5_list_convert_from_dgCMatrix(well1_list)

write_h5_list(well1_list,
              "inst/testdata/well1.h5")
h5closeAll()

well1_mol <- "G:/Shared drives/Imm - Molecular Biology/Analysis/pipeline_longitudinal_pilot/data/cellranger/PB7626W4-01-RNA/molecule_info.h5"

well1_keep_bc <- sub("-1","",h5read("inst/testdata/well1.h5","/matrix/barcodes"))

well1_feature_idx <- match(keep_genes, h5read(well1_mol, "/features/name")) - 1
well1_idx <- which(h5read(well1_mol, "/feature_idx") %in% well1_feature_idx)

well1_list <- list(barcode_idx = h5read(well1_mol, "/barcode_idx")[well1_idx],
                   barcodes = h5read(well1_mol, "/barcodes"),
                   count = h5read(well1_mol, "/count")[well1_idx])

well1_dt <- data.table(barcode_idx = well1_list$barcode_idx,
                       barcodes = well1_list$barcodes[well1_list$barcode_idx + 1],
                       count = well1_list$count)
well1_dt <- well1_dt[barcodes %in% well1_keep_bc,]

well1_list_out <- list(barcode_idx = match(well1_dt$barcodes, well1_keep_bc) - 1,
                       barcodes = well1_keep_bc,
                       count = well1_dt$count)

write_h5_list(well1_list_out,
              "inst/testdata/well1_molecule_info.h5",
              overwrite = TRUE)
h5closeAll()

well2_mol <- "G:/Shared drives/Imm - Molecular Biology/Analysis/pipeline_longitudinal_pilot/data/cellranger/PB7626W6-01-RNA/molecule_info.h5"

well2_keep_bc <- sub("-1","",h5read("inst/testdata/well2.h5","/matrix/barcodes"))

well2_feature_idx <- match(keep_genes, h5read(well2_mol, "/features/name")) - 1
well2_idx <- which(h5read(well2_mol, "/feature_idx") %in% well2_feature_idx)

well2_list <- list(barcode_idx = h5read(well2_mol, "/barcode_idx")[well2_idx],
                   barcodes = h5read(well2_mol, "/barcodes"),
                   count = h5read(well2_mol, "/count")[well2_idx])

well2_dt <- data.table(barcode_idx = well2_list$barcode_idx,
                       barcodes = well2_list$barcodes[well2_list$barcode_idx + 1],
                       count = well2_list$count)
well2_dt <- well2_dt[barcodes %in% well2_keep_bc,]

well2_list_out <- list(barcode_idx = match(well2_dt$barcodes, well2_keep_bc) - 1,
                       barcodes = well2_keep_bc,
                       count = well2_dt$count)

write_h5_list(well2_list_out,
              "inst/testdata/well2_molecule_info.h5",
              overwrite = TRUE)
h5closeAll()

file.copy("G:/Shared drives/Imm - Molecular Biology/Analysis/pipeline_longitudinal_pilot/output/hto_results/PB7626W4-01-HTO_S21/hto_category_table.csv.gz",
          "inst/testdata/well1_category_table.csv.gz")
well1_tbl <- fread("inst/testdata/well1_category_table.csv.gz")

ss <- fread(system.file("reference/SampleSheet_fallback.csv", package = "HTOparser"),
            col.names = c("pbmc_sample_id","batch_id","hto_name","pool_id"))
bc_key <- fread(system.file("reference/TotalSeqA_human_hto_key.csv", package = "HTOparser"),
                header = FALSE, col.names = c("hto_barcode","hto_name"))
bc_key <- dplyr::left_join(bc_key,ss)

hto_barcodes_to_pbmc_ids <- function(x,
                                     bc_key) {
  x <- as.character(x)
  res <- character()
  for(i in seq_along(x)) {
    bcs <- unlist(strsplit(x[i], ";")[[1]])
    res[i] <- paste(bc_key$pbmc_sample_id[match(bcs, bc_key$hto_barcode)],
                    collapse = ";")
  }
  res
}
well1_tbl$pbmc_sample_id <- hto_barcodes_to_pbmc_ids(well1_tbl$hto_barcode,
                                                     bc_key)
fwrite(well1_tbl,
       "inst/testdata/well1_category_table.csv.gz")

file.copy("G:/Shared drives/Imm - Molecular Biology/Analysis/pipeline_longitudinal_pilot/output/hto_results/PB7626W4-01-HTO_S21/hto_count_matrix.csv.gz",
          "inst/testdata/well1_count_matrix.csv.gz")

well2_list <- h5dump(well2_h5)
well2_list <- h5_list_convert_to_dgCMatrix(well2_list)

well2_keep <- match(keep_genes, well2_list$matrix$features$name)

well2_list$matrix_dgCMatrix <- well2_list$matrix_dgCMatrix[well2_keep,]

well2_list$matrix$features$name <- well2_list$matrix$features$name[well2_keep]
well2_list$matrix$features$genome <- well2_list$matrix$features$genome[well2_keep]
well2_list$matrix$features$feature_type <- well2_list$matrix$features$feature_type[well2_keep]

well2_list <- h5_list_convert_from_dgCMatrix(well2_list)

write_h5_list(well2_list,
              "inst/testdata/well2.h5")
h5closeAll()

file.copy("G:/Shared drives/Imm - Molecular Biology/Analysis/pipeline_longitudinal_pilot/output/hto_results/PB7626W6-01-HTO_S23/hto_category_table.csv.gz",
          "inst/testdata/well2_category_table.csv.gz")
file.copy("G:/Shared drives/Imm - Molecular Biology/Analysis/pipeline_longitudinal_pilot/output/hto_results/PB7626W6-01-HTO_S23/hto_count_matrix.csv.gz",
          "inst/testdata/well2_count_matrix.csv.gz")

dir.create("inst/testdata/splitdata/")

file.copy(system.file("rmarkdown/split_h5_by_hash.Rmd", package = "H5weaver"),
          "./split_h5_by_hash.Rmd")

rmarkdown::render(
  input = "./split_h5_by_hash.Rmd",
  params = list(in_h5 = system.file("testdata/well1.h5", package = "H5weaver"),
                in_mol = system.file("testdata/well1_molecule_info.h5", package = "H5weaver"),
                in_mat = system.file("testdata/well1_count_matrix.csv.gz", package = "H5weaver"),
                in_tbl = system.file("testdata/well1_category_table.csv.gz", package = "H5weaver"),
                in_well = "T001-P1C1W1",
                out_dir = "inst/testdata/splitdata/"),
  output_file = file.path("inst/testdata/splitdata/", "well1_split_summary.html"),
  quiet = TRUE
)

rmarkdown::render(
  input = "./split_h5_by_hash.Rmd",
  params = list(in_h5 = system.file("testdata/well2.h5", package = "H5weaver"),
                in_mol = system.file("testdata/well2_molecule_info.h5", package = "H5weaver"),
                in_mat = system.file("testdata/well2_count_matrix.csv.gz", package = "H5weaver"),
                in_tbl = system.file("testdata/well2_category_table.csv.gz", package = "H5weaver"),
                in_well = "T001-P1C1W2",
                out_dir = "inst/testdata/splitdata/"),
  output_file = file.path("inst/testdata/splitdata/", "well2_split_summary.html"),
  quiet = TRUE
)

file.remove("./split_h5_by_hash.Rmd")
