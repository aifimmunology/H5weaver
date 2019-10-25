library(rhdf5)
library(H5weaver)

well1_h5 <- "G:/Shared drives/Imm - Molecular Biology/Analysis/pipeline_longitudinal_pilot/data/cellranger/PB7626W4-01-RNA_CS/filtered_feature_bc_matrix.h5"
well2_h5 <- "G:/Shared drives/Imm - Molecular Biology/Analysis/pipeline_longitudinal_pilot/data/cellranger/PB7626W6-01-RNA_CS/filtered_feature_bc_matrix.h5"

keep_genes <- c("HSPA8","ERCC6","CD3E","CD27","CD68","CD14","CD4","ENTPD1","NCAM1","CD34")

well1_list <- h5dump(well1_h5)
well1_list <- h5_list_convert_to_dgCMatrix(well1_list)

well1_keep <- match(keep_genes, well1_list$matrix$features$name)

well1_list$h5_dgCMatrix <- well1_list$h5_dgCMatrix[well1_keep,]

well1_list$matrix$features$name <- well1_list$matrix$features$name[well1_keep]
well1_list$matrix$features$genome <- well1_list$matrix$features$genome[well1_keep]
well1_list$matrix$features$feature_type <- well1_list$matrix$features$feature_type[well1_keep]

well1_list <- h5_list_convert_from_dgCMatrix(well1_list)

write_h5_list(well1_list,
              "inst/testdata/well1.h5")
h5closeAll()

file.copy("G:/Shared drives/Imm - Molecular Biology/Analysis/pipeline_longitudinal_pilot/output/hto_results/PB7626W4-01-HTO_S21/hto_category_table.csv.gz",
          "inst/testdata/well1_category_table.csv.gz")
file.copy("G:/Shared drives/Imm - Molecular Biology/Analysis/pipeline_longitudinal_pilot/output/hto_results/PB7626W4-01-HTO_S21/hto_count_matrix.csv.gz",
          "inst/testdata/well1_count_matrix.csv.gz")

well2_list <- h5dump(well2_h5)
well2_list <- h5_list_convert_to_dgCMatrix(well2_list)

well2_keep <- match(keep_genes, well2_list$matrix$features$name)

well2_list$h5_dgCMatrix <- well2_list$h5_dgCMatrix[well2_keep,]

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
