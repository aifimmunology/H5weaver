library(rhdf5)
library(H5weaver)
library(hise)

chrM_genes <- fread(system.file("reference/GRCh38_10x_chrM_gene_metadata.csv.gz",
                                package = "H5weaver"))
h5_fid <- '1be65739-445a-41fe-89a4-52fbbba2000d'
hise::cacheFiles(list(h5_fid))
sample_cite_h5 <- list.files("cache/1be65739-445a-41fe-89a4-52fbbba2000d", full.names = T)

## Well 1 .h5 file
sample_list <- h5dump(sample_cite_h5)
sample_list <- h5_list_convert_to_dgCMatrix(sample_list)

set.seed(3)
keep_genes <- c(sample(sample_list$matrix$features$name, 1000), chrM_genes$name)
sample_keep <- match(keep_genes, sample_list$matrix$features$name)

sample_list$matrix_dgCMatrix <- sample_list$matrix_dgCMatrix[sample_keep,]

sample_list$matrix$features$name <- sample_list$matrix$features$name[sample_keep]
sample_list$matrix$features$genome <- sample_list$matrix$features$genome[sample_keep]
sample_list$matrix$features$feature_type <- sample_list$matrix$features$feature_type[sample_keep]
sample_list$matrix$features$interval <- sample_list$matrix$features$interval[sample_keep]

sample_list <- h5_list_convert_from_dgCMatrix(sample_list)

write_h5_list(sample_list,
              "cite_sample1.h5",
              overwrite = TRUE)
h5closeAll()

