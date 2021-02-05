#' Read the from a .h5ad file as a sparse matrix
#'
#' @param h5ad_file the path to an .h5ad file
#' @param feature_names a character object specifying whether to use "id" or "name" for row.names. Default is "name".
#'
#' @return a sparse matrix of class Matrix::dgCMatrix
#'
#' @return a dgCMatrix of gene expression values.
#' @export
#'
read_h5ad_dgCMatrix <- function(h5ad_file,
                                feature_names = "name") {

  assertthat::assert_that(is.character(h5ad_file))
  assertthat::assert_that(length(h5ad_file) == 1)

  barcodes <- as.vector(h5read(h5ad_file, "/obs/barcodes"))
  features <- as.vector(h5read(h5ad_file, paste0("/var/",feature_names)))

  Matrix::sparseMatrix(x = as.vector(h5read(h5ad_file, "/X/data")),
                       i = as.vector(h5read(h5ad_file, "/X/indices")),
                       p = as.vector(h5read(h5ad_file, "/X/indptr")),
                       dim = c(length(features), length(barcodes)),
                       dimnames = list(features, barcodes),
                       index1 = FALSE)

}

#' Read .h5ad Cell Metadata
#'
#' @param h5ad_file the path to an .h5ad file
#'
#' @return A data.frame containing all feature metadata found in /obs
#' @export
#'
read_h5ad_cell_meta <- function(h5ad_file) {

  assertthat::assert_that(is.character(h5ad_file))
  assertthat::assert_that(length(h5ad_file) == 1)

  h5ad_contents <- H5weaver::h5ls(h5ad_file)

  obs_locs <- h5ad_contents$full_name[h5ad_contents$group == "/obs"]
  obs_locs <- obs_locs[! obs_locs %in% c("/obs/__categories","/obs/_index")]

  obs_list <- lapply(obs_locs, function(loc) { as.vector(h5read(h5ad_file, loc)) })
  names(obs_list) <- sub(".+/","",obs_locs)

  obs_cats <- lapply(h5read(h5ad_file, "/obs/__categories"), as.vector)

  for(obs_cat in names(obs_cats)) {
    obs_list[[obs_cat]] <- obs_cats[[obs_cat]][obs_list[[obs_cat]] + 1]
  }

  as.data.frame(obs_list)

}

#' Read .h5ad Feature Metadata
#'
#' @param h5_file the path to an .h5 file in 10x Genomics format
#'
#' @return a data.frame containing all feature metadata found in /var
#' @export
#'
read_h5ad_feature_meta <- function(h5ad_file) {

  assertthat::assert_that(is.character(h5ad_file))
  assertthat::assert_that(length(h5ad_file) == 1)

  h5ad_contents <- H5weaver::h5ls(h5ad_file)

  var_locs <- h5ad_contents$full_name[h5ad_contents$group == "/var"]

  var_list <- lapply(var_locs, function(loc) { as.vector(h5read(h5ad_file, loc)) })
  names(var_list) <- sub(".+/","",var_locs)

  as.data.frame(var_list)

}
