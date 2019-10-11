#' Read the /matrix from a .h5 file as a sparse matrix
#'
#' @param h5_file the path to an .h5 file in 10x Genomics format
#' @param feature_names a character object specifying whether to use "id" or "name" for row.names. Default is "id".
#' @param sample_names a character object specifying whether to use "barcodes" or "guids" for col.names. Default is "guid".
#'
#' @return a dgCMatrix of gene expression values.
#' @export
#'
read_h5_dgCMatrix <- function(h5_file,
                              feature_names = "id",
                              sample_names = "barcodes") {

  assertthat::assert_that(is.character(h5_file))
  assertthat::assert_that(length(h5_file) == 1)

  assertthat::assert_that(is.character(feature_names))
  assertthat::assert_that(length(feature_names) == 1)

  if(!file.exists(h5_file)) {
    stop(paste(h5_file, "does not exist."))
  }

  # Make sure the HDF5 file connection is closed if the function
  # exits due to an error.
  on.exit(expr = {
    H5Fclose(h5_handle)
  })

  feature_names <- match.arg(arg = feature_names,
                             choices = c("id","name"))

  sample_names <- match.arg(arg = sample_names,
                            choices = c("barcodes", "guids"))

  h5_handle <- H5Fopen(h5_file)

  if(sample_names == "guids") {
    if(!"guids" %in% h5ls(h5_handle)$name) {
      stop(paste("guids are not available in", h5_file))
    }
  }

  mat <- sparseMatrix(x = h5read(h5_handle, "/matrix/data"),
                      i = h5read(h5_handle, "/matrix/indices"),
                      p = h5read(h5_handle, "/matrix/indptr"),
                      index1 = FALSE,
                      dims = h5read(h5_handle, "/matrix/shape"),
                      dimnames = list(h5read(h5_handle, paste0("/matrix/features/", feature_names)),
                                      h5read(h5_handle, paste0("/matrix/barcodes"))
                                      )
                      )

  mat
}

#' List objects in an HDF5 file
#'
#' This is a wrapper around rhdf5::h5ls() that adds the full name of each object.
#'
#' @param ... parameters passed to rhdf5::h5ls()
#'
#' @return a data.frame listing the contents of the target hdf5 file.
#' @export
#'
h5ls <- function(...) {
  df <- rhdf5::h5ls(...)
  df$full_name <- paste0(df$group, "/" ,df$name)

  df <- df[,c("full_name","group","name","otype","dclass","dim")]

  df
}

#' Get dimensions of an object in an HDF5 file
#'
#' @param h5_file The path to an HDF5 file
#' @param name The full name of an object in the HDF5 file.
#'
#' @return a numeric vector containing the dimensions of the target object.
#' @export
#'
h5dims <- function(h5_file,
                   name) {

  h5_contents <- h5ls(h5_file)

  d <- h5_contents$dim[h5_contents$full_name == name]
  d <- unlist(strsplit(d, split = ","))

  as.numeric(d)
}
