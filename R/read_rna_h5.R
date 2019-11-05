#' Read the /matrix from a .h5 file as a sparse matrix
#'
#' @param h5_file the path to an .h5 file in 10x Genomics format
#' @param feature_names a character object specifying whether to use "id" or "name" for row.names. Default is "id".
#' @param sample_names a character object specifying which values to use for col.names. If "barcodes", will use /matrix/barcodes. Other values will be read from /matrix/observations/
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

  assertthat::assert_that(is.character(sample_names))
  assertthat::assert_that(length(sample_names) == 1)

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

  h5_handle <- H5Fopen(h5_file)

  if(sample_names == "barcodes") {
    colname_target <- "/matrix/barcodes"
  } else {
    colname_target <- paste0("/matrix/observations/", sample_names)
  }

  mat <- sparseMatrix(x = h5read(h5_handle, "/matrix/data"),
                      i = h5read(h5_handle, "/matrix/indices"),
                      p = h5read(h5_handle, "/matrix/indptr"),
                      index1 = FALSE,
                      dims = h5read(h5_handle, "/matrix/shape"),
                      dimnames = list(as.vector(h5read(h5_handle, paste0("/matrix/features/", feature_names))),
                                      as.vector(h5read(h5_handle, colname_target))
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
  df$full_name <- sub("//","/",df$full_name)

  df <- df[,c("full_name","group","name","otype","dclass","dim")]

  df
}

#' Convert all 1D Arrays in a list object to vectors recursively
#'
#' @param x a list object
#'
#' @return a list object with all 1D arrays converted.
#' @export
#'
strip_1d_array_recursive <- function(x) {
  assertthat::assert_that(class(x) == "list")

  if(length(x) > 0) {
    for(n in seq_along(x)) {
      if(class(x[[n]]) == "list") {
        x[[n]] <- strip_1d_array_recursive(x[[n]])
      } else if(class(x[[n]]) == "array" & length(dim(x[[n]] == 1))) {
        x[[n]] <- as.vector(x[[n]])
      }
    }
  }

  x
}

#' Dump all objects from an HDF5 file to a list.
#'
#' This is a wrapper around rhdf5::h5dump() that converts all 1D arrays to vectors.
#'
#' @param ... parameters passed to rhdf5::h5dump()
#'
#' @return a list object with the contents of the target HDF5 file
#' @export
#'
h5dump <- function(...) {

  h5_list <- rhdf5::h5dump(...)
  h5_list <- strip_1d_array_recursive(h5_list)

  h5_list
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
