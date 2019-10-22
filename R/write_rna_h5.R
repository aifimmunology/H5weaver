#' Create an extensible character HDF5 dataset
#'
#' @param h5_handle a connection object created by H5Fopen()
#' @param name a character object specifying the full path to the object to modify within the HDF5 file.
#' @param size the maximum length/nchar to use for storage. Default is 32 characters.
#'
#' @return No return value
#' @export
#'
create_ext_h5_character <- function(h5_handle,
                                    name,
                                    size = 32) {

  assertthat::assert_that(class(h5_handle) == "H5IdComponent")
  assertthat::assert_that(length(h5_handle) == 1)

  assertthat::assert_that(class(name) == "character")
  assertthat::assert_that(length(name) == 1)

  assertthat::assert_that(class(size) == "numeric")
  assertthat::assert_that(length(size) == 1)

  h5createDataset(h5_handle,
                  "/matrix/barcodes",
                  dims = 0,
                  storage.mode = "character",
                  size = size,
                  maxdims = H5Sunlimited())
}

#' Write values to an extensible charachter HDF5 dataset
#'
#' If the dataset
#'
#' @param object
#' @param h5_handle
#' @param name
#'
#' @return
#' @export
#'
#' @examples
write_ext_h5_character <- function(object,
                                   h5_handle,
                                   name) {

  assertthat::assert_that(class(object) == "character")

  assertthat::assert_that(class(h5_handle) == "H5IdComponent")
  assertthat::assert_that(length(h5_handle) == 1)

  assertthat::assert_that(class(name) == "character")
  assertthat::assert_that(length(name) == 1)

  h5_contents <- HTOparser::h5ls(h5_handle)

  if(name %in% h5_contents$full_name) {
    if(h5_contents$dim == "0") {
      append_ext_h5_character(object,
                              h5_handle,
                              name)
    } else {
      h5write(object,
              h5_handle,
              name)
    }

    create_ext_h5_character(h5_handle,
                            name,
                            size = max(nchar(object)))
    append_ext_h5_character(object,
                            h5_handle,
                            name)
  }

}

append_ext_h5_character <- function(object,
                                    h5_handle,
                                    name) {
  assertthat::assert_that(class(object) == "character")

  assertthat::assert_that(class(h5_handle) == "H5IdComponent")
  assertthat::assert_that(length(h5_handle) == 1)

  assertthat::assert_that(class(name) == "character")
  assertthat::assert_that(length(name) == 1)

  h5_contents <- HTOparser::h5ls(h5_handle)

  if(!name %in% h5_contents$full_name) {
    stop(paste(name, "does not exist. Use create_ext_h5_character() first."))
  }

  current_size <- h5dims(h5_handle,
                         name)

  new_size <- current_size + length(object)

  expand_h5_dataset(h5_handle,
                    name,
                    new_size)

  h5write(object,
          h5_handle,
          name,
          index = (current_size + 1):new_size)

}

#' Create an extensible unsigned integer HDF5 dataset
#'
#' This will generate an unsigned integer dataset within an HDF5 dataset, and will either determine the appropriate bit size
#' based on max_val or the supplied bits argument. The maximum size of the dataset will be set as H5Sunlimited() (effectively 2^64 - 1).
#'
#' If both max_val and bits are NULL, will use UINT32 by default.
#'
#' @param h5_handle a connection object created by H5Fopen()
#' @param name a character object specifying the full path to the object to modify within the HDF5 file.
#' @param max_val (optional) the maximum value of the object to write.
#' @param bits (optional) the number of bits to use for storing values. Must be 8, 16, 32, or 64.
#'
#' @return No return value
#' @export
#'
create_ext_h5_uint <- function(h5_handle,
                               name,
                               max_val = NULL,
                               bits = 32) {

  assertthat::assert_that(class(h5_handle) == "H5IdComponent")
  assertthat::assert_that(length(h5_handle) == 1)

  assertthat::assert_that(class(name) == "character")
  assertthat::assert_that(length(name) == 1)

  assertthat::assert_that(class(max_val) %in% c("NULL","numeric"))
  assertthat::assert_that(length(max_val) %in% c(0,1))

  assertthat::assert_that(class(bits) %in% c("NULL","numeric"))
  assertthat::assert_that(length(bits) %in% c(0,1))

  if(!is.null(max_val) & is.null(bits)) {
    if(max_val < 2^8) {
      bits <- 8
    } else if(max_val < 2^16) {
      bits <- 16
    } else if(max_val < 2^32) {
      bits <- 32
    } else if(max_val < 2^64) {
      bits <- 64
    }
  } else if(!is.null(bits)) {
    if(typeof(bits) == "double") {
      assertthat::assert_that(bits %in% c(8,16,32,64))
    } else if(typeof(bits) == "integer") {
      assertthat::assert_that(bits %in% c(8L, 16L, 32L, 64L))
    }
  } else {
    bits <- 32
  }

  H5type <- paste0("H5T_NATIVE_UINT",bits)

  h5createDataset(h5_handle,
                  name,
                  dims = 0,
                  H5type = H5type,
                  maxdims = H5Sunlimited())
}

#' Create an extensible float HDF5 dataset
#'
#' This will create a 32-bit float dataset compatible with base R numeric floats.
#'
#' @param h5_handle a connection object created by H5Fopen()
#' @param name a character object specifying the full path to the object to modify within the HDF5 file.
#'
#' @return No return value
#' @export
#'
create_ext_h5_float <- function(h5_handle,
                                 name) {

  assertthat::assert_that(class(h5_handle) == "H5IdComponent")
  assertthat::assert_that(length(h5_handle) == 1)

  assertthat::assert_that(class(name) == "character")
  assertthat::assert_that(length(name) == 1)

  h5createDataset(h5_handle,
                  name,
                  dims = 0,
                  H5type = "H5T_NATIVE_FLOAT",
                  maxdims = H5Sunlimited())
}

#' Generate an empty HDF5 container to store RNA-seq count data in 10x format
#'
#' @param h5_file
#' @param cell_barcode_length 18
#' @param data_type integer
#' @param data_bits 16
#' @param indices_bits 16
#' @param indptr_bits 32
#'
#' @return
#' @export
#'
#' @examples
write_10x_h5_container <- function(h5_file,
                                   cell_barcode_length = 15,
                                   data_type = "integer",
                                   data_bits = 16,
                                   indices_bits = 16,
                                   indptr_bits = 32) {

  assertthat::assert_that(is.character(h5_file))
  assertthat::assert_that(length(h5_file) == 1)

  if(file.exists(h5_file)) {
    stop(paste(h5_file, "already exists."))
  }

  # Make sure the HDF5 file connection is closed if the function
  # exits due to an error.
  on.exit(expr = {
    H5Fclose(h5_handle)
  })

  H5Fcreate(h5_file)

  h5_handle <- H5Fopen(h5_file)

  h5createGroup(h5_handle, "/matrix")
  h5createGroup(h5_handle, "/matrix/features")

  # default 10x cell metadata
  create_ext_h5_character(h5_handle,
                          name = "/matrix/barcodes",
                          size = cell_barcode_length)

  # default 10x feature metadata
  create_ext_h5_character(h5_handle,
                          name = "/matrix/features/feature_type",
                          size = 24)
  create_ext_h5_character(h5_handle,
                          name = "/matrix/features/genome",
                          size = 12)
  # ensembl gene ids, 15 characters
  create_ext_h5_character(h5_handle,
                          name = "/matrix/features/id",
                          size = 15)
  create_ext_h5_character(h5_handle,
                          name = "/matrix/features/name",
                          size = 24)

  # matrix data elements
  if(data_type == "integer") {
    create_ext_h5_uint(h5_handle,
                       name = "/matrix/data",
                       bits = data_bits)
  } else if(data_type %in% c("float", "numeric")) {
    create_ext_h5_float(h5_handle,
                        name = "/matrix/data")
  }

  create_ext_h5_uint(h5_handle,
                     name = "/matrix/indices",
                     bits = indices_bits)
  create_ext_h5_uint(h5_handle,
                     name = "/matrix/indptr",
                     bits = indptr_bits)

  H5Fclose(h5_handle)
}

#' Expand the size of an HDF5 dataset.
#'
#' This will only work up to the size limit set for the
#'
#' @param h5_file Either a character object specifying the location of an HDF5 file, or a connection created by H5Fopen()
#' @param name a character object specifying the full path to the object to modify within the HDF5 file.
#' @param new_size a numeric object specifying the new size of the object. Must be greater than the existing size.
#'
#' @return No return.
#' @export
#'
expand_h5_dataset <- function(h5_file,
                              name,
                              new_size) {

  assertthat::assert_that(class(h5_file) %in% c("character","H5IdComponent"))
  assertthat::assert_that(length(h5_file) == 1)

  assertthat::assert_that(class(name) == "character")
  assertthat::assert_that(length(name) == 1)

  assertthat::assert_that(class(new_size) == "numeric")
  assertthat::assert_that(length(new_size) == 1)

  if(class(h5_file) == "character") {
    on.exit(expr = {
      H5Fclose(h5_handle)
    })

    h5_handle <- H5Fopen(h5_file)
  } else if(class(h5_file) == "H5IdComponent") {
    h5_handle <- h5_file
  }

  current_size <- h5dims(h5_handle,
                         name)

  if(new_size <= current_size) {
    stop(paste("new_size", new_size, "is not greater than the current size of", name, ":", current_size))
  }

  dset_handle <- H5Dopen(h5_handle,
                         name = name)

  H5Dset_extent(dset_handle,
                new_size)

  H5Dclose(dset_handle)
}
