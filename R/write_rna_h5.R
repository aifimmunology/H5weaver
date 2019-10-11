write_h5_container <- function(h5_file) {

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
  h5createGroup(h5_handle, "/matrix/feature")

  h5createDataset(h5_handle,
                  "/matrix/barcodes",
                  dims = 0,
                  storage.mode = "character",
                  size = 32,
                  maxdims = H5Sunlimited())

  # add other objects

}

expand_h5_dataset <- function(h5_file,
                              name,
                              new_size) {

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
