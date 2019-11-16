# H5weaver
### Read, Rearrange, and Write HDF5 files with 10x Genomics conventions

## Requirements

`H5weaver` requires libraries for HDF5 files. On Windows, these are bundled with the `rhdf5` package. On Linux/Unix, you will need to first install hdf5 libraries.

This can usually be accomplished with:
```
sudo apt-get install hdf5-devel
```
or
```
sudo yum install hdf5-devel
```

Once hdf5 libraries are available, you can proceed to installation of `rhdf5`.

The `rhdf5` package is provided through BioConductor, and can be installed using:
```
if(!"BiocManager" %in% .packages(all.available = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install("rhdf5")
```

`H5weaver` also requires the `data.table`, `ids`, and `Matrix` packages, which are available on CRAN and should be automatically installed by `install_github()`.

# Installation

This package can be installed from Github using the `devtools` package.

You may first need to register your GitHub PAT, as this is a private repository.
```
Sys.setenv(GITHUB_PAT = "your-access-token-here")
devtools::install_github("aifimmunology/H5weaver")
```
# Reading .h5 files

H5weaver allows for reading of the contents of an HDF5 file in multiple ways. To demonstrate, we'll use a file stored in the H5weaver package. You can obtain the path to this file using:
```
h5_file <- system.file("testdata/well1.h5", package = "H5weaver")
```

### Reading and separating out all contents

To read the entirety of an HDF5 file as a list object, use h5dump():
```
library(H5weaver)

h5_list <- h5dump(h5_file)
str(h5_list)
```
This is a very raw representation of the contents of these HDF5 files. You may want to convert the major components to a sparse matrix (for cell x gene counts), and a data.frame (for metadata):
```
h5_list <- h5_list_convert_to_dgCMatrix(h5_list,
                                        target = "matrix")
                                        
mat <- h5_list$matrix_dgCMatrix

feature_metadata <- as.data.frame(h5_list$matrix$features[-1])
```
Now, mat will consist of a dgCMatrix with genes as rows and barcodes as columns, and feature_metadata will be a data.frame with genes as rows and various metadata as columns. 

For this test dataset, there isn't any cell metadata. However, files that are generated by our pipeline will include a substantial metadata set stored in matrix/observations. This can be retrieved with:
```
cell_metadata <- cbind(data.frame(barcodes = h5_list$matrix$barcodes),
                       as.data.frame(h5_list$matrix$observations))
```

### Reading the matrix directly

There is also a convenience function to directly read the main cell x gene matrix from the HDF5 file, read_h5_dgCMatrix():

```
library(H5weaver)

mat <- read_h5_dgCMatrix(h5_file)
```

## Tests

Tests for `HTOparser` are implemented using the `testthat` testing suite:  
https://testthat.r-lib.org/

To run tests for this package, download the repository from Github and run `devtools::test()` in R from the base directory of the package.

Extra-stringent, CRAN-level package testing can be performed using `devtools::check()` in R.

## Style and Standards

This package aims to conform to the tidyverse style guide:  
https://style.tidyverse.org/index.html

General information about R package conventions can be found in `R Packages`:  
http://r-pkgs.had.co.nz/
