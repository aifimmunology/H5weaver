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
