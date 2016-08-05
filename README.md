
<!-- README.md is generated from README.Rmd. Please edit that file -->
caffeR: a wrapper for 'caffe'
============================

[![Build Status](https://travis-ci.org/cnaumzik/caffeR.svg?branch=master)](https://travis-ci.org/cnaumzik/caffeR) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/caffeR)](https://cran.r-project.org/package=caffeR) [![Coverage Status](https://img.shields.io/codecov/c/github/cnaumzik/caffeR/master.svg)](https://codecov.io/github/cnaumzik/caffeR?branch=master)

**CaffeR** offers a range of wrapper functions for the deep learning framework  **Caffe** by the BVLC. Currently the package allows to prepare image data in either LMDB or HDF5 in a way that it can be processes by the caffe framework. In addition the package allows to set up the required .prototxt files to finetune an existing model. 

Overview
--------

The most important functions in **caffeR** are:

-   ...

Installation
------------

Using the **devtools** package, you can easily install the latest development version of **caffeR** with

``` r
install.packages("devtools")

# Option 1: download and install latest version from ‘GitHub’
devtools::install_github("cnaumzik/caffeR")

# Option 2: install directly from bundled archive
# devtoos::install_local("caffeR_0.2-0.tar.gz")
```

Notes:

-   In the case of option 2, you have to specify the path either to the directory of **caffeR** or to the bundled archive **caffeR_0.2-0.tar.gz**

-   A CRAN version has not yet been released.
