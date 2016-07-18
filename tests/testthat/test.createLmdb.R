library(caffeR)
context("Script for creating lmdb file")

test_that("create script performs correct replacements", {
  script <- generateCreateLmdb(caffedir = "~/Documents/caffe", name = "MyModel",
                               caffe_preprocessing = TRUE,
                               resize_height = 100, resize_width = 100)

  expect_match(script, "~/Documents/caffe/examples/MyModel")
})
