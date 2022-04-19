context("gdal_datatype()")

test_that("correct conversion", {
  expect_equal(gdal_datatype("INT1U"), "Byte")
  expect_equal(gdal_datatype("INT2S"), "Int16")
  expect_equal(gdal_datatype("INT4S"), "Int32")
  expect_equal(gdal_datatype("FLT8S"), "Float64")
  expect_equal(gdal_datatype("INT2U"), "UInt16")
  expect_equal(gdal_datatype("INT4U"), "UInt32")
  expect_equal(gdal_datatype("FLT4S"), "Float32")
  expect_error(gdal_datatype("asd"))
})
