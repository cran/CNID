context("test CNID")

test_that("Obtain information based on Chinese ID number", {

  # Example
  id = c(
    "652801197305161555", 
    "110101841125178"
  )
  cnid_info(id)

})
