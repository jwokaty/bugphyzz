
# Setup -------------------------------------------------------------------

library(purrr)

expected_columns_multistate <- c(
  NCBI_ID = "integer", Taxon_name = "character",
  Rank = "character",
  Attribute = "character", Attribute_value = "character",
  Evidence = "character",
  Frequency = "character", Score = "double", Attribute_source = "character",
  Confidence_in_curation = "character", Attribute_type = "character",
  Validation = "double"
)
expected_columns_binary <- c(
  NCBI_ID = "integer", Taxon_name = "character",
  Rank = "character",
  Attribute = "character", Attribute_value = "logical",
  Evidence = "character",
  Frequency = "character", Score = "double", Attribute_source = "character",
  Confidence_in_curation = "character", Attribute_type = "character",
  Validation = "double"
)
expected_columns_numeric <- c(
  NCBI_ID = "integer", Taxon_name = "character",
  Rank = "character",
  Attribute = "character", Attribute_value = "double",
  Evidence = "character",
  Frequency = "character", Score = "double", Attribute_source = "character",
  Confidence_in_curation = "character", Attribute_type = "character",
  NSTI = "double", Validation = "double"
)

checkColumnNames <- function(x) {
  attr_type <- unique(x$Attribute_type)
  if (attr_type == "multistate-intersection" | attr_type == "multistate-union") {
    lgl_vct <- colnames(x) == names(expected_columns_multistate)
  } else if (attr_type == "binary") {
    lgl_vct <- colnames(x) == names(expected_columns_binary)
  } else if (attr_type == "numeric") {
    lgl_vct <- colnames(x) == names(expected_columns_numeric)
  }
  return(all(lgl_vct))
}

checkColumnTypes <- function(x) {
  attr_type <- unique(x$Attribute_type)
  if ("Validation" %in% colnames(x)) {
    x$Validation <- as.double(x$Validation)
  }
  if ("NSTI" %in% colnames(x)) {
    x$NSTI <- as.double(x$NSTI)
  }
  types <- map_chr(x, typeof)
  if (attr_type == "multistate-intersection" | attr_type == "multistate-union") {
    lgl_vct <- types == expected_columns_multistate
  } else if (attr_type == "binary") {
    lgl_vct <- types == expected_columns_binary
  } else if (attr_type == "numeric") {
    lgl_vct <- types == expected_columns_numeric
  }
  return(all(lgl_vct))
}

checkNAs <- function(x) {
  ## TODO
  ## Some columns should never have NAs
  select_col <- c(
    "NCBI_ID",
    "Taxon_name",
    "Rank",
    "Attribute",
    "Attribute_value",
    "Attribute_type",
    "Evidence",
    "Frequency"
    # "Score"
    ## Regarding numeric attributes, the frequency used was "unknown"
    ## Maybe I should assign a minimal score to these ones as well
  )
  x <- x[,select_col]
  !any(purrr::map_lgl(x, ~ any(is.na(.x))))
}

checkCuration <- function(x) {
  ## TODO
  ## Only allowed keywords are included in the final output
  x
}

# tests -------------------------------------------------------------------

test_that("importBugphyzz works with devel", {
  bp <- importBugphyzz(version = "devel", force_download = TRUE)
  expect_true(all("data.frame" == map_chr(bp, class)))
  expect_true(all(map_lgl(bp, ~ nrow(.x) > 0)))
  expect_true(all(map_lgl(bp, checkColumnNames)))
  expect_true(all(map_lgl(bp, checkColumnTypes)))
  expect_true(all(map_lgl(bp, checkNAs)))

})

test_that("importBugphyzz works with hash", {
  bp <- importBugphyzz(version = "d3fd894", force_download = TRUE)
  expect_true(all("data.frame" == map_chr(bp, class)))
  expect_true(all(map_lgl(bp, ~ nrow(.x) > 0)))
  expect_true(all(map_lgl(bp, checkColumnNames)))
  expect_true(all(map_lgl(bp, checkColumnTypes)))
  expect_true(all(map_lgl(bp, checkNAs)))
})

## TODO create test for using Zenodo
test_that("importBugphyzz doesn't work with other words", {
  expect_error(importBugphyzz(version = "abcd-1234", force_download = TRUE))
})
