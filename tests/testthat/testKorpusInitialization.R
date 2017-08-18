context("Korpus Initialization")

testthat::test_that("Initialize: Validate korpus user inputs", {

  # #Test required parameters exist
  testthat::expect_error(Korpus$new())
  # testthat::expect_error(Korpus$new('predictifyR'))

  # # Test platform
  # testthat::expect_error(Korpus$new(2, 'ba', 'Raw Corpus'))
  # testthat::expect_error(Korpus$new(TRUE, 'bb', 'Raw Corpus'))
  # testthat::expect_error(Korpus$new('x', 'bc', 'Raw Corpus'))
  # testthat::expect_error(Korpus$new(x, 'bd', 'Raw Corpus'))
  #
  # # Test korpus name
  # testthat::expect_error(Korpus$new('predictifyR', 2, 'Raw Corpus'))
  # testthat::expect_error(Korpus$new('predictifyR', FALSE, 'Raw Corpus'))
  # testthat::expect_error(Korpus$new('predictifyR', '', 'Raw Corpus'))

})

testthat::test_that("Initialize: Validate korpus output", {

  #korpus <- Korpus$new('predictifyR', 'korpus', 'some wacked corpus')

  # testthat::expect_identical(korpus$platform, 'predictifyR')
  # testthat::expect_identical(korpus$korpusName, 'korpus')
  # testthat::expect_identical(korpus$korpusDesc, 'some wacked corpus')
  # testthat::expect_identical(korpus$korpusPath,
  #                            gsub(pattern = " ", replacement = "-", 'korpus'))
})
