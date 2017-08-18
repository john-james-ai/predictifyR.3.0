context("Platform Class Test")

testthat::test_that("Test Initialize Validation Input", {

  # Validate Input
  testthat::expect_error(Platform$new('ada'))
  testthat::expect_error(Platform$new(9))
  testthat::expect_error(Platform$new('predictifyR'))


})

testthat::test_that("Test Create Environment", {

  latenite <- Platform$new('latenite', 'late nite Rin')
  testthat::expect_identical(latenite$pName, 'latenite')
  testthat::expect_identical(latenite$pDesc, 'late nite Rin')
  testthat::expect_identical(latenite$pPath, 'latenite')


})

testthat::test_that("Test Archive Environment", {

})
get('env', inherits = TRUE, envir = .pkgGlobalEnv)
