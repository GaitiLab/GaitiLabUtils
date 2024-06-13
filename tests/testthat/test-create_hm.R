test_that("create simple heatmap (no annotation)", {
  # Create matrix
  mat <- matrix(runif(25), nrow = 5)

  hm <- create_hm(mat)
  expect_type(hm, "S4")
})


test_that("create simple heatmap + no annot + save", {
  output_file <- file.path(tempdir(), "test-heatmap-without-annot.pdf")

  # Create matrix
  mat <- matrix(runif(25), nrow = 5)
  # Create heatmap
  hm <- create_hm(mat)
  # Save heatmap
  save_hm(hm, output_file = output_file)
  # Check if heatmap is created
  expect_true(file.exists(output_file), output_file)

  if (interactive()) system(paste0("open ", output_file))
})


test_that("create simple heatmap + annot", {
  # Create matrix
  mat <- matrix(runif(25), nrow = 5)

  cell_func <- get_cell_function(matrix = round(mat, 2), is_upper_tri = FALSE, add_annot = TRUE)

  hm <- create_hm(matrix = mat, hm_cell_function = cell_func)

  expect_type(hm, "S4")
})


test_that("create simple heatmap + annot + save", {
  output_file <- file.path(tempdir(), "test-heatmap-with-annot.pdf")

  # Create matrix
  mat <- matrix(runif(25), nrow = 5)

  cell_func <- get_cell_function(matrix = round(mat, 2), is_upper_tri = FALSE, add_annot = TRUE)


  hm <- create_hm(matrix = mat, hm_cell_function = cell_func)

  # Save heatmap
  save_hm(hm, output_file = output_file)
  # Check if heatmap is created
  expect_true(file.exists(output_file), output_file)

  if (interactive()) system(paste0("open ", output_file))
})


test_that("create simple heatmap + different annot + save", {
  output_file <- file.path(tempdir(), "test-heatmap-with-diff-annot.pdf")

  # Create matrix
  mat <- matrix(runif(25), nrow = 5)
  annot_mat <- matrix(rep(1, 25), nrow = 5)

  cell_func <- get_cell_function(matrix = annot_mat, is_upper_tri = FALSE, add_annot = TRUE)
  hm <- create_hm(matrix = mat, hm_cell_function = cell_func)

  # Save heatmap
  save_hm(hm, output_file = output_file)
  # Check if heatmap is created
  expect_true(file.exists(output_file), output_file)

  if (interactive()) system(paste0("open ", output_file))
})



test_that("create simple heatmap + is_upper_tri", {
  output_file <- file.path(tempdir(), "test-heatmap-without-annot-upper-tri.pdf")

  # Create matrix
  mat <- matrix(runif(25), nrow = 5)
  annot_mat <- matrix(rep(1, 25), nrow = 5)

  cell_func <- get_cell_function(matrix = annot_mat, is_upper_tri = FALSE)
  hm <- create_hm(mat = mat, is_upper_tri = TRUE)
  hm
  # Save heatmap
  save_hm(hm, output_file = output_file)
  # Check if heatmap is created
  expect_true(file.exists(output_file), output_file)

  if (interactive()) system(paste0("open ", output_file))
})
