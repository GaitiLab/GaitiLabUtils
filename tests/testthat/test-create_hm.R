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

  hm <- create_hm(matrix = mat, cell_fun = cell_func)

  expect_type(hm, "S4")
})


test_that("create simple heatmap + annot + save", {
  output_file <- file.path(tempdir(), "test-heatmap-with-annot.pdf")

  # Create matrix
  mat <- matrix(runif(25), nrow = 5)

  cell_func <- get_cell_function(matrix = round(mat, 2), is_upper_tri = FALSE, add_annot = TRUE)


  hm <- create_hm(matrix = mat, cell_fun = cell_func)
  draw(hm)
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
  hm <- create_hm(matrix = mat, cell_fun = cell_func)

  # Save heatmap
  save_hm(hm, output_file = output_file)
  # Check if heatmap is created
  expect_true(file.exists(output_file), output_file)

  if (interactive()) system(paste0("open ", output_file))
})



test_that("create simple heatmap + is_upper_tri + annot", {
  output_file <- file.path(tempdir(), "test-heatmap-with-annot-upper-tri.pdf")

  # Create matrix
  mat <- matrix(runif(25), nrow = 5)
  annot_mat <- matrix(rep(1, 25), nrow = 5)

  # Params
  is_upper_tri <- TRUE
  add_annot <- TRUE

  cell_func <- get_cell_function(
    matrix = annot_mat,
    is_upper_tri = is_upper_tri,
    add_annot = add_annot
  )
  hm <- create_hm(
    mat = mat,
    cell_fun = cell_func,
    is_full = !is_upper_tri
  )

  # Save heatmap
  save_hm(hm, output_file = output_file)
  # Check if heatmap is created
  expect_true(file.exists(output_file), output_file)

  if (interactive()) system(paste0("open ", output_file))
})


test_that("create simple heatmap + is_upper_tri + no annot", {
  output_file <- file.path(tempdir(), "test-heatmap-without-annot-upper-tri.pdf")

  # Create matrix
  mat <- matrix(runif(25), nrow = 5)
  annot_mat <- matrix(rep(1, 25), nrow = 5)

  # Params
  is_upper_tri <- TRUE
  add_annot <- FALSE

  cell_func <- get_cell_function(
    matrix = annot_mat,
    is_upper_tri = is_upper_tri,
    add_annot = add_annot
  )
  hm <- create_hm(
    mat = mat,
    cell_fun = cell_func,
    is_full = !is_upper_tri
  )

  # Save heatmap
  save_hm(hm, output_file = output_file)
  # Check if heatmap is created
  expect_true(file.exists(output_file), output_file)

  if (interactive()) system(paste0("open ", output_file))
})


test_that("create simple heatmap + no annot + horiz_leg + hm_legend_side_bottom + save", {
  output_file <- file.path(tempdir(), "test-heatmap-m_legend_side_bottom.pdf")

  # Create matrix
  mat <- matrix(runif(25), nrow = 5)

  color_fun <- circlize::colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))


  # Create heatmap
  hm <- create_hm(mat, heatmap_legend_param = list(direction = "horizontal"))

  # Save heatmap
  save_hm(hm, output_file = output_file, heatmap_legend_side = "bottom")
  # Check if heatmap is created
  expect_true(file.exists(output_file), output_file)

  if (interactive()) system(paste0("open ", output_file))
})


test_that("create simple heatmap + no annot + hm_annot_col + pos_bottom", {
  output_file <- file.path(tempdir(), "test-heatmap-hm_annot_col-pos_bottom.pdf")

  # Create matrix
  mat_size <- 20
  mat <- matrix(runif(mat_size**2), nrow = mat_size)

  # Add top-annotation
  ha <- ComplexHeatmap::HeatmapAnnotation(
    group = sample(letters[1:3], mat_size, replace = TRUE),
    annotation_legend_param = list(
      group = list(nrow = 1)
    )
  )

  # Create heatmap
  hm <- create_hm(mat,
    top_annotation = ha,
    heatmap_legend_param = list(
      direction = "horizontal",
      legend_width = unit(9, "lines"),
      legend_height = unit(.9, "lines")
    )
  )

  # Save heatmap
  save_hm(hm,
    output_file = output_file,
    merge_legend = TRUE,
    annotation_legend_side = "bottom",
    heatmap_legend_side = "bottom"
  )
  # Check if heatmap is created
  expect_true(file.exists(output_file), output_file)

  if (interactive()) system(paste0("open ", output_file))
})


test_that("create simple heatmap + no annot + color_fun + save", {
  output_file <- file.path(tempdir(), "test-heatmap-col_fun.pdf")

  # Create matrix
  mat <- matrix(runif(25), nrow = 5)

  color_fun <- circlize::colorRamp2(c(0, 0.5, 1), c("green", "white", "yellow"))

  # Create heatmap
  hm <- create_hm(mat, col = color_fun)

  # Save heatmap
  save_hm(hm, output_file = output_file)
  # Check if heatmap is created
  expect_true(file.exists(output_file), output_file)

  if (interactive()) system(paste0("open ", output_file))
})
