compare_bitmaps <- function(file1, file2, width = NULL, height = NULL, tolerance = 1,
                            compare_fun = compare_abs_difference) {
  imgA <- rsvg::rsvg_raw(svg = file1, width = width, height = height)
  imgB <- rsvg::rsvg_raw(svg = file2, width = width, height = height)
  stopifnot(identical(dim(imgA), dim(imgB)))

  diff <- compare_fun(imgA, imgB)
  return(diff <= tolerance)
}

compare_abs_difference <- function(imgA, imgB) {
  # 255 is inherited from rsvg::rsvg
  sum(abs(as.numeric(imgA) - as.numeric(imgB))) / 255
}