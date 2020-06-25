compare_bitmaps <- function(path1, path2, width = NULL, height = NULL, tolerance = 1,
                            compare_fun = compare_abs_difference) {

  diff <- compare_fun(path1, path2, width, height)
  return(diff <= tolerance)
}

compare_abs_difference <- function(path1, path2, width, height) {

  imgA <- rsvg::rsvg_raw(svg = path1, width = width, height = height)
  imgB <- rsvg::rsvg_raw(svg = path2, width = width, height = height)
  stopifnot(identical(dim(imgA), dim(imgB)))

  # 255 is inherited from rsvg::rsvg
  sum(abs(as.numeric(imgA) - as.numeric(imgB))) / 255
}
