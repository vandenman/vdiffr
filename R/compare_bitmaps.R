compare_bitmaps <- function(file1, file2, width = NULL, height = NULL, tolerance = 1) {
  imgA <- rsvg::rsvg_raw(svg = file1, width = width, height = height)
  imgB <- rsvg::rsvg_raw(svg = file2, width = width, height = height)
  stopifnot(identical(dim(imgA), dim(imgB)))

  # 255 is inherited from rsvg::rsvg
  diff <- sum(abs(as.numeric(imgA) - as.numeric(imgB))) / 255
  return(diff <= tolerance)
}
