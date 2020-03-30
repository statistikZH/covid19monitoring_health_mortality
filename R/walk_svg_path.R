#' @export
walk_svg_path <- function(path, x0 = 0, y0 = 0) {
  coords <- path[, .(x, y)]
  if(path[, sum(command == "moveto") > 1]) {
    stop("You seem to be attempting to walk a multi-segment path. Nope!")
  }

  # h and v produce NAs which should be treated as 0
  coords[is.na(coords)] <- 0

  for(i in 1:nrow(path)) {
    step <- path[i, ]
    if(step$relative) {
      # First step is always interpreted as absolute so no risk here
      coords[i, x := x + coords[i-1, x]]
      coords[i, y := y + coords[i-1, y]]
    }
  }

  # Shift origin
  coords[, x := x + x0]
  coords[, y := y + y0]
  coords
}
