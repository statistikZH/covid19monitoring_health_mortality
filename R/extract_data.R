#' @export
extract_data <- function(path, year) {
  # Read SVG and strip namespaces (otherwise xml_find_all won't work)
  svg <- xml_ns_strip(read_xml(path))

  # Paths with a transform attached are part of the graphic
  # Those without are glyph definitions
  paths <- xml_find_all(svg, ".//g/path[@transform]")

  # Get data from paths and put into step-by-step instructions for ease of use
  paths_parsed <- lapply(xml_attr(paths, "d"), parse_svg_path)

  # Extract position of paths. Used to identify axis ticks and position of series
  transforms <- rbindlist(lapply(xml_attr(paths, "transform"), function(x) {
    data.table(
      x = parse(text = x)[[1]][[6]],
      y = parse(text = x)[[1]][[7]]
    )
  }))

  # Extract styles, we use the color to identify the series
  styles <- xml_attr(paths, "style")

  # Anything shorter than 2 units is either an axis tick or a data point
  # We don't need those
  is_minitick <- sapply(paths_parsed, function(x) {
    x[1, x] - x[2, x] < 2 && x[1, y] - x[2, y] < 2 && nrow(x) == 2
  })

  # ... neither the vertical gridlines
  is_vertical_tick <- sapply(paths_parsed, function(x) {
    nrow(x) == 2 && x[1, x] - x[2, x] < 2
  })

  paths_parsed <- paths_parsed[!(is_minitick | is_vertical_tick)]
  transforms <- transforms[!(is_minitick | is_vertical_tick)]
  styles <- styles[!(is_minitick | is_vertical_tick)]


  # Identify horizontal gridlines
  is_h_tick <- sapply(paths_parsed, nrow) == 2

  horizontal_ticks_trans <- transforms[is_h_tick]

  # Establish scale (the distance between two gridlines corresponds to 200 deaths)
  units_per_200 <- horizontal_ticks_trans[, diff(sort(y)[1:2])]

  # Establish baseline (there is no gridline at 0 so we use the one at 200 and add 1 tick)
  zero <- horizontal_ticks_trans[, max(y) + units_per_200]

  # Remove horizontal gridlines from candidates
  series_candidates <- paths_parsed[!is_h_tick]
  candidate_transforms <- transforms[!is_h_tick]
  candidate_styles <- styles[!is_h_tick]

  # Get blue  values
  candidate_colors <- gsub(".+(rgb\\(.+\\)).+", "\\1", candidate_styles)
  blues <- as.numeric(gsub(".+,([0-9.]+)%\\)", "\\1", candidate_colors))

  # Decition bonsai: Only the series paths have a blue value above 80
  is_the_series <- blues > 80

  series_transforms <- candidate_transforms[is_the_series]
  series_paths <- series_candidates[is_the_series]

  # Transform path instructions into absolute number of deaths
  series <- lapply(1:2, function(i) {

    y_abs <- walk_svg_path(series_paths[[i]],
                           series_transforms[i, x], # Position of origin
                           series_transforms[i, y])[,
                                                    y] # We only care about the y value

    # Transform graphic-space coordinates into count
    # The variable name tells us that 1) I like Pratchett and 2) I wrote this way past my bedtime ;P
    ook <- 200 * (y_abs - zero) / units_per_200

    # The svg y axis starts at the top. Shift the series up to account for that.
    floor(ook - 2*ook[1])
  })

  # Assume old people die more often than young ones
  series <- series[order(sapply(series, mean))]

  # Done
  data.table(
    year = year,
    week = 1:52,
    under_65 = series[[1]],
    over_65 = series[[2]]
  )
}
