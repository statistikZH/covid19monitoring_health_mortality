#' @export
extract_data <- function(path, year) {
  svg <- xml_ns_strip(read_xml(path))

  # Somewhat unbased assumption: paths with a transform are either axis ticks
  # or the series we are after
  paths <- xml_find_all(svg, ".//g/path[@transform]")

  paths_parsed <- lapply(xml_attr(paths, "d"), parse_svg_path)
  transforms <- rbindlist(lapply(xml_attr(paths, "transform"), function(x) {
    data.table(
      x = parse(text = x)[[1]][[6]],
      y = parse(text = x)[[1]][[7]]
    )
  }))
  styles <- xml_attr(paths, "style")

  is_tick <- sapply(paths_parsed, nrow) == 2

  is_minitick <- sapply(paths_parsed, function(x) {
    x[1, x] - x[2, x] < 2 && x[1, y] - x[2, y] < 2 && nrow(x) == 2
  })

  is_vertical_tick <- sapply(paths_parsed, function(x) {
    nrow(x) == 2 && x[1, x] - x[2, x] < 2
  })

  paths_parsed <- paths_parsed[!(is_minitick | is_vertical_tick)]
  transforms <- transforms[!(is_minitick | is_vertical_tick)]
  styles <- styles[!(is_minitick | is_vertical_tick)]

  is_h_tick <- sapply(paths_parsed, nrow) == 2

  horizontal_ticks_trans <- transforms[is_h_tick]

  units_per_200 <- horizontal_ticks_trans[, diff(sort(y)[1:2])]
  zero <- horizontal_ticks_trans[, max(y) + units_per_200]

  series_candidates <- paths_parsed[!is_h_tick]
  candidate_transforms <- transforms[!is_h_tick]
  candidate_styles <- styles[!is_h_tick]

  candidate_colors <- gsub(".+(rgb\\(.+\\)).+", "\\1", candidate_styles)
  blues <- as.numeric(gsub(".+,([0-9.]+)%\\)", "\\1", candidate_colors))

  is_the_series <- blues > 80

  series_transforms <- candidate_transforms[is_the_series]
  series_paths <- series_candidates[is_the_series]

  series <- lapply(1:2, function(i) {
    y_abs <- walk_svg_path(series_paths[[i]], series_transforms[i, x], series_transforms[i, y])[, y]

    ook <- 200 * (y_abs - zero) / units_per_200
    floor(ook - 2*ook[1])
  })

  # Assume old people die more often than young ones
  series <- series[order(sapply(series, mean))]

  data.table(
    year = year,
    week = 1:52,
    under_65 = series[[1]],
    over_65 = series[[2]]
  )
}
