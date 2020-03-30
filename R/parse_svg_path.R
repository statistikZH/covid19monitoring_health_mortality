# Overkill? coitenly!
# cf. https://www.w3.org/TR/SVG/paths.html
#' @export
parse_svg_path <- function(p) {
  commands <- c("m", "M", "l", "L", "v", "V", "h", "H") # And then some more complicated ones

  # Also matches behind the letters but meh. regex101 says I'm right
  parts <- strsplit(p, sprintf("(?=[%s])", paste(commands, collapse = "")), perl = TRUE)[[1]]

  commands <- parts[seq(1, length(parts), 2)]
  args <- parts[seq(2, length(parts), 2)]

  rbindlist(lapply(seq_along(commands), function(i) {
    command <- commands[i]
    arg <- strsplit(trimws(args[[i]]), " ")[[1]]
    switch(tolower(command),
           "m" = {
             numbers <- t(sapply(strsplit(arg, ","), as.numeric))
             data.table(
               command = c("moveto", rep("lineto", nrow(numbers) - 1)),
               relative = if(i == 1 && command == "m") { c(FALSE, rep(TRUE, length(numbers) - 1)) } else { command == "m" },
               x = numbers[, 1],
               y = numbers[, 2]
             )
           },
           "l" = {
             numbers <- t(sapply(strsplit(arg, ","), as.numeric))
             data.table(
               command = "lineto",
               relative = command == "l",
               x = numbers[, 1],
               y = numbers[, 2]
             )
           },
           "v" = {
             numbers <- as.numeric(arg)
             data.table(
               command = "vertical",
               relative = command == "v",
               x = NA,
               y = numbers
             )
           },
           "h" = {
             numbers <- as.numeric(arg)
             data.table(
               command = "horizontal",
               relative = command == "h",
               x = numbers,
               y = NA
             )
           },
           "z" = {
             data.table(
               command = "closepath",
               relative = FALSE,
               x = NA,
               y = NA
             )
           })
  }))
}
