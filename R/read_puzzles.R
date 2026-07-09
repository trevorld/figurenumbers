#' Read puzzles from a YAML file
#'
#' `read_puzzles()` reads puzzle definitions from a YAML file,
#' defaulting to the puzzles bundled with this package.
#'
#' Each puzzle is a list with a `label` (human-readable name, which may
#' spoil the picture), `segments` (a list of point sequences suitable
#' for passing straight to [puzzleGrob()]: the first is the hidden
#' picture the math problems are generated from and any later ones are
#' pre-drawn in the number grid), and `credits` (image source
#' attribution, if any).  More fields may be added later.
#'
#' @param file Path to a YAML file of puzzle definitions.
#' @return A list of puzzles (each a list with at least `label` and
#'         `segments` elements).
#' @examples
#' puzzles <- read_puzzles()
#' vapply(puzzles, `[[`, character(1L), "label")
#' if (require("grid", quietly = TRUE)) {
#'     set.seed(42)
#'     grid.newpage()
#'     grid.draw(puzzleGrob(puzzles[[1L]]$segments))
#' }
#' @export
read_puzzles <- function(
	file = system.file("puzzles.yaml", package = "figurenumbers", mustWork = TRUE)
) {
	puzzles <- yaml::read_yaml(file)
	stopifnot(
		"each puzzle must have a `label`" = all(vapply(
			puzzles,
			function(p) is.character(p$label) && length(p$label) == 1L,
			logical(1L)
		)),
		"each puzzle must have a list of `segments`" = all(vapply(
			puzzles,
			function(p) is.list(p$segments) && length(p$segments) >= 1L,
			logical(1L)
		)),
		"`segments` values must be whole numbers between 1 and 100" = all(vapply(
			puzzles,
			function(p) {
				points <- unlist(p$segments)
				all(points == as.integer(points) & points >= 1 & points <= 100)
			},
			logical(1L)
		))
	)
	puzzles
}
