#' Read puzzles from a YAML file
#'
#' `read_puzzles()` reads puzzle definitions from a YAML file,
#' defaulting to the puzzles bundled with this package.
#'
#' Each puzzle is a list with a `label` (human-readable name, which may
#' spoil the picture; restricted to ASCII alphanumerics plus `_` and
#' `.` starting with a letter, since it is also used as the puzzle's
#' name in the returned list where such names are convenient to use),
#' `segments` (a list of point sequences suitable
#' for passing straight to [puzzleGrob()]: the first is the hidden
#' picture the math problems are generated from and any later ones are
#' pre-drawn in the number grid), and `credits` (image source
#' attribution, if any).  More fields may be added later.
#'
#' `read_puzzles()` additionally adds a `hash` element to each puzzle
#' (a short [digest::digest()] of its entire YAML entry, not stored in
#' the file) which can be used to annotate a puzzle without spoiling
#' the picture, e.g. to match a puzzle page to a solution/credits page.
#'
#' @param file Path to a YAML file of puzzle definitions.
#' @return A list of puzzles named by their labels (each puzzle a list
#'         with at least `label`, `segments`, and `hash` elements).
#' @examples
#' puzzles <- read_puzzles()
#' names(puzzles)
#' if (require("grid", quietly = TRUE)) {
#'     set.seed(42)
#'     grid.newpage()
#'     grid.draw(puzzleGrob(puzzles$letter_x$segments))
#' }
#' @export
read_puzzles <- function(
	file = system.file("puzzles.yaml", package = "figurenumbers", mustWork = TRUE)
) {
	puzzles <- yaml::read_yaml(file)
	labels <- vapply(
		puzzles,
		function(p) {
			if (is.character(p$label) && length(p$label) == 1L) {
				p$label
			} else {
				NA_character_
			}
		},
		character(1L)
	)
	stopifnot(
		"each puzzle must have a `label` of ASCII alphanumerics plus `_` and `.` (starting with a letter)" = !anyNA(
			labels
		) &&
			all(grepl("^[a-zA-Z][a-zA-Z0-9._]*$", labels)),
		"puzzle `label`s must be unique" = anyDuplicated(labels) == 0L,
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
	hashes <- vapply(
		puzzles,
		digest::digest,
		character(1L),
		algo = "crc32"
	)
	stopifnot("puzzle `hash`es must be unique" = anyDuplicated(hashes) == 0L)
	puzzles <- Map(function(p, hash) c(p, hash = hash), puzzles, hashes)
	names(puzzles) <- labels
	puzzles
}
