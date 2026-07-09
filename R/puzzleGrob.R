#' Puzzle page grob
#'
#' `puzzleGrob()` creates a grid grob of a complete puzzle page:
#' arithmetic problems (see [generate_math_problems()]) on the left panel
#' and the number grid (see [numberGridGrob()]) on the right panel of a
#' landscape bifold page (i.e. a landscape sheet folded once vertically
#' into two facing panels, e.g. two 5.5" x 8.5" panels for letter paper).
#'
#' The first element of `segments` is the picture's point sequence: it is
#' used to generate one math problem per point (in order) but is *not*
#' drawn in the number grid -- connecting those points is the puzzle.
#' Any later elements of `segments` are drawn in the grid as pre-drawn
#' line segments.
#'
#' The math problems are randomly generated so use [set.seed()] first
#' for reproducible pages.
#'
#' @param segments A list of numeric vectors of numbers from 1 to 100.
#'                 The first element is the (hidden) picture sequence the
#'                 math problems are generated from; any later elements
#'                 are drawn in the number grid.
#' @param op,max_factor Passed to [generate_math_problems()].
#' @param margin Printer margin around each panel (a [grid::unit()]).
#' @param gp A [grid::gpar()] object.
#' @param name A character identifier (for the grob).
#' @param vp A [grid::viewport()] object (or `NULL`).
#' @return A [grid::gTree()] object.
#' @importFrom grid gList gpar gTree unit viewport vpStack
#' @examples
#' if (require("grid", quietly = TRUE)) {
#'     set.seed(42)
#'     house <- c(63, 67, 37, 15, 33, 63)
#'     door <- c(65, 45, 46, 66)
#'     grid.newpage()
#'     grid.draw(puzzleGrob(list(house, door)))
#' }
#' \dontrun{
#' # a landscape letter PDF ready to print and fold
#' pdf("puzzle.pdf", width = 11, height = 8.5)
#' grid::grid.draw(puzzleGrob(list(house, door)))
#' dev.off()
#' }
#' @export
puzzleGrob <- function(
	segments,
	op = "auto",
	max_factor = 12L,
	margin = unit(0.25, "in"),
	gp = gpar(col = "black", fill = "black", lwd = 2),
	name = NULL,
	vp = NULL
) {
	stopifnot(
		"`segments` must be a list of at least one numeric vector" = is.list(
			segments
		) &&
			length(segments) >= 1L
	)
	problems <- generate_math_problems(
		segments[[1L]],
		op = op,
		max_factor = max_factor
	)
	width <- unit(0.5, "npc") - 2 * margin
	height <- unit(1, "npc") - 2 * margin
	left <- problemsGrob(
		problems,
		name = "problems",
		vp = viewport(x = 0.25, width = width, height = height)
	)
	right <- numberGridGrob(
		segments = segments[-1L],
		name = "number_grid",
		vp = vpStack(
			viewport(x = 0.75, width = width, height = height),
			viewport(width = unit(1, "snpc"), height = unit(1, "snpc"))
		)
	)
	gTree(
		children = gList(left, right),
		gp = gp,
		name = name,
		vp = vp,
		cl = "puzzle_grob"
	)
}
