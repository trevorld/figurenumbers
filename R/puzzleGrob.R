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
#' The math problems are randomly generated using `seed` (via
#' [withr::with_seed()], so the caller's random number generator state
#' is left undisturbed).  The page is annotated in the bottom corner of
#' the number-grid panel with `hash` (if any) and the `seed` so an
#' exact page can be recreated later.
#'
#' @param segments A list of numeric vectors of numbers from 1 to 100.
#'                 The first element is the (hidden) picture sequence the
#'                 math problems are generated from; any later elements
#'                 are drawn in the number grid.
#' @param op,max_factor Passed to [generate_math_problems()].
#' @param seed Random seed used to generate the math problems (and
#'             included in the page annotation).  The default picks a
#'             random one.
#' @param hash A short identifier included in the page annotation, e.g.
#'             the `hash` element added by [read_puzzles()] (which
#'             identifies the puzzle without spoiling the picture), or
#'             `NULL` to omit.
#' @param problems_side Which panel the math problems go on: `"left"`
#'                      (default, so a right-handed kid drawing on the
#'                      grid doesn't cover their math solutions with
#'                      their writing hand) or `"right"` (likely
#'                      preferable for a left-handed kid).  The number
#'                      grid goes on the other panel.
#' @param margin Printer margin around each panel (a [grid::unit()]).
#' @param gp A [grid::gpar()] object.
#' @param name A character identifier (for the grob).
#' @param vp A [grid::viewport()] object (or `NULL`).
#' @return A [grid::gTree()] object.
#' @importFrom grid gList gpar gTree textGrob unit viewport vpStack
#' @examples
#' if (require("grid", quietly = TRUE)) {
#'     house <- c(63, 67, 37, 15, 33, 63)
#'     door <- c(65, 45, 46, 66)
#'     grid.newpage()
#'     grid.draw(puzzleGrob(list(house, door), seed = 42))
#' }
#' \dontrun{
#' # a landscape letter PDF ready to print and fold
#' pdf("puzzle.pdf", width = 11, height = 8.5)
#' grid::grid.draw(puzzleGrob(list(house, door), seed = 42))
#' dev.off()
#' }
#' @export
puzzleGrob <- function(
	segments,
	op = "auto",
	max_factor = 12L,
	seed = sample.int(1000000L, 1L),
	hash = NULL,
	problems_side = c("left", "right"),
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
	problems_side <- match.arg(problems_side)
	problems <- withr::with_seed(
		seed,
		generate_math_problems(segments[[1L]], op = op, max_factor = max_factor)
	)
	x_problems <- if (problems_side == "left") 0.25 else 0.75
	width <- unit(0.5, "npc") - 2 * margin
	height <- unit(1, "npc") - 2 * margin
	problems_panel <- problemsGrob(
		problems,
		name = "problems",
		vp = viewport(x = x_problems, width = width, height = height)
	)
	grid_panel <- numberGridGrob(
		segments = segments[-1L],
		name = "number_grid",
		vp = vpStack(
			viewport(x = 1 - x_problems, width = width, height = height),
			viewport(width = unit(1, "snpc"), height = unit(1, "snpc"))
		)
	)
	# annotate in the bottom outer corner of the (sparser) grid panel
	annotation <- textGrob(
		paste(c(hash, paste0("seed ", seed)), collapse = " "),
		x = if (problems_side == "left") unit(1, "npc") - margin else margin,
		y = margin,
		just = c(if (problems_side == "left") "right" else "left", "bottom"),
		gp = gpar(cex = 0.6),
		name = "annotation"
	)
	gTree(
		children = gList(problems_panel, grid_panel, annotation),
		gp = gp,
		name = name,
		vp = vp,
		cl = "puzzle_grob"
	)
}
