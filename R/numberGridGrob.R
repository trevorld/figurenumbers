#' Number grid grob
#'
#' `numberGridGrob()` creates a grid grob of a 10x10 grid of (small) circles
#' representing the numbers 1 to 100 (in reading order).
#' The left side is labelled with the numbers 1, 11, ..., 91 (right-aligned),
#' the right side with the numbers 10, 20, ..., 100 (right-aligned),
#' and the top with the numbers 1, 2, ..., 10.
#' It is intended to be drawn within a square viewport.
#'
#' @param segments A list of numeric vectors of numbers from 1 to 100.
#'                 For each numeric vector we'll draw line segments
#'                 between each (consecutive) number in that vector
#'                 (as mapped to the points in the number grid).
#' @param r Radius of the circles.
#' @param gp A [grid::gpar()] object.
#' @param name A character identifier (for the grob).
#' @param vp A [grid::viewport()] object (or `NULL`).
#' @return A [grid::gTree()] object.
#' @importFrom grid circleGrob gList gpar gTree segmentsGrob textGrob unit
#' @examples
#' if (require("grid", quietly = TRUE)) {
#'     grid.newpage()
#'     vp <- viewport(width = unit(1, "snpc"), height = unit(1, "snpc"))
#'     grid.draw(numberGridGrob(vp = vp))
#' }
#' if (require("grid", quietly = TRUE)) {
#'     grid.newpage()
#'     puzzle <- read_puzzles()$letter_x
#'     vp <- viewport(width = unit(1, "snpc"), height = unit(1, "snpc"))
#'     grid.draw(numberGridGrob(segments = puzzle$segments, vp = vp))
#' }
#' @export
numberGridGrob <- function(
	segments = list(),
	r = unit(0.005, "snpc"),
	gp = gpar(col = "black", fill = "black", lwd = 2),
	name = NULL,
	vp = NULL
) {
	stopifnot("`segments` must be a list" = is.list(segments))

	# 10x10 grid of circles within [0.1, 0.9] x [0.1, 0.9]
	# leaving margins for the labels
	cell <- 0.08
	x_col <- 0.1 + (seq.int(10L) - 0.5) * cell
	y_row <- 0.9 - (seq.int(10L) - 0.5) * cell

	circles <- circleGrob(
		x = rep(x_col, 10L),
		y = rep(y_row, each = 10L),
		r = r,
		name = "circles"
	)
	left <- textGrob(
		seq.int(1L, 91L, by = 10L),
		x = 0.08,
		y = y_row,
		just = "right",
		name = "left_labels"
	)
	right <- textGrob(
		seq.int(10L, 100L, by = 10L),
		x = 0.95,
		y = y_row,
		just = "right",
		name = "right_labels"
	)
	top <- textGrob(
		seq.int(10L),
		x = x_col,
		y = 0.92,
		just = "bottom",
		name = "top_labels"
	)

	# map the numbers 1 to 100 (in reading order) to grid coordinates
	number2x <- function(n) x_col[(n - 1) %% 10 + 1]
	number2y <- function(n) y_row[(n - 1) %/% 10 + 1]
	from <- unlist(lapply(segments, function(n) n[-length(n)]))
	to <- unlist(lapply(segments, function(n) n[-1L]))
	children <- gList(circles, left, right, top)
	if (length(from)) {
		lines <- segmentsGrob(
			x0 = number2x(from),
			y0 = number2y(from),
			x1 = number2x(to),
			y1 = number2y(to),
			name = "segments"
		)
		children <- gList(lines, circles, left, right, top)
	}

	gTree(
		children = children,
		gp = gp,
		name = name,
		vp = vp,
		cl = "number_grid_grob"
	)
}
