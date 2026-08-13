#' Multiple number grids grob
#'
#' `gridsGrob()` arranges several blank [numberGridGrob()] panels in
#' reading order in a row/column grid filling a `[0, 1] x [0, 1]` viewport,
#' inset by `margin`.  Intended for printing practice sheets of blank number
#' grids to hand-sketch candidate puzzle pictures on before encoding them as
#' `segments` in `inst/puzzles.yaml`.
#'
#' @param n Number of number grids to draw.
#' @param ncol Number of columns of grids.  The default (`NULL`) arranges
#'             the grids as close to a square as possible (`ceiling(sqrt(n))`
#'             columns), e.g. 2x2 for 4 grids.
#' @param nrow Number of rows of grids.  The default (`NULL`) picks enough
#'             rows to fit `n` grids into `ncol` columns.
#' @param margin Printer margin around the whole arrangement (a
#'               [grid::unit()]).
#' @param r Radius of the circles in each grid, passed to [numberGridGrob()].
#' @param gp A [grid::gpar()] object.
#' @param name A character identifier (for the grob).
#' @param vp A [grid::viewport()] object (or `NULL`).
#' @return A [grid::gTree()] object.
#' @importFrom grid gList gpar gTree unit viewport vpStack
#' @examples
#' # a two-sided landscape letter PDF of blank grids ready to print (duplex)
#' # for hand-sketching candidate puzzle pictures
#' if (isTRUE(capabilities("cairo")) && require("grid")) {
#'     f <- tempfile(fileext = ".pdf")
#'     cairo_pdf(f, width = 11, height = 8.5) # landscape letter
#'     grid.draw(gridsGrob())
#'     grid.newpage()
#'     grid.draw(gridsGrob())
#'     invisible(dev.off())
#'     unlink(f)
#' }
#' @export
gridsGrob <- function(
	n = 4L,
	ncol = NULL,
	nrow = NULL,
	margin = unit(0.25, "in"),
	r = unit(0.005, "snpc"),
	gp = gpar(col = "black", fill = "black", lwd = 2),
	name = NULL,
	vp = NULL
) {
	stopifnot(
		"`n` must be a single positive whole number" = is.numeric(n) &&
			length(n) == 1L &&
			n >= 1L
	)
	n <- as.integer(n)
	if (is.null(ncol)) {
		ncol <- ceiling(sqrt(n))
	}
	if (is.null(nrow)) {
		nrow <- ceiling(n / ncol)
	}
	stopifnot("`ncol` times `nrow` must fit all `n` grids" = ncol * nrow >= n)

	# cell (column, row) of each grid, in reading order
	cw <- 1 / ncol
	ch <- 1 / nrow
	i <- seq_len(n)
	x_center <- ((i - 1L) %% ncol + 0.5) * cw
	y_center <- 1 - ((i - 1L) %/% ncol + 0.5) * ch

	content <- viewport(
		width = unit(1, "npc") - 2 * margin,
		height = unit(1, "npc") - 2 * margin
	)
	grids <- lapply(i, function(k) {
		numberGridGrob(
			r = r,
			name = paste0("grid", k),
			vp = vpStack(
				content,
				viewport(x = x_center[k], y = y_center[k], width = cw, height = ch),
				viewport(width = unit(1, "snpc"), height = unit(1, "snpc"))
			)
		)
	})

	gTree(
		children = do.call(gList, grids),
		gp = gp,
		name = name,
		vp = vp,
		cl = "grids_grob"
	)
}
