test_that("`puzzleGrob()` hides the first element and draws the rest", {
	house <- c(63, 67, 37, 15, 33, 63)
	door <- c(65, 45, 46, 66)

	grob <- puzzleGrob(list(house))
	expect_s3_class(grob, "puzzle_grob")
	expect_false("segments" %in% grid::childNames(grob$children$number_grid))

	grob <- puzzleGrob(list(house, door))
	expect_true("segments" %in% grid::childNames(grob$children$number_grid))

	expect_error(puzzleGrob(house), "must be a list")
	expect_error(puzzleGrob(list()), "at least one")
})

test_that("`puzzleGrob()` renders as expected", {
	skip_if_not_installed("vdiffr")
	library("grid")

	draw_puzzle <- function() {
		set.seed(42)
		house <- c(63, 67, 37, 15, 33, 63)
		door <- c(65, 45, 46, 66)
		grid.newpage()
		grid.draw(puzzleGrob(list(house, door)))
	}
	vdiffr::expect_doppelganger("puzzle", draw_puzzle)
})
