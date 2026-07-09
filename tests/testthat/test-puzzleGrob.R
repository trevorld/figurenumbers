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

test_that("`problems_side` controls which panel the problems go on", {
	house <- c(63, 67, 37, 15, 33, 63)
	problems_x <- function(grob) as.numeric(grob$children$problems$vp$x)

	expect_equal(problems_x(puzzleGrob(list(house))), 0.25)
	expect_equal(problems_x(puzzleGrob(list(house), problems_side = "right")), 0.75)
	expect_error(puzzleGrob(list(house), problems_side = "top"), "arg")
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

	draw_puzzle_lefty <- function() {
		set.seed(42)
		house <- c(63, 67, 37, 15, 33, 63)
		door <- c(65, 45, 46, 66)
		grid.newpage()
		grid.draw(puzzleGrob(list(house, door), problems_side = "right"))
	}
	vdiffr::expect_doppelganger("puzzle_lefty", draw_puzzle_lefty)
})
