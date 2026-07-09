test_that("`read_puzzles()` reads the bundled puzzles", {
	puzzles <- read_puzzles()
	expect_gte(length(puzzles), 1L)
	for (puzzle in puzzles) {
		expect_type(puzzle$label, "character")
		expect_type(puzzle$segments, "list")
		points <- unlist(puzzle$segments)
		expect_true(all(points >= 1 & points <= 100))
		# every puzzle should make a valid page
		set.seed(42)
		expect_s3_class(puzzleGrob(puzzle$segments), "puzzle_grob")
	}
})

test_that("the letter X puzzle renders as expected", {
	skip_if_not_installed("vdiffr")
	library("grid")

	x_serif <- read_puzzles()[[1L]]$segments[[1L]]
	draw_letter_x <- function() {
		grid.newpage()
		vp <- viewport(width = unit(1, "snpc"), height = unit(1, "snpc"))
		grid.draw(numberGridGrob(segments = list(x_serif), vp = vp))
	}
	vdiffr::expect_doppelganger("letter_x", draw_letter_x)
})
