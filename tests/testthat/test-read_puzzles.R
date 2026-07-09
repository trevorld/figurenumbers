test_that("`read_puzzles()` reads the bundled puzzles", {
	puzzles <- read_puzzles()
	expect_gte(length(puzzles), 1L)
	labels <- vapply(puzzles, `[[`, character(1L), "label", USE.NAMES = FALSE)
	expect_named(puzzles, labels)
	expect_true(all(grepl("^[a-zA-Z][a-zA-Z0-9._]*$", labels)))

	hashes <- vapply(puzzles, `[[`, character(1L), "hash", USE.NAMES = FALSE)
	expect_true(all(grepl("^[0-9a-f]{8}$", hashes)))
	expect_identical(anyDuplicated(hashes), 0L)
	# hashes are deterministic
	expect_identical(hashes, vapply(read_puzzles(), `[[`, character(1L), "hash", USE.NAMES = FALSE))
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

	x_serif <- read_puzzles()$letter_x$segments[[1L]]
	draw_letter_x <- function() {
		grid.newpage()
		vp <- viewport(width = unit(1, "snpc"), height = unit(1, "snpc"))
		grid.draw(numberGridGrob(segments = list(x_serif), vp = vp))
	}
	vdiffr::expect_doppelganger("letter_x", draw_letter_x)
})
