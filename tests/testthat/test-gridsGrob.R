test_that("`gridsGrob()` picks sensible layout defaults", {
	grob <- gridsGrob()
	expect_s3_class(grob, "grids_grob")
	expect_identical(grid::childNames(grob), paste0("grid", 1:4))

	grob5 <- gridsGrob(5)
	expect_identical(grid::childNames(grob5), paste0("grid", 1:5))

	expect_error(gridsGrob(0), "positive whole number")
	expect_error(gridsGrob(5, ncol = 2, nrow = 2), "must fit")
})

test_that("`gridsGrob()` renders as expected", {
	skip_if_not_installed("vdiffr")
	library("grid")

	draw_grids <- function() {
		grid.newpage()
		grid.draw(gridsGrob())
	}
	vdiffr::expect_doppelganger("grids", draw_grids)
})
