test_that("`numberGridGrob()` renders as expected", {
	skip_if_not_installed("vdiffr")
	library("grid")
	draw_number_grid <- function() {
		grid.newpage()
		vp <- viewport(width = unit(1, "snpc"), height = unit(1, "snpc"))
		grid.draw(numberGridGrob(vp = vp))
	}
	vdiffr::expect_doppelganger("number_grid", draw_number_grid)
})
