test_that("`numberGridGrob()` renders as expected", {
	skip_if_not_installed("vdiffr")
	library("grid")
	draw_number_grid <- function() {
		grid.newpage()
		vp <- viewport(width = unit(1, "snpc"), height = unit(1, "snpc"))
		grid.draw(numberGridGrob(vp = vp))
	}
	vdiffr::expect_doppelganger("number_grid", draw_number_grid)

	draw_number_grid_segments <- function() {
		grid.newpage()
		vp <- viewport(width = unit(1, "snpc"), height = unit(1, "snpc"))
		grob <- numberGridGrob(
			segments = list(c(34, 37, 67, 64, 34), c(1, 100)),
			vp = vp
		)
		grid.draw(grob)
	}
	vdiffr::expect_doppelganger("number_grid_segments", draw_number_grid_segments)
})
