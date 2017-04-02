#volcano plot
data(volcano)
summary(volcano)
require(misc3d)
voldf <- local({
  zz <- 7 * volcano
  xx <- 20 * (1:nrow(zz))
  yy <- 20 * (1:ncol(zz))
  surfaceTriangles(xx, yy, zz, color="cyan2")
})
drawScene(voldf, scale = FALSE)

require(grDevices); require(graphics)
filled.contour(volcano, color.palette = terrain.colors, asp = 1)
title(main = "volcano data: filled contour map")
drawScene(surfaceTriangles(seq(-1,1,len=30), seq(-1,1,len=30),
                           function(x, y) (x^2 + y^2), color2 = "green"))
