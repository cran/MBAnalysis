## Draw a circle

## @description  Draw a circle

## @usage circle(center = c(0,0),diameter = 2, npoints = 100)

## @param center Center.
## @param diameter Diameter.
## @param npoints Number of points.

## @return returns a circle of a desired center and diameter.

circle <- function(center = c(0,0),diameter = 2, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
