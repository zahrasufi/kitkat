#' Volume of triangular pyramid 
#' 
#' Compute the volume of triangular pyramid shape
#' @param a is the area of the base
#' @param t is the height of triangle
#' @param T is the height of triangular prism
#' @return The volume triangular of pyramid
#' @examples
#' volume1 <- vpyramid(5,3,2);
#' volume2 <- vpyramid(4,3,1);
#' @export
vtriangularpyramid<-function(a,t,T)
{volume<-(1/2)*a*t*T
return(volume)}


#' Volume of cylinder (tube)
#'
#' Compute the area of circle shape
#' @param r is the radius of the cirlce
#' @param T is the height of cylinder
#' @return Volume of cylinder (tube)
#' @examples
#' volume1 <- vcylinder(4,2);
#' volume2 <- vcylinder (3,1);
#' @export
vcylinder<-function(r,T)
{area<-pi*(r^2)*T
return(area)}


#' Volume of rectangular prism
#' 
#' Compute the volume of rectangular prism shape
#' @param l is the base length
#' @param w is the base width
#' @param h is the height of the prism
#' @return The volume rectangular of prism
#' @examples
#' volume1 <- vrectangularprism(2,1,4);
#' volume2 <- vrectangularprism(4,5,1);
#' @export
vrectangularprism<-function(l,w,h)
{volume<-l*w*h
return(volume)}


#' Area of cube
#'
#' Compute the area of cube shape
#' @param r is the square side length
#' @return The area of cube
#' @examples
#' area1 <- acube(4);
#' area2 <- acube(3);
#' @export
acube<-function(r)
{area<-6*(r^2)
return(area)}


#' Area of cylinder
#'
#' Compute the area of cylinder shape
#' @param r is the radius of the cylinder
#' @param h is the height of the cylinder
#' @return The area of cylinder
#' @examples
#' area1 <- acylinder(4);
#' area2 <- acylinder(3);
#' @export
acylinder<-function(r,h)
{area<-2*pi*r*h+(2*pi*(r^2))
return(area)}


#' Perimeter of semicircle
#'
#' Compute the perimeter of semicircle shape
#' @param r is the radius of the semicircle
#' @return The perimeter of semicircle
#' @examples
#' perimeter1 <- psemicircle(5);
#' perimeter2 <- psemicircle(8);
#' @export
psemicircle<-function(r)
{perimeter<-(pi*r)+(2*r)
return(perimeter)}