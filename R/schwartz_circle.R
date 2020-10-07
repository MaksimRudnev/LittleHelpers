#' A helper function to draw Schwartz circle
#'
#' See \code{\link{schwartz_circle}}
#'
#' @param r a radius.
#' @param ... optional, passed to `geom_path`.
#'
#' @md
#'
#' @export

add_circle <- function(r, ...) {
  circle.data1 <- data.frame(x = r*cos(seq(0,2*pi,length.out=100)),
                             y=  r*sin(seq(0,2*pi,length.out=100)))
  geom_path(data=circle.data1, aes(x,y), ...)

}

#' A helper function to draw Schwartz circle
#'
#' See \code{\link{schwartz_circle}}
#'
#' @param angles.r1  a vector of angles.
#' @param r1 length of line.
#' @param r0 where the line begins, 0 by default
#' @param ... optional, passed to `geom_segment`.
#'
#' @md
#'
#' @export
add_radius <- function(angles.r1, r1, r0=0, ...) {

  angles.r1 <- (angles.r1 * pi) / (180) # Degrees to radians

  inner.sectors<- data.frame(
    x=rep(0, length(angles.r1)),
    y=rep(0, length(angles.r1)),
    xend=r1*cos(angles.r1),
    yend=r1*sin(angles.r1)
  )

  if(r0!=0) {
    inner.sectors$x<- r0*cos(angles.r1)
    inner.sectors$y<- r0*sin(angles.r1)
  }
  #inner.sectors
  geom_segment(data=inner.sectors, aes(x=x,y=y, xend=xend, yend=yend),  ...)

}

#' A helper function to draw Schwartz circle
#'
#' See \code{\link{schwartz_circle}}
#'
#' @param r  radius.
#' @param angle.r vector of angles to which labels are located.
#' @param label vector of character labels.
#' @param pos position between center of circle and circle, by default is 1/2, which is middle.
#' @param ... optional, passed to `geom_text`; important is `angle` which accepts vectors and rotates the labels..
#'
#' @md
#'
#' @export
add_label <- function(r, angle.r, label, pos=1/2, ...) {
  d<- data.frame(
    x= r*pos * cos((angle.r * pi) / (180)),
    y= r*pos * sin((angle.r * pi) / (180)),
    label=label
  )
  geom_text(data=d, aes(x,y,label=label),...)
}


#'Schwartz value circle with ggplot2 and three handy functions
#'
#'Using three functions I can create any version of the value circle and customize it for my purposes.
#'
#'@details
#'
#'The functions add step-by-step to the ggplot2 code just like the usual geoms.
#' @details This function us an application of the three lower-level funcions:
#'  \describe{
#'   \item{\code{\link{add_circle}}}{ one argument is `r`, a radius. All the others are optional, passed to `geom_path`.}
#'    \item{\code{\link{add_radius}} }{ two arguments `angles.r1` is a vector of angles , `r1` is a length of line, `r0` is where the line begins, 0 by default. All the others are passed to `geom_segment`.}
#'     \item{\code{\link{add_label}}}{ `r` is radius, `angle.r` - vector of angles to which labels are located, `label` - vector of character labels, `pos` - position between center of circle and circle, by default is 1/2, which is middle. All others are optional and passed to `geom_text`; important is `angle` which accepts vectors and rotates the labels.}
#'}
#'  \if{html}{\figure{circle.png}{options: width=400 alt="Schwartz circle"}}
#'
#' \preformatted{
#' ## Initial circle with radius 1
#' ggplot(data.frame(x = cos(seq(0,2*pi,length.out=100)),
#'                   y = sin(seq(0,2*pi,length.out=100))), aes(x,y))+coord_equal()+
#'   geom_path( col="black", linetype="solid")+
#'
#'   ## Split circle into 8 sectors
#'   add_radius(seq(0, 360, 360/9)[c(1:4,7:9)], 1)+
#'
#'   ## Label these 8 sectors
#'   add_label(r=1, angle.r=seq(1+20, 360-20, 319/8)[c(8,9,1:7)],
#'             label=v9,
#'             angle=seq(1+20, 360-20, 319/8)[c(8,9,1:7)]  %>% sapply( function(x) if(x>90 & x<270) x+180 else x  )      )+
#'
#'   ## Hedonism dashed radiuses
#'   add_radius(seq(0, 360, 360/9)[5:6], r1=1, linetype="dashed")+
#'
#'
#'   ## Add another circle for higher order values OP-CO-SE-ST
#'   add_circle(r=1.3, linetype="solid", size=0.7, alpha=0.9)+
#'   add_radius(seq(0, 360, 360/9)[c(1,3, 8)], r0=1, r1=1.3)+add_radius(180, r0=1, r1=1.3, linetype="solid")+
#'   add_label(r=1.3, angle.r=seq(0+40, 360, 360/4),
#'             v4,
#'             angle=c(-45, 40, -45, 40),
#'             pos=0.85, fontface="bold", size = 5)+
#'
#'   ## Yet another circles
#'   # Social-Person Focus
#'   add_circle(r=1.5, linetype="solid", size=1.5, alpha=0.7)+
#'   add_radius(seq(0, 360, 360/9)[c(3, 8)], r0=0, r1=1.5, size=1.5, alpha=0.7)+
#'   add_label(r=1.5, angle.r=c(0, 180), v2_1, angle=c(90, 90), pos=0.93, fontface="bold.italic", size = 5)+
#'
#'   # Growth-Protection
#'   add_circle(r=1.7, linetype="solid", size=3, alpha=0.4)+
#'   add_radius(c(0, 220), r0=0, r1=1.7, size=2, alpha=0.4)+
#'   add_label(r=1.7, angle.r=c(90-10, 270), v2_2, angle=c(-8,0),
#'             pos=0.93, fontface="bold", size = 6, color="grey30")+
#'
#'   theme_void()
#'    }
#'
#'
#'@md
#'
#'@export
#'

schwartz_circle <- function() {

  # Value labels (easy to translate or abbreviate)

  v9 <- c("Security",
          "Conformity/Tradition",
          "Benevolence",
          "Universalism",
          "Self-Direction",
          "Stimulation",
          "Hedonism",
          "Achievement",
          "Power" )

  v4 <- c("Self-Transcendence",
          "Openness to Change",
          "Self-Enhancement",
          "Conservation")
  v2_1 <- c("Social Focus", "Person Focus")
  v2_2 <- c("Growth", "Self-Protection")


  # Plotting

  ## Initial circle with radius 1
  ggplot(data.frame(x = cos(seq(0,2*pi,length.out=100)),
                    y = sin(seq(0,2*pi,length.out=100))), aes(x,y))+coord_equal()+
    geom_path( col="black", linetype="solid")+

    ## Split circle into 8 sectors
    add_radius(seq(0, 360, 360/9)[c(1:4,7:9)], 1)+

    ## Label these 8 sectors
    add_label(r=1, angle.r=seq(1+20, 360-20, 319/8)[c(8,9,1:7)],
              label=v9,
              angle=seq(1+20, 360-20, 319/8)[c(8,9,1:7)]  %>% sapply( function(x) if(x>90 & x<270) x+180 else x  )      )+

    ## Hedonism dashed radiuses
    add_radius(seq(0, 360, 360/9)[5:6], r1=1, linetype="dashed")+


    ## Add another circle for higher order values OP-CO-SE-ST
    add_circle(r=1.3, linetype="solid", size=0.7, alpha=0.9)+
    add_radius(seq(0, 360, 360/9)[c(1,3, 8)], r0=1, r1=1.3)+add_radius(180, r0=1, r1=1.3, linetype="solid")+
    add_label(r=1.3, angle.r=seq(0+40, 360, 360/4),
              v4,
              angle=c(-45, 40, -45, 40),
              pos=0.85, fontface="bold", size = 5)+

    ## Yet another circles
    # Social-Person Focus
    add_circle(r=1.5, linetype="solid", size=1.5, alpha=0.7)+
    add_radius(seq(0, 360, 360/9)[c(3, 8)], r0=0, r1=1.5, size=1.5, alpha=0.7)+
    add_label(r=1.5, angle.r=c(0, 180), v2_1, angle=c(90, 90), pos=0.93, fontface="bold.italic", size = 5)+

    # Growth-Protection
    add_circle(r=1.7, linetype="solid", size=3, alpha=0.4)+
    add_radius(c(0, 220), r0=0, r1=1.7, size=2, alpha=0.4)+
    add_label(r=1.7, angle.r=c(90-10, 270), v2_2, angle=c(-8,0),
              pos=0.93, fontface="bold", size = 6, color="grey30")+

    theme_void()

}
