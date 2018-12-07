require(ggplot2)
require(ggthemes)

## theme

# https://material.io/design/color/the-color-system.html#tools-for-picking-colors
pal_heart <- c('#fee5d9','#fcbba1','#fc9272','#fb6a4a','#de2d26','#a50f15')
blue_heart <- "#3F69AA"
#ten color pallette:
# c('#fee5d9', #part of the current palette
#   '#fcbba1', '#fc9272',
#   '#FF6666', 
#   '#fb6a4a','#de2d26','#a50f15', 
#   'darkred',
#   '#660000', 
#   '#330000'))

############# color pieces!
scale_fill_heart <- function(){
  
  structure(list(
    scale_fill_manual(values=pal_heart)
  ))
}

scale_color_discrete_heart <- function(){
  
  structure(list(
    scale_color_manual(values=pal_heart)
  ))
}

scale_color_continuous_heart <- function(){
  
  structure(list(
    scale_color_gradientn(colours = pal_heart)
  ))
}
##################################

theme_heart <- function(base_size = 20, base_family = "Helvetica",
                          base_line_size = base_size / 42,
                          base_rect_size = base_size / 42) {
  # Starts with theme_bw and remove most parts
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      axis.ticks      = element_blank(),
      axis.text       = element_text(size = 16,color=blue_heart),
      axis.title = element_text(color = blue_heart),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.title = element_blank(),
      panel.background   = element_blank(),
      panel.border      = element_blank(),
      strip.background  = element_blank(),
      plot.title = element_text(color = blue_heart, face = "bold"),
      plot.background   = element_blank(),
      
      complete = TRUE
    )
}

p <- ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species))+
  geom_point()
# Use economist color scales
p + scale_color_discrete_heart() + theme_heart() + ggtitle("Sample Heart Theme")

