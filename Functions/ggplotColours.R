#Replicates the ggplot colour pallete

#credit to
#http://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette

ggplotColours <- function(n = 6, h = c(0, 360) + 15){
  if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}
