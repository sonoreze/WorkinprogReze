#library(rstudioapi)
#library(fBasics)
#https://www.coloringnoise.com/theoretical_background/new-color-scheme/

r <- c(130,160,184,207,227,244,232,205,161,117,67)
g <- c(137,187,214,228,242,198,125,70,26,9,10)
b <- c(172,191,209,204,191,131,77,63,77,93,74)

sound <- function (n, name = c("soundcolors"))
{
  soundcolors = rgb(r, g, b, maxColorValue = 255)
  name = match.arg(name)
  orig = eval(parse(text = name))
  rgb = t(col2rgb(orig))
  temp = matrix(NA, ncol = 3, nrow = n)
  x = seq(0, 1, , length(orig))
  xg = seq(0, 1, , n)
  for (k in 1:3) {
    hold = spline(x, rgb[, k], n = n)$y
    hold[hold < 0] = 0
    hold[hold > 255] = 255
    temp[, k] = round(hold)
  }
  palette = rgb(temp[, 1], temp[, 2], temp[, 3], maxColorValue = 255)
  palette
}

# pal_sound100 <- sound(n=100)
# pal_sound10 <- sound(n=10)
# 
# par(mar = rep(0, 4))
# pie(rep(1, length(pal_sound10)), col = pal_sound10)
# pie(rep(1, length(pal_sound100)), col = pal_sound100)

# pal_sound <- sound(n=10)
# par(mar = rep(0, 4))
# pie(rep(1, length(pal2)), col = pal_sound)
