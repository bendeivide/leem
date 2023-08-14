# Link
# https://www.nagraj.net/notes/gifs-in-r/

# Pacotes
library(magick)
library(ggplot2)
library(dplyr)
library(tidyr)

# Gerando os graficos
a = 105; b = 110; col = "lightblue"; mean = 90; sd = 10; type = 6; rounding = 4; zang = 0; xang = 0
probnormal(a, b, col, mean, sd, type, rounding, zang, xang)

## list file names and read in
imgs <- list.files()
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 0.5)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "leem.gif")
