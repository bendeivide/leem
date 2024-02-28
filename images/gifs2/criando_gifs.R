# Link
# https://www.nagraj.net/notes/gifs-in-r/

# Pacotes
library(magick)
library(ggplot2)
library(dplyr)
library(tidyr)

# Gerando os graficos
showtabnormal(3.12)

## list file names and read in
imgs <- list.files()[-1]
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
