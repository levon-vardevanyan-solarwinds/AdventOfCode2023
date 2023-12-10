#!/usr/bin/env python3

import numpy
from PIL import Image

image = Image.open('maze.png', 'r')
pixel_values = numpy.asarray(image)

pixel_values = pixel_values[::3]
pixel_values = pixel_values[:, ::3]
im = Image.fromarray(pixel_values, "RGBA")
print(im.getcolors())
im.save("solution.png", "PNG")
