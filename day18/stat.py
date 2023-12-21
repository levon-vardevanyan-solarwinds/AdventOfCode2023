#!/usr/bin/env python3

from PIL import Image

image = Image.open('floodfilled.png', 'r')
print(image.getcolors())
