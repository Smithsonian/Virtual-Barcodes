# From https://pypi.org/project/pylibdmtx/

from PIL import Image
import pylibdmtx.pylibdmtx as dmtx
import sys

#Get what to encode from the command line
id = sys.argv[1]
wh = int(sys.argv[2])

encoded = dmtx.encode(id.encode('utf8'))
img = Image.frombytes('RGB', (encoded.width, encoded.height), encoded.pixels)

#Resize to 400x400
im1 = img.resize((wh, wh), Image.NEAREST)

#Write image
im1.save('data/' + id + '.png')

#To test: http://boy.co.ua/decode.php
