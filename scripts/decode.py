from pylibdmtx.pylibdmtx import decode
from PIL import Image
import sys

#Get what to encode from the command line
img_file = sys.argv[1]

print(decode(Image.open(img_file)))
