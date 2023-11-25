## i(a)

# for fun
def pad(m: list[list], sz: int, pad=0):
    """Pad a 2D nxn matrix with `pad` of length `sz`

    Args:
        m (list[list]): 2D matrix to pad
        sz (int): Thickness of padding (number of layers of padding)
        pad (int, optional): Value to pad matrix with. Defaults to 0.
    """
    w = len(m)
    h = len(m[0])
    for _ in range(sz):
        m.insert(0, [pad for _ in range(w)])
    for _ in range(sz):
        m.append([pad for _ in range(w)])
    for j in range(h + sz * 2):
        for _ in range(sz):
            m[j].insert(0, pad)
            m[j].append(pad)
    return m


def convolve(
  arr: list[list], 
  kernel: list[list], 
  stride = 1) -> list[list]:
  mat = [[]]
  # check for nxn array and kxk kernel
  if len(arr) == len(arr[0]) and len(kernel) == len(kernel[0]):
    n = len(arr)
    k = len(kernel)
    nn = n - 2
    mat = [[0 for _ in range(nn)] for _ in range(nn)]
    i = 0
    j = 0 
    for i in range(0, nn, stride):
      for j in range(0, nn, stride):
        kr = 0 # kernel r
        for r in range(i, i + k):
          kc = 0 # kernel c
          for c in range(j, j + k):
              mat[i][j] += arr[r][c] * kernel[kr][kc]
              kc += 1
          kr += 1
  return mat


mat0 = [
    [1, 2, 3, 4, 5],
    [1, 3, 2, 3, 10],
    [3, 2, 1, 4, 5],
    [6, 1, 1, 2, 2],
    [3, 2, 1, 5, 4],
]
k0 = [[1, 0, -1], [1, 0, -1], [1, 0, -1]]
# print(convolve(mat0, k0))

## i(b)
import numpy as np
from PIL import Image

im = Image.open("strawberry_128.png")
rgb = np.array(im.convert("RGB"))
r = rgb[:, :, 0]  # array of R pixels

k1 = [[-1, -1, -1], [-1, 8, -1], [-1, -1, -1]]
k2 = [[0, -1, 0], [-1, 8, -1], [0, -1, 0]]

tr = convolve(r, k2, 1)
Image.fromarray(np.uint8(tr)).show()