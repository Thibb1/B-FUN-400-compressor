# imageCompressor

## How to use

Install the following packages:
- [stack](https://docs.haskellstack.org)

```bash
make
./imageCompressor -n N -l L -f F
```
- **N** number of colors in the final image
- **L** convergence limit
- **F** path to the file containing the colors of the pixels

## Input file

```bash
>head exampleIn | cat -e
(0,0) (33,18,109)$
(0,1) (33,18,109)$
(0,2) (33,21,109)$
(0,3) (33,21,112)$
(0,4) (33,25,112)$
(0,5) (33,32,112)$
(1,0) (33,18,109)$
(1,1) (35,18,109)$
```

## Background

A pretty basic way to compress image consists in reducing the number of colors it contains.
3 steps are needed to do so:

1. **read** the image and **extract** the colors of each pixel,
2. **cluster** these colors, and **replace** each color of a given cluster by the mean color of this cluster,
3. **index** the means of the cluster, and **create** the compressed image.