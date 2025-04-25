<p align="left">
  <img src="man/figures/curvr_hex_sticker.png" height="120" />
</p>


<br>

## Installation

You can install the development version of `curvr` by:

``` r
# install.packages("devtools")
devtools::install_github("mannfred/curvr")
```

If you find this package useful, please cite:
<br>
Boehm, M. M. A., Jankowski, J. E., & Cronk, Q. C. (2022). 
Plant-pollinator specialization: Origin and measurement of curvature. 
*The American Naturalist*, 199(2), 206-222.

```
@article{boehm_2022,
  title={Plant-pollinator specialization: Origin and measurement of curvature},
  author={Boehm, Mannfred M.A. and Jankowski, J.E. and Cronk, Q.C.B.},
  year={2022},
  journal={The American Naturalist},
  volume={199},
  number={2},
  pages={206--222}
}
```
<br>
[Click here](https://mannfred.github.io/media/pdfs/Boehm_etal_2022_AmNat.pdf) for a .pdf of the paper. 
<br>
<br>

## Example

This photograph of *Centropogon granulosus* and Buff-tailed Sicklebill was taken by 
Julian Heavyside in the southeastern Andes of Peru:

<p align="left">
  <img src="man/figures/Figure_A2.jpg" height="300" />
</p>

<br>

How curved is the bill of this hummingbird? One approach is to add landmarks along the 
dorsal side of the bill using `geomorph`. Then, we can use `curvr` to calculate 
total curvature from the landmarks. 


``` r
library(curvr)
library(geomorph)

# landmark the .jpg in this R package's directory
geomorph::digitize2d(filelist="man/figures/Figure_A2.jpg", nlandmarks=10, tpsfile="sicklebill.tps", verbose=F)
```
Naturally, one would be more rigorous with landmarking for comparative purposes.
This is for demonstration only.

<p align="left">
  <img src="man/figures/Figure_A2_lm.jpg" height="300" />
</p>

`curvr` fits a spline to these landmarks and calculates the curvature of the spline.

```r
# import the .tps file for curve-fitting
tps <- readland.tps(file="man/figures/sicklebill.tps")

# calculate curvature (curvr gives radians)
ktot <- curvr::curvature_spline(tps[,,1], x_range=range(tps[,,1]))$Ktot

# convert to degrees
ktot * (180/pi)
# 57.7 degrees
```

<br>
<br>

