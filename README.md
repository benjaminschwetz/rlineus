
<!-- README.md is generated from README.Rmd. Please edit that file -->
rlineus
=======

<!-- badges: start -->
<!-- badges: end -->
A graphical device for the line-us plotter.

Usage
=====

**Make sure the pen height is callibrated correctly (with phone app)**

``` r
library(rlineus)
lineus_dev()
boxplot(data = iris, Sepal.Length ~Species)
dev.off()
```

Limitations/todo
----------------

-   no text
-   no pagination
-   no fills

Inspiration / Foundation
------------------------

[thomasp85/fawkes](https://github.com/thomasp85/fawkes)

[coolbutuseless/devout](https://github.com/coolbutuseless/devout)

[ixd-hof/LineUs\_SVG](ixd-hof/LineUs_SVG)

[Line-us/LineUsPythonModule](https://github.com/Line-us/LineUsPythonModule)
