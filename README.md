
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rlineus

<!-- badges: start -->

<!-- badges: end -->

A graphical device for the line-us plotter.

## Installation / Setup

``` r
remotes::install_github("benjaminschwetz/rlineus")
install_py_lineus() # installs python dependencies
```

## Usage

### Callibartion

This still needs to be done with the [phone
app](https://www.line-us.com/software.html).

### Test the setup

``` r
rlineus::hello_world()
```

### Usage

``` r
library(rlineus)
lineus_dev()
boxplot(data = iris, Sepal.Length ~Species) # your plot goes here
dev.off()
```

## Limitations/todo

  - no text
  - no pagination
  - no fills

## Inspiration / Foundation

[thomasp85/fawkes](https://github.com/thomasp85/fawkes)

[coolbutuseless/devout](https://github.com/coolbutuseless/devout)

[ixd-hof/LineUs\_SVG](https://github.com/ixd-hof/LineUs_SVG)

[Line-us/LineUsPythonModule](https://github.com/Line-us/LineUsPythonModule)
