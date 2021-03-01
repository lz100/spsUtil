# spsUtil <img src="https://github.com/lz100/spsUtil/blob/master/img/sps_sub_pkgs.png?raw=true" align="right" height="139" />

The {[systemPipeShiny](https://github.com/systemPipeR/systemPipeShiny)} (SPS) framework comes with many useful utility functions. 
However, installing the whole framework is heavy and takes some time. If you 
like only a few useful utility functions from SPS, install this package is enough. 

## Install

Install release version from CRAN:

```r
install.packages("spsUtil")
```

Develop version:

```r
if (!requireNamespace("spsUtil", quietly=TRUE))
    remotes::install_github("lz100/spsUtil")
```

## User manual 

Read the manual of all utilities on [our website](https://systempipe.org/sps/dev/general/)

## Other packages in systemPipeShiny

| Package | Description | Documents | Demo |
| --- | --- | --- | --- |
|<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/sps_small.png?raw=true" align="right" height="25" />[systemPipeShiny](https://github.com/systemPipeR/systemPipeShiny) | SPS main package |[website](https://systempipe.org/sps/)|[demo](https://tgirke.shinyapps.io/systemPipeShiny/)|
|<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/spscomps.png?raw=true" align="right" height="25" />[spsComps](https://github.com/lz100/spsComps) | SPS UI and server components |[website](https://systempipe.org/sps/dev/spscomps/)|[demo](https://lezhang.shinyapps.io/spsComps)|
|<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/spsutil.png?raw=true" align="right" height="25" />[spsUtil](https://github.com/lz100/spsUtil) | SPS utility functions |[website](https://systempipe.org/sps/dev/spsutil/)|NA|
|<img src="https://github.com/systemPipeR/systemPipeR.github.io/blob/main/static/images/drawr.png?raw=true" align="right" height="25" />[drawR](https://github.com/lz100/drawR) | SPS interactive image editing tool |[website](https://systempipe.org/sps/dev/drawr/)|[demo](https://lezhang.shinyapps.io/drawR)|
