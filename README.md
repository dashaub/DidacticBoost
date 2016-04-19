[![Travis-CI Build Status](https://travis-ci.org/dashaub/DidacticBoost.svg?branch=master)](https://travis-ci.org/dashaub/DidacticBoost)
[![Coverage Status](https://coveralls.io/repos/github/dashaub/DidacticBoost/badge.svg?branch=master)](https://coveralls.io/github/dashaub/DidacticBoost?branch=master)
[![CRAN version](http://www.r-pkg.org/badges/version/DidacticBoost)](http://www.r-pkg.org/pkg/DidacticBoost)
# DidacticBoost
A simple demonstration and implementation of gradient boosting


## Installation
The stable release of the package is hosted on [CRAN](https://cran.r-project.org/web/packages/DidacticBoost/index.html) and can be installed as usual:
````r
install.packages("DidacticBoost")
````
## Usage
```r
library(DidacticBoost)
k <- kyphosis
k$Kyphosis <- factor(ifelse(k$Kyphosis == "present", 1L, -1L))
fit <- fitBoosted(Kyphosis ~ Age + Number + Start, data = k, iterations = 10)
predict(fit, newdata = k[, 1:5])
````

