# SSS-Data-analysis

## R env

### Installing a package

To install a package in your `R env` enter the following in the console:

```r
install.packages("my_pckg_name")
```

### Update / freeze the environment (lockfile)

After adding/removing/upgrading packages, update the lockfile:

```r
renv::snapshot()
```

### 4) Restore the environment (on a new machine / fresh clone)

After cloning the repo, open the project in RStudio and run:

```r
renv::restore()
```
