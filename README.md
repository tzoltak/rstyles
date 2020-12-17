# rstyles

# How to use package

# How package works

# To do

-   some documentation on how to use package;

-   function(s) that will automate creating sets of items (i.e. *tests*) providing user a control of the distribution of values of its parameters;

-   functions to compute non-GPCM (2PLM) models at nodes of *sequentially* responded items;

-   functions to use as `editResponse` argument to `make_item()`;

-   implementation of Plieninger & Heck (2018) generalized IRTree approach;

    -   in this approach responding is assumed to be *sequential* but be *locally simultaneous* () at some nodes;

    -   refactorization will be needed because such generalized approach can't be described with a single scoring matrix (at least not without additional information about grouping of columns) - rather actual trees should be implemented to describe items' structure;

# Other useful packages and repositories

## IRT and response styles

-   [mirt](https://github.com/philchalmers/mirt) package

-   [irtrees](https://github.com/cran/irtrees) package

-   [IRtreeslnORM](https://github.com/hplieninger/IRtreesInORM) package (repository)

-   [stylesim](https://github.com/hplieninger/stylesim) package (repository)

-   [mpt2irt](https://github.com/hplieninger/mpt2irt) package (repository)

-   [FitResponseStyles](https://github.com/mirka-henninger/FitResponseStyles) repository

## Simulations and generating distributions

-   [SimDesign](https://github.com/philchalmers/SimDesign) package

-   [mvtnorm](https://cran.r-project.org/web/packages/mvtnorm/) package - multivariate normal and *t* distributions

-   [mnormt](https://cran.r-project.org/web/packages/mnormt/) package - multivariate normal and *t* distributions and their truncated versions

-   [TruncatedNormal](https://cran.r-project.org/web/packages/TruncatedNormal/) package - multivariate normal and *t* distributions and their truncated versions

-   [OrdNor](https://cran.r-project.org/web/packages/OrdNor/) package - generating data with normal and categorical variables
