# rstyles

# What is this package for?

Package *rstyles* provides functions that enables performing simulations involving IRT/CCFA models that assume responses to be affected by so-called *response styles*. It includes functions to generate (randomly or deterministically) items' parameters and to generate responses.

# Installing the package

The latest version of the package can be installed from GitHub using package `devtools`:

```{r}
devtools::install.github("tzoltak/rstyles")
```

# How to use the package

## General description

There are four steps one needs to follow to simulate responses to a test:

1.  Choose model describing how respondent answers an item.

    -   Within package *rstyles* model of answering an item is described on the most general level using so-called *scoring matrix*, i.e. a matrix that describes how different latent traits contribute to propensity of choosing a given category (answer). Names of columns of the *scoring matrix* corresponds to the names of latent traits in the model and names of rows to available response categories for each item. The same form of representation is used to describe two quite different types of models:

        -   noncompensatory IRTree models (Böckenholt, 2012, 2017) in which responding process is assumed to be a sequence of (typically binary, but this is not the only possible approach) decisions and in each of these decisions only one latent trait is involved; this type of models can be estimated by means of defining so-called *pseudo-items* - each of them describing such a single decision made by respondent (compare function `expand_responses()`); in the package *rstyles* this approach is identified as a *sequential mode* of responding;
        -   partially-compensatory models (Falk & Cai, 2016; Henninger & Meiser, 2020a, 2020b; Plieninger, 2017) in which latent traits are supposed to describe the specific way of perceiving the response scale by the respondent and may be thought as affecting the response process *simultaneously* (contrary to the IRTree approach), typically along with an additional latent trait that describes *what the test (scale) is supposed to measure* (i.e. not a response style); in the package *rstyles* this approach is identified as a *simultaneous mode* of responding.

    -   Although *scoring matrix* used to describe this both approaches within the package *rstyles* looks quite the same, it is used in a different way internally in the response generating step - that's why one needs to declare the *mode* of responding while defining items in the second step of the procedure.

    -   At the moment there is one function in the package that enables easy creation of *response matrices* describing many variants of the aforementioned models: `make_scoring_matrix_aem()`. Look at its documentation or at examples below for how to use it.

2.  Generate model parameters describing items.

    -   Having defined response model on the most general level one needs to define the number of items and to generate specific values of items' parameters, namely *slopes* (*discriminations*) and *intercepts* (*difficulties*/*thresholds*).

    -   One may use functions `generate_slopes()` and `generate_intercepts()` to generate matrices of items' parameters in a convenient way - see documentation of these functions and examples below.

        -   If one wants to prepare these matrices manually, he/she should remember to name its columns properly - see documentation of `generate_intercepts()`. Moreover one should recognize that the package employs the IRT model specification in which *intercepts* are added to the sum of products of *slopes* and value of respective latent traits, not subtracted from this sum (this is the same specification that is used in the package [*mirt*](https://CRAN.R-project.org/package=mirt)). Consequently ***intercepts*** **describe items'/categories' *easiness*, not *difficulty***.

    -   Having matrices of items' *slopes* and *intercepts* one may use function `make_test()` to prepare an object representing a test (i.e. collection of items) that can be further use to generate responses in the step four.

        -   At this point one also needs to declare which *mode of responding* should be used.

        -   Alternatively one may use function `make_item()` to declare each item separately and construct a list of objects returned by this function as a test description.

3.  Generate values of latent traits describing *respondents*.

    -   This is the only step that is not covered by the package *rstyles* itself - one needs to use packages developed to draw samples from multivariate distributions (typically normal, truncated-normal or *t*). Some of such packages are described in the last section of this page.
    -   One thing one needs to remember is to assign proper column names to the generated matrix - these should be **the same names as those of columns of the *scoring matrix***.

4.  Generate responses using information from the three previous steps.

    -   Now one may generate responses using function `generate_test_responses()`, provided with a matrix of values of the latent traits and a list of objects describing items included in the test.
    -   To generate responses according to GPCM you may also use function `simdata()` from package *mirt* (see below).

## Examples

### IRTree model (noncompensatory)

-   Below perhaps the most widely known IRTree model is used: Middle-Acquiescence-Extreme (MAE) model for a 5-point Likert scale items (Böckenholt, 2012, 2017).
-   Test consist of 20 items.
-   Items' *slopes* are generated from a log-normal distribution with expected value and standard deviation on the log scale being 0 and 0.2 respectively.
-   Items' *intercepts* (*thresholds*) are generated from a normal distribution with expected value of 0 and standard deviation of 1.5.
-   Latent traits are assumed to be standard normal and independent of each other (this is not a very plausible assumption).
-   There are 1000 *respondents* (responses that are generated).
-   Function `mirt()` from package *mirt* is used to estimate 2PL IRT model on the generated data, using so-called *pseudo-items* approach (function `expand_responses()` enables reshaping data to the *pseudo-items* form).

```{r}
require(rstyles)
require(mnormt)
require(mirt)

# you should alway set seed in simulation to make your results reproducible
set.seed(123456)
# generating test
nItems <- 20
sM <- make_scoring_matrix_aem(1:5, "mae")
slopes <- generate_slopes(nItems, sM, FUN = rlnorm, meanlog = 0, sdlog = 0.2)
intercepts <- generate_intercepts(nItems, sM,
                                  FUNd = rnorm, argsd = list(mean = 0, sd = 1.5))
items <- make_test(sM, slopes, intercepts, "sequential")

# generating "subjects" - uncorrelated traits
vcovTraits <- matrix(0, nrow = ncol(sM), ncol = ncol(sM),
                     dimnames = list(colnames(sM), colnames(sM)))
diag(vcovTraits) <- 1
theta = rmnorm(1000, varcov = vcovTraits)
colnames(theta) <- colnames(vcovTraits)

# generating responses
resp <- generate_test_responses(theta, items)

# scaling
respWide <- expand_responses(resp, sM)
mSqt <- mirt(respWide,
             mirt.model("m = 1-20
                         a = 21-40
                         e = 41-60"),
             '2PL')
```

### Partially-compensatory random-thresholds GPCM including *middle*, *extreme* and *acquiescence* response styles

-   Below the model is defined in which apart of the *trait the test is supposed to measure*, named "i", there are three additional latent traits describing response styles that affect responses *simultaneously*. This traits may be interpreted as describing *middle* ("m"), *extreme* ("e") and *acquiescence* ("a") response styles.
-   Test consist of 20 items, half of which is *reversed* (i.e. *negatively* associated with the trait called "i").
-   Items' *slopes* are generated from a log-normal distribution with expected value and standard deviation on the log scale being 0 and 0.2 respectively.
-   Items' *intercepts* (*thresholds*) in a two-step procedure in which firstly (*general* or, technically speaking, average) *difficulties* of the items are generated (precisely these are *ease* parameters due to the parameterization that is used by the package) and next *thresholds* are generated relatively to these items' *difficulties*. At the end this both are summed up to get actual *intercepts*. In the code below:
    - Items' *difficulties* are sampled from a uniform distribution ranging from -2 to 2.
    - *Thresholds* relative to the item's difficulty are generated deterministically as a sequence of four values regularly spanned between 0.9 and -0.9.
-   Latent traits are assumed to be standard normal and independent of each other (this is not a very plausible assumption).
-   There are 1000 *respondents* (responses that are generated).
-   Function `mirt()` from package *mirt* is used to estimate 2PL IRT model on the generated data, with a custom *scoring matrix* provided using `gpcm_mats` argument.

```{r}
require(rstyles)
require(mnormt)
require(mirt)

# you should alway set seed in simulation to make your results reproducible
set.seed(123456)
# generating test
nItems <- 20
sM <- make_scoring_matrix_aem(1:5, "simultaneous")
slopes <- cbind(generate_slopes(nItems, sM[, 1L, drop = FALSE],
                                FUN = rlnorm, meanlog = 0, sdlog = 0.3,
                                nReversed = floor(nItems / 2)),
                generate_slopes(nItems, sM[, 2:4], 1))
intercepts <- generate_intercepts(nItems, sM,
                                  FUNd = runif, argsd = list(min = -2, max = 2),
                                  FUNt = seq, argst = list(from = 0.9,
                                                           to = -0.9,
                                                           length.out = 4))
items <- make_test(sM, slopes, intercepts, "simultaneous")

# generating "subjects" - uncorrelated traits
vcovTraits <- matrix(0, nrow = ncol(sM), ncol = ncol(sM),
                     dimnames = list(colnames(sM), colnames(sM)))
diag(vcovTraits) <- 1
theta = mnormt::rmnorm(1000, varcov = vcovTraits)
colnames(theta) <- colnames(vcovTraits)

# generating responses
resp <- generate_test_responses(theta, items)

# scaling
mSml <- suppressMessages(mirt(resp,
                              mirt.model("i = 1-20
                                          m = 1-20
                                          e = 1-20
                                          a = 1-20"),
                         'gpcm',
                         gpcm_mats = lapply(1:ncol(resp), function(x) sM),
                         method = "EM")
```

## Specific solutions

### Reversed (reverse-keyed) items

If one wants to include reversed (reverse-keyed) item into the analysis using the *rstyles* package the convenient way of doing so is by specifying negative items' *slopes* (*discriminations*). For this purpose function `generate_slopes()` has optional arguments `nReversed`, `reverseTraits` and `reverseIndep` (although this last one is rarely needed).

Also, it is possible to specify distinct *scoring matrices* for the *reversed* and the *regular* items, but this will result in the need of either manually defining list of item objects that will be passed to `generate_test_responses()` or generating responses separately for the *reversed* and the *regular* items - see section *Mixtures* below (this is because function `make_test()` may use only one *scoring matrix* in each call).

### Log-normal distribution parameters

Log-normal distribution is parameterized on the log scale (i.e. by parameters of the *underlying* normal distribution) but while generating parameters one is always interested in the parameters on the *exponential* scale, i.e. the scale of the sampled values. To deal with this problem package *rstyles* provides a set of functions:

- `lnorm_mean()` and `lnorm_sd()` enables to compute respectively expected value and standard deviation of the log-normal distribution with a given *meanlog* and *sdlog* parameters (compare `?rlnorm`);
- `find_pars_lnorm()` returns values of the *meanlog* and *sdlog* parameters one should use to get expected value and standard deviation of the log-normal distribution specified as arguments to this function.

### Mixtures

If one wants to generate responses from a mixture of different *populations* (groups of *respondents*) that differ with respect to the distribution of the latent traits or with respect to the model describing how they answer items, he/she should simply generate independently responses in each (sub)group and then bind them (using for example with `rbind()`, but please note that it will be typically beneficial to add a variable describing what *population* given result belongs to to the results before performing the binding).

One may also generate results from the different *sub-test* (collections of items) independently and then bind them using for example `cbind()` but in such a case the same matrix of the generated values of the latent traits should (typically) be used while generating responses to each *sub-test*.

### Speeding up generation of GPCM responses using `simdat()` function from the *mirt* package

# Function `simdata()` from the *mirt* package will be much faster than `generate_test_responses()` while generating GPCM responses, especially with large number of items or observations (or both). Luckily (since version 0.4.0 of *rstyles*), matrices of parameters generated by `generate_slopes()` and `generate_intercepts()` are fully-compatible with the `simdata()` function.

For example in the listing included in *Partially-compensatory GPCM including "middle", "extreme" and "acquiescence"" response styles* section above you can substitute call to `generate_test_responses()` by the following call to `simdata()`:

```{r}
respSimdata <- simdata(slopes, intercepts, N = nrow(theta), itemtype = "gpcm",
                       Theta = theta, gpcm_mats = rep(list(sM), ncol(slopes)))
```

However, remember that `simdata()` always returns responses as numbers starting from 0 (the first category), irrespective the scoring matrix you provide it.

# To do

-   functions to compute non-GPCM (2PLM) models at nodes of *sequentially* responded items;

-   functions to use as `editResponse` argument to `make_item()`;

-   implementation of Plieninger & Heck (2018) generalized IRTree approach (large refactorization will be needed because this kind of models can't be described by a simple *scoring matrix*);

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

-   [mvtnorm](https://CRAN.R-project.org/package=mvtnorm) package - multivariate normal and *t* distributions

-   [mnormt](https://CRAN.R-project.org/package=mnormt) package - multivariate normal and *t* distributions and their truncated versions

-   [truncnorm](https://CRAN.R-project.org/package=truncnorm) package - truncated normal distributions

-   [TruncatedNormal](https://CRAN.R-project.org/package=TruncatedNormal) package - multivariate normal and *t* distributions and their truncated versions

-   [OrdNor](https://CRAN.R-project.org/package=OrdNor) package - generating data with normal and categorical variables

# Literature

Böckenholt, U. (2012). Modeling multiple response processes in judgment and choice. Psychological Methods, 17(4), 665–678. <https://doi.org/10.1037/a0028111>

Böckenholt, U. (2017). Measuring response styles in Likert items. *Psychological Methods, 22*(1), 69–83. <https://doi.org/10.1037/met0000106>

Falk, C. F., & Cai, L. (2016). A flexible full-information approach to the modeling of response styles. *Psychological Methods, 21*(3), 328–347. <https://doi.org/10.1037/met0000059>

Henninger, M., & Meiser, T. (2020). Different approaches to modeling response styles in divide-by-total item response theory models (part 1): A model integration. *Psychological Methods, 25*(5), 560–576. <https://doi.org/10.1037/met0000249>

Henninger, M., & Meiser, T. (2020). Different approaches to modeling response styles in divide-by-total item response theory models (part 2): Applications and novel extensions. *Psychological Methods, 25*(5), 577–595. <https://doi.org/10.1037/met0000268>

Plieninger, H. (2017). Mountain or Molehill? A Simulation Study on the Impact of Response Styles. *Educational and Psychological Measurement, 77*(1), 32–53. <https://doi.org/10.1177/0013164416636655>

Plieninger, H. & Heck, D.W. (2018). A New Model for Acquiescence at the Interface of Psychometrics and Cognitive Psychology. *Multivariate Behavioral Research, 53*(5), 633-654, <https://doi.org/10.1080/00273171.2018.1469966>
