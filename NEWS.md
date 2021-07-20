# rstyles 0.4.0 (20.072021)

## New features

- `make_test()` assigns names to the created items by default and provides additional `names` argument if user wants to provide names himself/herself.
- `generate_test_responses()` uses items' names (if there are any) to name columns of the returned matrix.
- `generate_test_responses()` converts matrix it returns to numeric one (if only this is possible without loss of information); it also provides additional argument `tryConvertToNumeric` that allows to bring back its former behavior (i.e. returning a character matrix).

## Documentation

- Some improvements in documentation.

# rstyles 0.3.0 (5.05.2021)

## New features

- Functions that enables user to generate *items* and *tests*: `generate_slopes()`, `generate_intercepts()` and `make_test()`.
- Functions to compute log-normal distribution parameters on the scale of the actual variable (i.e. the exponential scale): `lnorm_mean()`, `lnorm_sd()` and `find_pars_lnorm()`.
- Function that enables user to generate a very simple *scoring matrix* that doesn't deal with any response styles: `make_scoring_matrix_trivial()`. It is useful if one wants to use *rstyles* for simulations that don't involve response styles.

## Bug-fixes

- Some improvements in documentation.

# rstyles 0.2.0 (17.12.2020)

- First ready to use version of the package.
