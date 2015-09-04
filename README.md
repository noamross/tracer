<!-- README.md is generated from README.Rmd. Please edit that file -->
A package to collect trace information from optimization functions. Very early-stage and experimental.

Use `nloptr_tr()` in place of `nloptr::nloptr()`. It will give the same results but append the trace information to the result as an attribute, as well as adding the class `tracer`.

Use `tracer(result)` to retrieve the trace information.

TODO:
-----

-   General package infrastructure, documentation, testing, etc.
-   Methods for other optimizers (**minqa**, **optim**, **optimx**)
