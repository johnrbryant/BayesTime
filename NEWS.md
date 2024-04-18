
# BayesRates 0.1.1

## Interface

-  `augment()` and `components()` now give the same result every time they are called, rather than generating new results at random. This is implemented by storing a random seed in the results object.
