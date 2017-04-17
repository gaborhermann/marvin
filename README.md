# Marvin

A small machine learning library in Haskell. My Bachelor's thesis.

Parts of the design was inspired by the popular [scikit-learn](http://scikit-learn.org/stable/) Python library.

## Features
- Type classes for machine learning tasks: `fit`, `predict`, `evaluate`.
- Combining transformations with `Pipeline` type, an `Arrow`-instance. Instead
  of using transformers as in [scikit-learn](http://scikit-learn.org/stable/data_transforms.html),
  we make preprocessing composable with a pinch of category theory.
- Error handling with `Either`.
- `Table`: column-oriented representation of datasets (also for validating data
  type).
- Small (and ugly) GUI for showcasing.

## How to build

Use [stack](https://www.haskellstack.org/). Recommended to install `gtk-buildtools` before building.

## Documentation

See [haddock](https://www.haskell.org/haddock/). Generate them with e.g. `stack haddock`.

## Datasets for running examples

You can run the examples with publicly available datasets.
- `HousingPrices`: https://archive.ics.uci.edu/ml/datasets/Housing
- `NetworkIntrusionDetection`: http://kdd.ics.uci.edu/databases/kddcup99/kddcup99.html
- `SpamClassification`: http://www2.aueb.gr/users/ion/data/enron-spam/
