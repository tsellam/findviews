# findviews: a View Generator for Multidimensional Data

The findviews package helps exploring wide data sets, by detecting, ranking and
plotting groups of statistically dependent columns. It relies heavily on
ggplot2 and shiny.

findviews is expecially useful to get quickly familiar with a new dataset. Load
your data in a data frame, call `findviews`, and you are ready to go.


## Installation

The findviews package is currently under heavy development. You may install the latest
version as follows:
```R
devtools::install_github("tsellam/findviews")
```

## How to use it

### Overview

The findviews package is based on three functions:

1. `findviews` detects and plots groups of dependent variables. This function
   is useful to explore new datasets.
2. `findviews_to_compare` calls `findviews` and ranks the views by how well
   they separate two arbitrary subsets of the data. This function is useful 
   to compare groups - for instance young vs. old in a survey dataset, or
   winners vs. losers in a sports use case.
3. `findviews_to_predict` calls `findviews` and ranks the views by how well
   they predict an arbitrary variable. This function is useful to understand
   how one particular column is influenced by the other variables in the
   database (e.g., salary in a census) 

The following sections describe these 3 functions in more detail.

### The main function: `findviews`

`findviews` is the most important function in the package. It takes a data
frame or a matrix as input, as well as a few optional parameters described in
its R documentation. `findviews` performs the following operations:

1. It detects columns types and removes unpractical columns (e.g., primary keys or constants values).
2. It computes the statistical dependency between all the pairs of colums.
3. It detects clusters of dependent columns - that is, views.
4. It plots the views with ggplot2 and loads them in a Shiny app.

You may call `findviews` as follows: 

```R
findviews(mtcars)
```

As a result, R will start a browser and display the views.

![Screenshot of findviews](screenshot-findviews.png?raw=true "Screenshot of findviews")

You can pick a view on the left panel and visualize it in the main panel.


### Ranking the views: `findviews_to_predict` and `findviews_to_compare`

The function `findviews` can generate views, but it cannot tell which ones to
look at. `findviews_to_predict` and `findviews_to_compare` are steps in this
direction. These two functions generate views, exactly as `findviews` does - in
fact, they call `findviews` internally. But they also *rank* the results,
in effect making recommendations.

The function `findviews_to_compare` ranks views which highlight how two groups
of row *differ* from each other. Suppose for intance that we wish to compare
the rows for which `mpg > 20` and those for which `mpg < 20`. We call the
function as follows:

```R
findviews_to_compare(mtcars$mpg >= 20 , mtcars$mpg < 20 , mtcars)
```

The result is a set of views on which the two groups have different statistical
distributions:
![Screenshot of findviews_to_compare](screenshot-findviews_to_compare.png?raw=true "Screenshot of findviews_to_compare")

The aim of `findviews_to_predict` is to help users understand how a specific
column is influenced by the other columns in the database.  For instance,
suppose that we wish to understand what influences the variable `mpg` in the
`mtcars` data set. We would call `findviews_to_predict` as follows:

```R
findviews_to_predict('mpg', mtcars)
```

The result is a ranked set of views, as shown below.
![Screenshot of findviews_to_predict](screenshot-findviews_to_predict.png?raw=true "Screenshot of findviews_to_predict")


### `_core` functions

The functions `findviews`, `findviews_to_predict` and `findviews_to_compare`
present their results with Shiny. At times, this method can be heavy and we may
prefer to obtain the results directly as R objects (possibly to use them in a
more complex workflow). This is possible, with the `_core` functions.  The
functions `findviews_core`, `findviews_to_predict_core` and
`findviews_to_compare_core` operate exactly as their counterparts, but they
return their results as lists and data frames.


## A word of warning

Beware - the recommendations of `findviews` must be taken with a huge grain of
salt.  Some of its views are absurd - they are artifacts of the algorithms, or
the system just "got lucky" and made totally spurious findings. Inversely,
`findviews` will almost surely  miss important aspects of the data.

In summary `findviews` is designed to help you get started with a data set and
give some inspiration. But IN NO WAY does it replace critical judgement. In
fact, the best way to use it is to understand what it does. To this end, I
encourage you to read the functions' R documentation.

