19. From hypothesis testing to parameter estimation
================

Collected data: observed 100 games with 24 successes.

``` r
bayes_factor <- function(h1, h2) {
  (h1^24 * (1 - h1)^76) / (h2^24 * (1 - h2)^76)
}

# Calculate BF comparing the prob of all possible parameter values to 0.5.
dx <- 0.001
hypotheses <- seq(0, 1, dx)
bfs <- bayes_factor(hypotheses, 0.5)

ggplot(
  tibble(x = hypotheses, y = bfs),
  aes(x = x, y = y)
) +
  geom_line() +
  labs(x = "hypotheses for true success rate", y = "Bayes factor")
```

![](19_from-hypothesis-testing-to-parameter-estimation_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
a <- hypotheses[which.max(bfs)]
b <- round(max(bfs))
glue("The most likely hypothesis is {a} with a BF of {b}.")
```

    #> The most likely hypothesis is 0.24 with a BF of 1478776.
