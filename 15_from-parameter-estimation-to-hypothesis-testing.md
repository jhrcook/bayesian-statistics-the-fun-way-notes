15. From parameter estimation to hypothesis testing: building a Bayesian
A/B test
================

Prior distribution of Beta(3,7).

``` r
prior_alpha <- 3
prior_beta <- 7

df <- tibble(x = seq(0, 1, 0.001), prob = dbeta(x, prior_alpha, prior_beta))

ggplot(df, aes(x = x, y = prob)) +
  geom_line() +
  labs(
    x = "conversion rate",
    y = "density",
    title = "Weak prior belief in conversion rate Beta(3,7)"
  )
```

![](15_from-parameter-estimation-to-hypothesis-testingt_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
x <- seq(0.1, 0.5, 0.001)
df <- bind_rows(
  tibble(
    email = "A",
    x = x,
    prob = dbeta(x, prior_alpha + 36, prior_beta + 114)
  ),
  tibble(
    email = "B",
    x = x,
    prob = dbeta(x, prior_alpha + 50, prior_beta + 100)
  ),
)
ggplot(df, aes(x = x, y = prob)) +
  geom_line(aes(color = email)) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(
    x = "conversion rate",
    y = "density",
    title = "Parameter estimation variants A and B",
    color = "email variant"
  )
```

![](15_from-parameter-estimation-to-hypothesis-testingt_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
n_trials <- 1000000
a_samples <- rbeta(n_trials, 36 + prior_alpha, 114 + prior_beta)
b_samples <- rbeta(n_trials, 50 + prior_alpha, 100 + prior_beta)
```

``` r
prob_b_superior <- mean(b_samples > a_samples)
prob_b_superior
```

    #> [1] 0.958924

``` r
df <- tibble(sample_ratios = b_samples / a_samples)

ggplot(df, aes(x = sample_ratios)) +
  geom_histogram(color = "black", alpha = 0.5, binwidth = 0.1) +
  labs(x = "B:A ratio", y = "frequency", title = "Histogram of B:A samples")
```

![](15_from-parameter-estimation-to-hypothesis-testingt_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
ratio_ecdf <- ecdf(df$sample_ratios)
ecdf_df <- tibble(x = seq(0.4, 3, 0.001), cdf = ratio_ecdf(x))

ggplot(ecdf_df, aes(x = x, y = cdf)) +
  geom_line() +
  geom_hline(yintercept = c(0, 1), linetype = 2, alpha = 0.2) +
  geom_hline(yintercept = c(0.25, 0.75), linetype = 2) +
  geom_hline(yintercept = c(0.5)) +
  labs(
    x = "improvement by switching to email B",
    y = "cumulative probability",
    title = "ECDF of B:A ratios"
  )
```

![](15_from-parameter-estimation-to-hypothesis-testingt_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

------------------------------------------------------------------------

## Exercises

**Question 3**

``` r
n_trials <- 100000
prior_a <- 3
prior_b <- 7
A_TRUE <- 0.25
B_TRUE <- 0.3

idx <- 1:500
prob_b_superior_record <- idx

for (i in idx) {
  a_success <- round(A_TRUE * i)
  b_success <- round(B_TRUE * i)
  a_samples <- rbeta(n_trials, a_success + prior_a, (i - a_success) + prior_b)
  b_samples <- rbeta(n_trials, b_success + prior_a, (i - b_success) + prior_b)
  prob_b_superior_record[i] <- mean(b_samples > a_samples)
}
```

``` r
df <- tibble(i = idx, prob_b_superior = prob_b_superior_record)

ggplot(df, aes(x = i, y = prob_b_superior)) +
  geom_line() +
  geom_hline(yintercept = 0.95, linetype = 2) +
  labs(
    x = "number of emails",
    y = "probability that B is superior",
    title = "Simulated results of email A/B tests"
  )
```

![](15_from-parameter-estimation-to-hypothesis-testingt_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
n_emails <- min(which(prob_b_superior_record > 0.95))
glue("required number of emails of each variant: {n_emails}")
```

    #> required number of emails of each variant: 425
