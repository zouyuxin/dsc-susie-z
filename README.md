# dsc-susie-z

This is a Dynamic Statistical Comparison
to test the SuSiE using summary statistics, z scores.

# The goal:

The goal is to test the performance of susie_z method under different context of effect.

Methods will input:

  - $Y1 a numeric matrix of data from group 1 (p by n, p genes in columns, n samples/cells in rows)
  - $Y2 a numeric matrix of data from group 2 (p by n, p genes in columns, n samples/cells in rows)
  
and optionally:

  - $X1 an n vector of covariates from group 1 (eg "library size")
  - $X2 an n vector of covariates from group 2

Methods will output:

  - $log_fold_change_est a vector of estimates of log(mu1/mu2) for each gene where mu1 is the mean of group 1 and mu2 is the mean of group 2
  - $s_hat a vector of standard error for the estimated $log_fold_change
  - $p a p-vector of p values testing whether each log-fold change is 0

# Data

We use the data X in the SuSiE package.

To create data we will take a file containing data X and select
samples at random to create two groups. These will be "null" data.

Input:
  - file of data
  - n1 sample size for group 1
  - n2 sample size for group 2
  - p number of genes
  - pi0 proportion of nulls
  - g a distribution on non-zero effects

Output:
  - $Y1
  - $Y2
  - $log-fold-change (true value of log_fold_change for each gene)

# Performance evaluation criteria

List criteria we might want to use...

# Run DSC

The main DSC file is `susie_z_gaussian_benchmarks.dsc`. To see what is available:

```
dsc susie_z_gaussian_benchmark.dsc -h
```

before running,

```
rm -rf susie_z_gaussian_benchmark.scripts.html susie_z_gaussian_benchmark.html susie_z_gaussian_benchmark
```

and to run the benchmark:

```
dsc susie_z_gaussian_benchmark.dsc
```

Or to run a minimal test benchmark, eg

```
dsc susie_z_gaussian_benchmark.dsc --truncate
```

To run on the cluster:

```
dsc susie_z_gaussian_benchmark.dsc --host midway.yml --replicate 50 -c 40
```

# Input X
To input a customized design matrix X, please go to susie_z_gaussian_benchmark.dsc. At the bottom, change the pathX to the path of your matrix X as you want.
