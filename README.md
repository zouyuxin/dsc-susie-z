# dsc-susie-z

This is a Dynamic Statistical Comparison
to test the SuSiE using summary statistics, z scores.

The goal is to test the performance of susie_z method under different context of effect.

# Data

We use the data X in the SuSiE package, N3finemapping.

We simulate data with PVE = 0.01, 0.05, 0.1, 0.2, 0.5, 0.8, 0.95. 

The number of true effects are 0,1, 2, 5, 10, 20. 

We fit the susie_z model with L = 5, 20.

We check whether the model converges in 100 iterations.

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
To input a customized design matrix X, please go to `susie_z_gaussian_benchmark.dsc`. At the bottom, change the `pathX` to the path of your matrix X as you want.
