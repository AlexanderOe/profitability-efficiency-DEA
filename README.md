# Profitability efficiency using DEA with trade-offs

Replication code for the paper

> Öttl, A. (2026). *Profitability Efficiency in Data Envelopment Analysis: A Trade-Off Based Approach with a Generalized Distance Function*. European Journal of Operational Research.

The scripts reproduce the numerical example, the illustration of allocative
efficiency and the empirical application to Taiwanese banks. All estimations use
the R package [`hyperbolicDEA`](https://CRAN.R-project.org/package=hyperbolicDEA).

## Contents

| File | Description |
|------|-------------|
| `numerical_example.R` | Numerical example with 6 DMUs, 3 inputs and 2 outputs. Computes technical, allocative and profitability efficiency under the trade-off matrices `T_1` (equivalent to classical profitability efficiency) and `T_2`, plus the optimal input/output quantities. |
| `Illustration_AE.R` | Two-input cost example that illustrates allocative efficiency through trade-offs/weight restrictions (Figure `AE_illustration.pdf`). |
| `AE_illustration.pdf` | Figure produced by `Illustration_AE.R`. |
| `taiwanese_bank_profitability.R` | Empirical application to 31 Taiwanese banks in 2010: descriptive statistics, efficiency scores, optimal quantities, equivalence check with the classical profitability model, and the additional analyses in the discussion section. |
| `Taiwanese_bank_data.csv` | Bank data (inputs, outputs and prices for 2006–2010; only 2010 is used). See [Data](#data). |

## Requirements

The code was run with R 4.4.1 and the following CRAN packages:

| Package | Version | Used in |
|---------|---------|---------|
| `hyperbolicDEA` | 1.0.2 | all scripts |
| `stargazer` | 5.2.3 | `taiwanese_bank_profitability.R` |

```r
install.packages(c("hyperbolicDEA", "stargazer"))
```

## Usage

Set the working directory to this folder and run any of the scripts, e.g.

```r
source("numerical_example.R", echo = TRUE)
source("taiwanese_bank_profitability.R", echo = TRUE)
source("Illustration_AE.R", echo = TRUE)
```

## Data

**Source.** The data on 31 Taiwanese banks were constructed by Juo et al.
(2015); see their article for the sources and the variable
definitions. Please cite the original source when
using the data:

> Juo, J.-C., Fu, T.-T., Yu, M.-M., & Lin, Y.-H. (2015). Profit-oriented
> productivity change. *Omega*, 57, 176–187.
> https://doi.org/10.1016/j.omega.2015.04.013

**Availability.** The dataset was kindly provided to me by José Luis Zofío and
Javier Barbero, and is publicly available as part of the replication material for

> Barbero, J., & Zofío, J. L. (2023). The measurement of profit, profitability,
> cost and revenue efficiency through data envelopment analysis: A comparison of
> models using BenchmarkingEconomicEfficiency.jl. *Socio-Economic Planning
> Sciences*, 89, 101656. https://doi.org/10.1016/j.seps.2023.101656

at https://github.com/joselzofio/Benchmarking_Economic_Efficiency_Julia_Notebooks
(`Article_SEPS/DataBanks.csv` for 2010; `DataBanks2years.csv` in the
`Chapter_EdwardElgar` folder for 2006–2010) and https://benchmarkingeconomicefficiency.com.

**File.** `Taiwanese_bank_data.csv` is a plain-text copy of the spreadsheet I
received, restricted to the 31 banks. The number of employees
(`x2`) contains small decimal deviations from integers in the raw data and is
rounded in the script.

Columns: `bank` (name), `id` (F1–F31), and each variable by year, e.g.
`x1(10)` is financial funds in 2010 (years 06–10 = 2006–2010).

| Column | Variable |
|--------|----------|
| `x1` | Financial funds |
| `x2` | Employees |
| `x3` | Physical capital |
| `y1` | Financial investments |
| `y2` | Loans |
| `w1`–`w3` | Input prices |
| `p1`–`p2` | Output prices |

## Contact

Alexander Öttl, University of Copenhagen — alexander@ifro.ku.dk
