# R-R-replication
# Romer & Romer (2004) Replication and Extension

This project replicates the core empirical results of Romer, C.D. and Romer, D.H. (2004), "A New Measure of Monetary Shocks: Derivation and Implications," *American Economic Review*, 94(4), 1055-1084, and extends the analysis to 2007 using an updated shock series from Wieland, J.F. and Yang, M.C. (2020), "Financial Dampening," *Journal of Money, Credit and Banking*.

## What this project does

1. Reconstructs Romer and Romer's "narrative" measure of monetary policy shocks by regressing the change in the Fed's intended interest rate on the Fed's own internal economic forecasts, and taking the leftover residual as a shock
2. Uses that shock series to estimate the effect of monetary policy on industrial production and producer prices
3. Extends the sample from 1996 through 2007 using Wieland and Yang's updated shock series, and compares the results

## Key results

- Replicated shock series correlates at 1.000 with the original paper's published shock series
- Replicated output and price regressions match the original paper's coefficients, R-squared, and sample sizes
- The extended sample (1970-2007) shows a smaller peak effect of monetary shocks on both output and prices than the original 1970-1996 sample, consistent with the "Great Moderation" literature on declining monetary policy potency over this period

## Repository structure

- `scripts/` - R script covering the full replication and extension
- `data/` - Original data appendix from Romer and Romer (2004), sourced from the American Economic Association's data archive
- `output/` - Cumulative impulse response charts (output and price effects)

## Data sources

- Romer and Romer (2004) data appendix: AEA data archive
- Extended shock series (1997-2007): Wieland and Yang (2020), johanneswieland/RomerShocks on GitHub
- Industrial production and PPI data: FRED (series IPB50001N and WPUFD49207)

## Attribution

The original Romer and Romer (2004) data is used under a CC Attribution 4.0 International License. Code and data in this repository build on that original work; all credit for the underlying dataset and methodology belongs to Romer and Romer (2004) and Wieland and Yang (2020).
