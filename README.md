# Compound Poisson Process Analysis

## Project Description
Analysis of a Compound Poisson Process S(t) where:
- Arrival process follows Poisson(λ) with exponential interarrival times.
- Claim sizes are i.i.d. Exponential(μ) random variables.
- S(t) = ∑Xi represents the aggregate claims by time t.

## Mathematical Framework
**Compound Poisson Process Properties:**
- N(t) ~ Poisson(λt) - Number of arrivals by time t.
- Xi ~ Exponential(μ) - Individual claim sizes.
- S(t) ~ Compound Poisson distribution.
## Features
- **R Shiny Application**: Interactive visualization of S(t) distributions.
- **Time Evolution Analysis**: Histograms at t=10, 100, 1000, 10000.
- **Parameter Sensitivity**: Real-time analysis of λ and μ parameter impacts.
- **Theoretical Validation**: Comparison of simulated vs theoretical results.
