# Survival Analysis for Anti-Hypertensive Medication Usage

## Overview
This project investigates the association between the usage of anti-hypertensive medication and survival outcomes using statistical modelling and inference techniques. The analysis explores the relationship between medication usage and time-to-death based on a healthcare dataset.

## Key Features
- Kaplan-Meier plot visualization using ggplot2 to depict survival curves.
- Application of Cox proportional hazards models to investigate the association between medication usage and survival outcomes.
- Model selection using the Akaike Information Criterion (AIC) to identify the most relevant covariates.
- Formal hypothesis testing to assess differences in survival rates between patients receiving and not receiving anti-hypertensive medication.

## Data
The analysis utilizes the Framingham heart study dataset, containing 3826 observations and 19 variables

## Installation
To replicate the analysis, ensure you have R and the following packages installed:
- ggplot2
- survival

## Results
The analysis indicates a significant difference in survival outcomes between patients receiving and not receiving anti-hypertensive medication. The chosen model highlights the most relevant covariates affecting survival rates.
