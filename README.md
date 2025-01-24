# Structural Equation Modeling: Short Dark Triad Survey

## Project Overview
This project applies Structural Equation Modeling (SEM) to analyze the Short Dark Triad survey ([Jones & Paulhus, 2013](https://www.researchgate.net/publication/259252435_Introducing_the_Short_Dark_Triad_SD3_A_Brief_Measure_of_Dark_Personality_Traits)), exploring relationships between dark personality traits and additional variables.

## Dataset
The dataset for this project is available on Kaggle: [Short Dark Triad Dataset](https://www.kaggle.com/datasets/yamqwe/short-dark-triad) 

## Key Features
- Analysis of Dark Triad personality traits (Machiavellianism, Narcissism, Psychopathy)
- Incorporation of custom variables: risk-taking behavior and gender
- Data cleaning and preliminary analyses
- SEM model construction and evaluation
- Multi-group SEM analysis using gender as the grouping variable

## Project Structure

### Part 1: Data Preparation and Preliminary Analyses
- Synthetic variable creation
- Missing data check
- Normality assessment:
  - Univariate normality (Shapiro-Wilk's test)
  - Multivariate normality (Mardia's test)

### Part 2: Initial SEM Model Construction
- Built the initial structural equation model
- Examined target indices for model fit

### Part 3: Multi-group SEM Analysis
- Conducted multi-group SEM using gender as the grouping variable
- Assessed and compared different levels of measurement invariance:
  - Metric invariance
  - Scalar invariance
  - Strict invariance

### Requirements
- R version 4.1.0 or later to use R package lavaangui

## How to Use
1. Clone the repository
2. Open the RMarkdown file in your preferred R environment
3. Run the code chunks sequentially
4. Alternatively, view the HTML or Markdown (.md) file for a quick overview of the results

## Feedback
Constructive feedback is always appreciated. Feel free to share any suggestions or improvements you might have.

