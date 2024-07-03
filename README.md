
# ICO Success Prediction

This repository contains the code and analysis for predicting the success of Initial Coin Offerings (ICOs) using various machine learning techniques. The project aims to identify patterns and predictors that contribute to successful ICO campaigns, providing valuable insights for investors and developers.

## Project Overview

The rise of cryptocurrency has led to the emergence of ICOs as a new fundraising method. This project analyzes a dataset of 2,767 ICO projects, using features like team size, coin price, rating, platform, marketing presence, and campaign duration to predict the likelihood of an ICO reaching its fundraising goal.

## Dataset

The dataset includes 2,767 entries with 16 attributes, covering both numerical and categorical data. Key features include:

- `priceUSD`: Coin price in USD
- `teamSize`: Number of team members
- `rating`: Project rating out of 5
- `distributedPercentage`: Percentage of coins distributed to investors
- `hasVideo`, `hasGithub`, `hasReddit`: Indicators of promotional and informative content
- `countryRegion`: Geographical location of the project
- `platform`: Blockchain technology used

## Data Preparation

Data cleaning and preprocessing steps included:

- Handling missing values
- Creating new features like campaign duration
- Outlier detection and handling
- Log transformation for normalization

## Modeling

Four machine learning models were used:

1. **Decision Tree**
2. **Random Forest**
3. **K-Nearest Neighbors (KNN)**
4. **Logistic Regression**

## Evaluation

The models were evaluated based on accuracy, F1 score, and AUC values. The Random Forest and Logistic Regression models emerged as the most reliable predictors, demonstrating high accuracy and robustness.

## Results

| Model               | Accuracy | F1 Score | AUC  |
|---------------------|----------|----------|------|
| Decision Tree       | 0.6626   | 0.7746   | 0.66 |
| Random Forest       | 0.7065   | 0.7935   | 0.73 |
| K-Nearest Neighbors | 0.7004   | 0.7804   | 0.65 |
| Logistic Regression | 0.6979   | 0.7825   | 0.72 |

## Conclusion

The Random Forest and Logistic Regression models are effective tools for predicting ICO success, offering valuable guidance for investors and project developers to optimize their strategies and resource allocation.


