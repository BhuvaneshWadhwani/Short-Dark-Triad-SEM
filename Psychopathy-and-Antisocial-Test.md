# Calling packages

    packages_to_use<- c("tidyverse", "dplyr", "mvnormalTest", "lavaan", "lavaangui", "readxl")

    for(i in packages_to_use){
      if( ! i %in% rownames(installed.packages())  ) {
        print(paste(i, "not installed; installing now:\n") )
        install.packages(i)
      }
      
      require(i, character.only = TRUE)
    }

    ## Loading required package: tidyverse

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
    ## Loading required package: mvnormalTest
    ## 
    ## Loading required package: lavaan
    ## 
    ## This is lavaan 0.6-19
    ## lavaan is FREE software! Please report any bugs.
    ## 
    ## Loading required package: lavaangui
    ## 
    ## This is lavaangui 0.2.1
    ## lavaangui is BETA software! Please report any bugs at https://github.com/karchjd/lavaangui/issues
    ## 
    ## Loading required package: readxl

# Take in data

    data <- read.csv("data.csv")
    head(data)

    ##   M1 M2 M3 M4 M5 M6 M7 M8 M9 N1 N2 N3 N4 N5 N6 N7 N8 N9 P1 P2 P3 P4 P5 P6 P7 P8
    ## 1  4  4  4  4  4  4  4  3  4  2  4  2  3  4  4  2  3  4  3  4  3  2  4  4  4  4
    ## 2  2  1  5  2  2  1  2  2  3  1  5  1  1  5  5  1  5  2  1  1  1  5  4  1  5  3
    ## 3  3  3  3  5  1  1  5  5  3  2  5  1  1  1  5  1  5  5  3  5  3  1  3  1  2  3
    ## 4  5  5  4  5  5  5  5  5  5  5  1  5  5  5  1  5  1  5  5  1  5  2  5  5  5  1
    ## 5  4  4  2  5  5  5  4  1  4  3  4  3  1  5  4  3  2  5  4  5  3  1  4  3  5  4
    ## 6  4  2  2  4  2  3  5  2  2  2  5  2  2  2  4  1  3  5  3  5  4  4  2  2  1  1
    ##   P9 country source
    ## 1  4      GB      1
    ## 2  2      US      1
    ## 3  1      US      1
    ## 4  5      GB      3
    ## 5  1      GB      3
    ## 6  5      IT      1

# Create synthetic variables

##### According to Jones and Paulhus (2013), psychopathy is a key predictor of reckless antisocial behavior. Hence, in our creation of risk\_taking variable, we can assign more weight to psychopathy.

    set.seed(111)
    machiavellianism <- rowSums(data[, 1:9])
    narcissism <- rowSums(data[, 10:18])  
    psychopathy <- rowSums(data[, 19:27])  

    # Create synthetic risk-taking variable
    ## According to Jones & Paulhus (2013), psyhopathy is a key predictor of reckless antisocial behavior
    risk_taking <- 0.5 * psychopathy + 0.3 * narcissism + 0.2 * machiavellianism + rnorm(nrow(data), mean = 0, sd = 1)

    # Add to dataset
    data$risk_taking <- risk_taking

    # Drop country and source
    data <- data[, !(names(data) %in% c("country", "source"))]

    # Create a new variable 'gender' with random 0 or 1
    # 0 = female, 1 = male
    set.seed(111)
    data$gender <- sample(0:1, nrow(data), replace = TRUE)

    head(data)

    ##   M1 M2 M3 M4 M5 M6 M7 M8 M9 N1 N2 N3 N4 N5 N6 N7 N8 N9 P1 P2 P3 P4 P5 P6 P7 P8
    ## 1  4  4  4  4  4  4  4  3  4  2  4  2  3  4  4  2  3  4  3  4  3  2  4  4  4  4
    ## 2  2  1  5  2  2  1  2  2  3  1  5  1  1  5  5  1  5  2  1  1  1  5  4  1  5  3
    ## 3  3  3  3  5  1  1  5  5  3  2  5  1  1  1  5  1  5  5  3  5  3  1  3  1  2  3
    ## 4  5  5  4  5  5  5  5  5  5  5  1  5  5  5  1  5  1  5  5  1  5  2  5  5  5  1
    ## 5  4  4  2  5  5  5  4  1  4  3  4  3  1  5  4  3  2  5  4  5  3  1  4  3  5  4
    ## 6  4  2  2  4  2  3  5  2  2  2  5  2  2  2  4  1  3  5  3  5  4  4  2  2  1  1
    ##   P9 risk_taking gender
    ## 1  4    31.63522      1
    ## 2  2    22.96926      0
    ## 3  1    24.28838      1
    ## 4  5    33.39765      0
    ## 5  1    30.62912      0
    ## 6  5    26.64028      0

# Check for missing data

    missing_data <- colSums(is.na(data))
    print(missing_data) 

    ##          M1          M2          M3          M4          M5          M6 
    ##           0           0           0           0           0           0 
    ##          M7          M8          M9          N1          N2          N3 
    ##           0           0           0           0           0           0 
    ##          N4          N5          N6          N7          N8          N9 
    ##           0           0           0           0           0           0 
    ##          P1          P2          P3          P4          P5          P6 
    ##           0           0           0           0           0           0 
    ##          P7          P8          P9 risk_taking      gender 
    ##           0           0           0           0           0

    # no missing data

# Drop rows

    # Currently, the dataset is extremely large. Let's select 5000 rows to work with.
    set.seed(111)
    data <- data[sample(nrow(data), 5000), ]

    nrow(data)

    ## [1] 5000

# Assess normality of variance

    filtered_data <- data %>%
      select(M1:P9)

    mvnout <- mardia(filtered_data)

    ## Shapiro-Wilk Univariate normality test
    mvnout$uv.shapiro

    ##    W      p-value UV.Normality
    ## M1 0.7748 0       No          
    ## M2 0.8825 0       No          
    ## M3 0.907  0       No          
    ## M4 0.8806 0       No          
    ## M5 0.864  0       No          
    ## M6 0.8541 0       No          
    ## M7 0.6974 0       No          
    ## M8 0.9141 0       No          
    ## M9 0.8029 0       No          
    ## N1 0.9109 0       No          
    ## N2 0.9075 0       No          
    ## N3 0.906  0       No          
    ## N4 0.9047 0       No          
    ## N5 0.8925 0       No          
    ## N6 0.9008 0       No          
    ## N7 0.9055 0       No          
    ## N8 0.8877 0       No          
    ## N9 0.9033 0       No          
    ## P1 0.9113 0       No          
    ## P2 0.9053 0       No          
    ## P3 0.8875 0       No          
    ## P4 0.8611 0       No          
    ## P5 0.8559 0       No          
    ## P6 0.9167 0       No          
    ## P7 0.8337 0       No          
    ## P8 0.8554 0       No          
    ## P9 0.8881 0       No

    ## Mardia Multivariate normality test
    mvnout$mv.test

    ##           Test  Statistic p-value Result
    ## 1     Skewness 17358.5595       0     NO
    ## 2     Kurtosis    95.1459       0     NO
    ## 3 MV Normality       <NA>    <NA>     NO

    # Results from univariate and multivariate tests indicate that the measures do not come from a normally distributed univariate or multivariate distributions. This will be addressed in model specification stage.

# Build SEM model

    # Define the model
    model <- '
    # Measurement Model
    machiavellianism =~ M1 + M2 + M3 + M4 + M5 + M6 + M7 + M8 + M9
    narcissism =~ N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9
    psychopathy =~ P1 + P2 + P3 + P4 + P5 + P6 + P7 + P8 + P9


    # Structural Model
    risk_taking ~ machiavellianism + narcissism + psychopathy
    '
    # Fit the SEM model
    fit <- sem(model, data = data, estimator = "MLR") # maximum likelihood method

# Threshold used

##### CFI: &gt;=0.95 for good fit

##### TLI: &gt;=0.95 for good fit

##### RMSEA: &lt;= .05 close fit, .05 - .08 reasonable fit, &gt;= .10 poor fit

##### SRMR: &lt;=0.08 for good fit

# SEM Results

    summary(fit, fit.measures = TRUE, standardized = TRUE)

    ## lavaan 0.6-19 ended normally after 78 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        61
    ## 
    ##   Number of observations                          5000
    ## 
    ## Model Test User Model:
    ##                                                Standard      Scaled
    ##   Test Statistic                              13750.285   12090.959
    ##   Degrees of freedom                                345         345
    ##   P-value (Chi-square)                            0.000       0.000
    ##   Scaling correction factor                                   1.137
    ##     Yuan-Bentler correction (Mplus variant)                        
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                             63333.769   54677.509
    ##   Degrees of freedom                               378         378
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.158
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.787       0.784
    ##   Tucker-Lewis Index (TLI)                       0.767       0.763
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.788
    ##   Robust Tucker-Lewis Index (TLI)                            0.767
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)            -208484.795 -208484.795
    ##   Scaling correction factor                                  1.090
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)    -201609.653 -201609.653
    ##   Scaling correction factor                                  1.130
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                              417091.590  417091.590
    ##   Bayesian (BIC)                            417489.139  417489.139
    ##   Sample-size adjusted Bayesian (SABIC)     417295.302  417295.302
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.088       0.083
    ##   90 Percent confidence interval - lower         0.087       0.081
    ##   90 Percent confidence interval - upper         0.089       0.084
    ##   P-value H_0: RMSEA <= 0.050                    0.000       0.000
    ##   P-value H_0: RMSEA >= 0.080                    1.000       1.000
    ##                                                                   
    ##   Robust RMSEA                                               0.088
    ##   90 Percent confidence interval - lower                     0.087
    ##   90 Percent confidence interval - upper                     0.089
    ##   P-value H_0: Robust RMSEA <= 0.050                         0.000
    ##   P-value H_0: Robust RMSEA >= 0.080                         1.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.065       0.065
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism =~                                                      
    ##     M1                   1.000                               0.472    0.492
    ##     M2                   2.013    0.066   30.417    0.000    0.950    0.748
    ##     M3                   1.706    0.058   29.457    0.000    0.805    0.665
    ##     M4                   1.111    0.046   24.318    0.000    0.525    0.441
    ##     M5                   2.205    0.070   31.716    0.000    1.041    0.793
    ##     M6                   2.004    0.065   31.021    0.000    0.946    0.745
    ##     M7                   0.947    0.031   30.235    0.000    0.447    0.513
    ##     M8                   1.769    0.059   30.163    0.000    0.835    0.673
    ##     M9                   1.328    0.047   28.381    0.000    0.627    0.631
    ##   narcissism =~                                                            
    ##     N1                   1.000                               0.620    0.540
    ##     N2                  -0.830    0.038  -21.611    0.000   -0.515   -0.395
    ##     N3                   1.242    0.035   35.823    0.000    0.771    0.663
    ##     N4                   1.171    0.038   30.913    0.000    0.726    0.595
    ##     N5                   1.153    0.038   30.638    0.000    0.715    0.613
    ##     N6                  -0.800    0.040  -20.041    0.000   -0.496   -0.379
    ##     N7                   1.202    0.038   31.819    0.000    0.746    0.578
    ##     N8                  -0.991    0.040  -24.653    0.000   -0.615   -0.474
    ##     N9                   1.201    0.041   29.355    0.000    0.745    0.630
    ##   psychopathy =~                                                           
    ##     P1                   1.000                               0.823    0.652
    ##     P2                  -0.316    0.027  -11.625    0.000   -0.260   -0.214
    ##     P3                   0.978    0.022   44.619    0.000    0.805    0.621
    ##     P4                   0.722    0.022   33.090    0.000    0.594    0.473
    ##     P5                   0.848    0.021   40.100    0.000    0.698    0.603
    ##     P6                   1.084    0.020   53.095    0.000    0.892    0.719
    ##     P7                  -0.123    0.032   -3.854    0.000   -0.101   -0.070
    ##     P8                   0.844    0.025   34.081    0.000    0.694    0.476
    ##     P9                   1.253    0.024   52.643    0.000    1.031    0.748
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   risk_taking ~                                                         
    ##     machiavellinsm    1.151    0.383    3.003    0.003    0.543    0.112
    ##     narcissism        0.086    0.134    0.643    0.520    0.053    0.011
    ##     psychopathy       5.105    0.295   17.285    0.000    4.199    0.864
    ## 
    ## Covariances:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism ~~                                                      
    ##     narcissism           0.198    0.009   22.315    0.000    0.677    0.677
    ##     psychopathy          0.354    0.014   26.047    0.000    0.911    0.911
    ##   narcissism ~~                                                            
    ##     psychopathy          0.382    0.014   27.935    0.000    0.748    0.748
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1                0.696    0.021   33.281    0.000    0.696    0.757
    ##    .M2                0.712    0.019   37.760    0.000    0.712    0.441
    ##    .M3                0.818    0.020   41.844    0.000    0.818    0.558
    ##    .M4                1.141    0.024   47.499    0.000    1.141    0.806
    ##    .M5                0.639    0.018   36.279    0.000    0.639    0.371
    ##    .M6                0.715    0.020   35.813    0.000    0.715    0.444
    ##    .M7                0.559    0.021   26.083    0.000    0.559    0.737
    ##    .M8                0.842    0.019   43.761    0.000    0.842    0.547
    ##    .M9                0.595    0.018   33.730    0.000    0.595    0.602
    ##    .N1                0.934    0.020   46.198    0.000    0.934    0.708
    ##    .N2                1.438    0.028   51.197    0.000    1.438    0.844
    ##    .N3                0.756    0.019   38.918    0.000    0.756    0.560
    ##    .N4                0.964    0.023   42.221    0.000    0.964    0.646
    ##    .N5                0.852    0.021   40.297    0.000    0.852    0.625
    ##    .N6                1.465    0.028   53.127    0.000    1.465    0.856
    ##    .N7                1.107    0.024   46.818    0.000    1.107    0.666
    ##    .N8                1.302    0.029   44.958    0.000    1.302    0.775
    ##    .N9                0.842    0.021   39.377    0.000    0.842    0.603
    ##    .P1                0.914    0.020   45.534    0.000    0.914    0.574
    ##    .P2                1.413    0.023   62.701    0.000    1.413    0.954
    ##    .P3                1.032    0.021   48.535    0.000    1.032    0.614
    ##    .P4                1.224    0.024   51.559    0.000    1.224    0.776
    ##    .P5                0.853    0.020   43.326    0.000    0.853    0.636
    ##    .P6                0.742    0.017   44.154    0.000    0.742    0.483
    ##    .P7                2.068    0.027   78.015    0.000    2.068    0.995
    ##    .P8                1.646    0.027   60.367    0.000    1.646    0.774
    ##    .P9                0.837    0.019   44.422    0.000    0.837    0.441
    ##    .risk_taking       1.177    0.137    8.579    0.000    1.177    0.050
    ##     machiavellinsm    0.223    0.014   15.977    0.000    1.000    1.000
    ##     narcissism        0.385    0.020   18.938    0.000    1.000    1.000
    ##     psychopathy       0.677    0.024   27.857    0.000    1.000    1.000

    # Chi-square Test (Satorra-Bentler scaled): Significant misfit (p < 0.001). 
    # However, the Chi-square test is highly sensitive to large sample sizes and minor model misspecifications. 
    # Therefore, we rely on additional fit indices for a comprehensive evaluation.

    # Robust CFI: 0.784 (indicates poor fit; acceptable threshold is typically ≥ 0.90).

    # Robust TLI: 0.763 (indicates poor fit; acceptable threshold is typically ≥ 0.90).

    # RMSEA: 0.083 (indicates poor fit; values below 0.06–0.08 are considered acceptable).

    # SRMR: 0.065 (indicates poor fit; values below 0.08 are considered acceptable).

    # Overall Model Fit: Based on these fit indices, the model demonstrates poor fit to the data.

    # Latent Variables: All factor loadings are statistically significant (p < 0.001), 
    # suggesting that the observed indicators reliably measure their respective latent constructs.

    # Covariances: All relationships between latent variables are statistically significant (p < 0.001), 
    # indicating meaningful connections between the constructs in the model.

    parameterEstimates(fit, standardized = TRUE, rsquare = TRUE) %>% 
      filter(op == "r2") %>% 
      select(Item = rhs, R2 = est) 

    ##           Item    R2
    ## 1           M1 0.243
    ## 2           M2 0.559
    ## 3           M3 0.442
    ## 4           M4 0.194
    ## 5           M5 0.629
    ## 6           M6 0.556
    ## 7           M7 0.263
    ## 8           M8 0.453
    ## 9           M9 0.398
    ## 10          N1 0.292
    ## 11          N2 0.156
    ## 12          N3 0.440
    ## 13          N4 0.354
    ## 14          N5 0.375
    ## 15          N6 0.144
    ## 16          N7 0.334
    ## 17          N8 0.225
    ## 18          N9 0.397
    ## 19          P1 0.426
    ## 20          P2 0.046
    ## 21          P3 0.386
    ## 22          P4 0.224
    ## 23          P5 0.364
    ## 24          P6 0.517
    ## 25          P7 0.005
    ## 26          P8 0.226
    ## 27          P9 0.559
    ## 28 risk_taking 0.950

    # R2 values are below .50 for some variables, indicating poor relationships with latent variables

# Plot SEM model

    # This code uses lavaangui, which is a relatively new feature. Make sure that R is version 4.1.0 or later to use this.

    #plot_lavaan(fit) # This opens an interactive window.

    #lavaangui(fit) # This opens an interactive web application.

# Check for multicollinearity

    # covariances among latent variables
    inspect(fit, "cor.lv")

    ##                  mchvll nrcsss psychp
    ## machiavellianism  1.000              
    ## narcissism        0.677  1.000       
    ## psychopathy       0.911  0.748  1.000

    # psychopathy and machiavellianism have very highly correlated

# Multigroup SEM

##### 0 = Female, 1 = Male

    fit_configural <- sem(model, data = data, group = "gender")

    summary(fit_configural, fit.measures = TRUE, standardized = TRUE)

    ## lavaan 0.6-19 ended normally after 156 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                       178
    ## 
    ##   Number of observations per group:                   
    ##     1                                             2502
    ##     0                                             2498
    ## 
    ## Model Test User Model:
    ##                                                        
    ##   Test statistic                              14119.636
    ##   Degrees of freedom                                690
    ##   P-value (Chi-square)                            0.000
    ##   Test statistic for each group:
    ##     1                                         6932.680
    ##     0                                         7186.956
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                             63745.498
    ##   Degrees of freedom                               756
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.787
    ##   Tucker-Lewis Index (TLI)                       0.766
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)            -208445.664
    ##   Loglikelihood unrestricted model (H1)    -201385.847
    ##                                                       
    ##   Akaike (AIC)                              417247.329
    ##   Bayesian (BIC)                            418407.389
    ##   Sample-size adjusted Bayesian (SABIC)     417841.767
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.088
    ##   90 Percent confidence interval - lower         0.087
    ##   90 Percent confidence interval - upper         0.090
    ##   P-value H_0: RMSEA <= 0.050                    0.000
    ##   P-value H_0: RMSEA >= 0.080                    1.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.063
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## 
    ## Group 1 [1]:
    ## 
    ## Latent Variables:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism =~                                                      
    ##     M1                   1.000                               0.457    0.477
    ##     M2                   2.061    0.088   23.334    0.000    0.941    0.741
    ##     M3                   1.779    0.080   22.226    0.000    0.813    0.666
    ##     M4                   1.127    0.066   17.174    0.000    0.515    0.430
    ##     M5                   2.296    0.095   24.079    0.000    1.049    0.801
    ##     M6                   2.092    0.089   23.525    0.000    0.955    0.756
    ##     M7                   0.965    0.051   19.072    0.000    0.441    0.504
    ##     M8                   1.847    0.083   22.355    0.000    0.844    0.674
    ##     M9                   1.370    0.064   21.561    0.000    0.626    0.626
    ##   narcissism =~                                                            
    ##     N1                   1.000                               0.617    0.541
    ##     N2                  -0.864    0.052  -16.735    0.000   -0.533   -0.409
    ##     N3                   1.229    0.053   23.248    0.000    0.758    0.655
    ##     N4                   1.219    0.055   22.295    0.000    0.752    0.611
    ##     N5                   1.155    0.052   22.240    0.000    0.713    0.609
    ##     N6                  -0.826    0.051  -16.232    0.000   -0.509   -0.394
    ##     N7                   1.200    0.056   21.531    0.000    0.740    0.579
    ##     N8                  -1.066    0.054  -19.783    0.000   -0.658   -0.511
    ##     N9                   1.179    0.052   22.535    0.000    0.727    0.622
    ##   psychopathy =~                                                           
    ##     P1                   1.000                               0.808    0.647
    ##     P2                  -0.316    0.031  -10.256    0.000   -0.255   -0.210
    ##     P3                   0.981    0.035   28.046    0.000    0.793    0.610
    ##     P4                   0.712    0.033   21.739    0.000    0.575    0.460
    ##     P5                   0.842    0.031   27.267    0.000    0.680    0.591
    ##     P6                   1.079    0.034   32.174    0.000    0.872    0.717
    ##     P7                  -0.063    0.036   -1.759    0.079   -0.051   -0.036
    ##     P8                   0.860    0.038   22.357    0.000    0.695    0.474
    ##     P9                   1.270    0.038   33.172    0.000    1.026    0.744
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   risk_taking ~                                                         
    ##     machiavellinsm    0.542    0.511    1.060    0.289    0.247    0.051
    ##     narcissism       -0.100    0.170   -0.591    0.555   -0.062   -0.013
    ##     psychopathy       5.602    0.377   14.842    0.000    4.528    0.941
    ## 
    ## Covariances:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism ~~                                                      
    ##     narcissism           0.189    0.012   15.564    0.000    0.671    0.671
    ##     psychopathy          0.338    0.018   18.380    0.000    0.914    0.914
    ##   narcissism ~~                                                            
    ##     psychopathy          0.374    0.021   18.201    0.000    0.750    0.750
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1                4.180    0.019  218.395    0.000    4.180    4.366
    ##    .M2                3.419    0.025  134.662    0.000    3.419    2.692
    ##    .M3                3.335    0.024  136.661    0.000    3.335    2.732
    ##    .M4                3.494    0.024  145.996    0.000    3.494    2.919
    ##    .M5                3.523    0.026  134.503    0.000    3.523    2.689
    ##    .M6                3.646    0.025  144.227    0.000    3.646    2.883
    ##    .M7                4.381    0.017  250.811    0.000    4.381    5.014
    ##    .M8                3.134    0.025  125.238    0.000    3.134    2.504
    ##    .M9                4.062    0.020  203.382    0.000    4.062    4.066
    ##    .N1                3.256    0.023  142.709    0.000    3.256    2.853
    ##    .N2                3.129    0.026  120.125    0.000    3.129    2.402
    ##    .N3                2.635    0.023  113.866    0.000    2.635    2.276
    ##    .N4                2.650    0.025  107.733    0.000    2.650    2.154
    ##    .N5                3.484    0.023  148.805    0.000    3.484    2.975
    ##    .N6                3.060    0.026  118.321    0.000    3.060    2.365
    ##    .N7                2.864    0.026  112.001    0.000    2.864    2.239
    ##    .N8                2.574    0.026  100.018    0.000    2.574    2.000
    ##    .N9                3.416    0.023  146.097    0.000    3.416    2.921
    ##    .P1                3.080    0.025  123.366    0.000    3.080    2.466
    ##    .P2                3.219    0.024  132.479    0.000    3.219    2.649
    ##    .P3                2.582    0.026   99.365    0.000    2.582    1.986
    ##    .P4                2.289    0.025   91.505    0.000    2.289    1.829
    ##    .P5                3.669    0.023  159.417    0.000    3.669    3.187
    ##    .P6                2.904    0.024  119.369    0.000    2.904    2.386
    ##    .P7                3.567    0.029  124.948    0.000    3.567    2.498
    ##    .P8                2.589    0.029   88.273    0.000    2.589    1.765
    ##    .P9                2.753    0.028   99.781    0.000    2.753    1.995
    ##    .risk_taking      28.100    0.096  292.156    0.000   28.100    5.841
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1                0.708    0.021   34.497    0.000    0.708    0.772
    ##    .M2                0.727    0.023   31.702    0.000    0.727    0.451
    ##    .M3                0.829    0.025   32.991    0.000    0.829    0.557
    ##    .M4                1.168    0.034   34.700    0.000    1.168    0.815
    ##    .M5                0.616    0.021   29.934    0.000    0.616    0.359
    ##    .M6                0.686    0.022   31.356    0.000    0.686    0.429
    ##    .M7                0.569    0.017   34.359    0.000    0.569    0.746
    ##    .M8                0.855    0.026   32.883    0.000    0.855    0.546
    ##    .M9                0.606    0.018   33.447    0.000    0.606    0.608
    ##    .N1                0.922    0.028   32.633    0.000    0.922    0.708
    ##    .N2                1.413    0.042   34.042    0.000    1.413    0.833
    ##    .N3                0.765    0.025   30.366    0.000    0.765    0.571
    ##    .N4                0.948    0.030   31.407    0.000    0.948    0.626
    ##    .N5                0.863    0.027   31.458    0.000    0.863    0.629
    ##    .N6                1.414    0.041   34.158    0.000    1.414    0.845
    ##    .N7                1.088    0.034   32.029    0.000    1.088    0.665
    ##    .N8                1.225    0.037   33.031    0.000    1.225    0.739
    ##    .N9                0.839    0.027   31.178    0.000    0.839    0.613
    ##    .P1                0.907    0.027   34.193    0.000    0.907    0.581
    ##    .P2                1.412    0.040   35.314    0.000    1.412    0.956
    ##    .P3                1.061    0.031   34.450    0.000    1.061    0.628
    ##    .P4                1.235    0.035   35.010    0.000    1.235    0.789
    ##    .P5                0.863    0.025   34.557    0.000    0.863    0.651
    ##    .P6                0.720    0.022   33.420    0.000    0.720    0.486
    ##    .P7                2.037    0.058   35.368    0.000    2.037    0.999
    ##    .P8                1.669    0.048   34.977    0.000    1.669    0.775
    ##    .P9                0.851    0.026   32.946    0.000    0.851    0.447
    ##    .risk_taking       0.973    0.160    6.073    0.000    0.973    0.042
    ##     machiavellinsm    0.209    0.017   12.223    0.000    1.000    1.000
    ##     narcissism        0.381    0.028   13.495    0.000    1.000    1.000
    ##     psychopathy       0.653    0.037   17.885    0.000    1.000    1.000
    ## 
    ## 
    ## Group 2 [0]:
    ## 
    ## Latent Variables:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism =~                                                      
    ##     M1                   1.000                               0.487    0.508
    ##     M2                   1.967    0.078   25.100    0.000    0.958    0.754
    ##     M3                   1.635    0.070   23.510    0.000    0.797    0.663
    ##     M4                   1.098    0.060   18.436    0.000    0.535    0.452
    ##     M5                   2.119    0.083   25.585    0.000    1.033    0.786
    ##     M6                   1.922    0.077   24.806    0.000    0.937    0.736
    ##     M7                   0.931    0.046   20.363    0.000    0.453    0.522
    ##     M8                   1.697    0.072   23.691    0.000    0.827    0.673
    ##     M9                   1.289    0.056   22.952    0.000    0.628    0.635
    ##   narcissism =~                                                            
    ##     N1                   1.000                               0.625    0.541
    ##     N2                  -0.794    0.050  -15.725    0.000   -0.496   -0.379
    ##     N3                   1.253    0.053   23.541    0.000    0.783    0.671
    ##     N4                   1.121    0.052   21.478    0.000    0.701    0.578
    ##     N5                   1.147    0.051   22.366    0.000    0.717    0.616
    ##     N6                  -0.772    0.051  -15.237    0.000   -0.483   -0.365
    ##     N7                   1.201    0.056   21.465    0.000    0.751    0.577
    ##     N8                  -0.916    0.052  -17.647    0.000   -0.572   -0.439
    ##     N9                   1.221    0.053   22.869    0.000    0.763    0.639
    ##   psychopathy =~                                                           
    ##     P1                   1.000                               0.837    0.658
    ##     P2                  -0.314    0.030  -10.469    0.000   -0.263   -0.216
    ##     P3                   0.975    0.034   29.056    0.000    0.816    0.631
    ##     P4                   0.731    0.032   22.883    0.000    0.612    0.486
    ##     P5                   0.854    0.030   28.364    0.000    0.715    0.615
    ##     P6                   1.088    0.033   32.660    0.000    0.911    0.722
    ##     P7                  -0.181    0.036   -5.067    0.000   -0.151   -0.104
    ##     P8                   0.828    0.037   22.539    0.000    0.693    0.478
    ##     P9                   1.236    0.037   33.812    0.000    1.035    0.752
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   risk_taking ~                                                         
    ##     machiavellinsm    1.667    0.384    4.341    0.000    0.812    0.165
    ##     narcissism        0.237    0.144    1.648    0.099    0.148    0.030
    ##     psychopathy       4.674    0.285   16.397    0.000    3.913    0.797
    ## 
    ## Covariances:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism ~~                                                      
    ##     narcissism           0.208    0.013   16.067    0.000    0.683    0.683
    ##     psychopathy          0.370    0.019   19.166    0.000    0.908    0.908
    ##   narcissism ~~                                                            
    ##     psychopathy          0.391    0.021   18.249    0.000    0.747    0.747
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1                4.178    0.019  217.639    0.000    4.178    4.355
    ##    .M2                3.484    0.025  136.978    0.000    3.484    2.741
    ##    .M3                3.356    0.024  139.638    0.000    3.356    2.794
    ##    .M4                3.499    0.024  147.850    0.000    3.499    2.958
    ##    .M5                3.582    0.026  136.225    0.000    3.582    2.726
    ##    .M6                3.659    0.025  143.688    0.000    3.659    2.875
    ##    .M7                4.391    0.017  252.779    0.000    4.391    5.058
    ##    .M8                3.146    0.025  127.934    0.000    3.146    2.560
    ##    .M9                4.069    0.020  205.741    0.000    4.069    4.116
    ##    .N1                3.243    0.023  140.267    0.000    3.243    2.806
    ##    .N2                3.097    0.026  118.379    0.000    3.097    2.369
    ##    .N3                2.654    0.023  113.751    0.000    2.654    2.276
    ##    .N4                2.626    0.024  108.254    0.000    2.626    2.166
    ##    .N5                3.540    0.023  152.048    0.000    3.540    3.042
    ##    .N6                3.069    0.026  115.990    0.000    3.069    2.321
    ##    .N7                2.885    0.026  110.890    0.000    2.885    2.219
    ##    .N8                2.622    0.026  100.458    0.000    2.622    2.010
    ##    .N9                3.428    0.024  143.445    0.000    3.428    2.870
    ##    .P1                3.045    0.025  119.561    0.000    3.045    2.392
    ##    .P2                3.199    0.024  131.298    0.000    3.199    2.627
    ##    .P3                2.594    0.026  100.361    0.000    2.594    2.008
    ##    .P4                2.323    0.025   92.189    0.000    2.323    1.845
    ##    .P5                3.717    0.023  159.686    0.000    3.717    3.195
    ##    .P6                2.956    0.025  117.086    0.000    2.956    2.343
    ##    .P7                3.533    0.029  121.352    0.000    3.533    2.428
    ##    .P8                2.594    0.029   89.410    0.000    2.594    1.789
    ##    .P9                2.783    0.028  101.057    0.000    2.783    2.022
    ##    .risk_taking      28.186    0.098  286.765    0.000   28.186    5.738
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1                0.683    0.020   34.331    0.000    0.683    0.742
    ##    .M2                0.697    0.022   31.433    0.000    0.697    0.431
    ##    .M3                0.808    0.024   33.038    0.000    0.808    0.560
    ##    .M4                1.113    0.032   34.595    0.000    1.113    0.796
    ##    .M5                0.661    0.022   30.528    0.000    0.661    0.383
    ##    .M6                0.743    0.023   31.849    0.000    0.743    0.458
    ##    .M7                0.548    0.016   34.250    0.000    0.548    0.727
    ##    .M8                0.827    0.025   32.914    0.000    0.827    0.547
    ##    .M9                0.583    0.017   33.363    0.000    0.583    0.597
    ##    .N1                0.945    0.029   32.573    0.000    0.945    0.708
    ##    .N2                1.464    0.043   34.220    0.000    1.464    0.856
    ##    .N3                0.747    0.025   29.811    0.000    0.747    0.549
    ##    .N4                0.979    0.031   31.978    0.000    0.979    0.666
    ##    .N5                0.840    0.027   31.234    0.000    0.840    0.621
    ##    .N6                1.516    0.044   34.316    0.000    1.516    0.867
    ##    .N7                1.127    0.035   31.988    0.000    1.127    0.667
    ##    .N8                1.374    0.041   33.750    0.000    1.374    0.808
    ##    .N9                0.844    0.028   30.702    0.000    0.844    0.592
    ##    .P1                0.919    0.027   33.832    0.000    0.919    0.567
    ##    .P2                1.414    0.040   35.260    0.000    1.414    0.953
    ##    .P3                1.004    0.029   34.059    0.000    1.004    0.601
    ##    .P4                1.212    0.035   34.794    0.000    1.212    0.764
    ##    .P5                0.842    0.025   34.184    0.000    0.842    0.622
    ##    .P6                0.762    0.023   33.022    0.000    0.762    0.479
    ##    .P7                2.095    0.059   35.323    0.000    2.095    0.989
    ##    .P8                1.623    0.047   34.819    0.000    1.623    0.772
    ##    .P9                0.823    0.025   32.441    0.000    0.823    0.434
    ##    .risk_taking       1.339    0.132   10.168    0.000    1.339    0.055
    ##     machiavellinsm    0.237    0.018   13.125    0.000    1.000    1.000
    ##     narcissism        0.391    0.029   13.479    0.000    1.000    1.000
    ##     psychopathy       0.701    0.039   18.178    0.000    1.000    1.000

    # CFI: 0.787 poor fit
    # TLI: 0.766 poor fit
    # RMSEA: 0.088 poor fit
    # SRMR: 0.063 poor fit

## Metric Invariance

### Test whether factor loadings are equal across groups

    fit_metric <- sem(model, data = data, group = "gender", group.equal = "loadings")

    summary(fit_metric, fit.measures = TRUE, standardized = TRUE)

    ## lavaan 0.6-19 ended normally after 136 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                       178
    ##   Number of equality constraints                    24
    ## 
    ##   Number of observations per group:                   
    ##     1                                             2502
    ##     0                                             2498
    ## 
    ## Model Test User Model:
    ##                                                        
    ##   Test statistic                              14142.303
    ##   Degrees of freedom                                714
    ##   P-value (Chi-square)                            0.000
    ##   Test statistic for each group:
    ##     1                                         6943.840
    ##     0                                         7198.463
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                             63745.498
    ##   Degrees of freedom                               756
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.787
    ##   Tucker-Lewis Index (TLI)                       0.774
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)            -208456.998
    ##   Loglikelihood unrestricted model (H1)    -201385.847
    ##                                                       
    ##   Akaike (AIC)                              417221.996
    ##   Bayesian (BIC)                            418225.644
    ##   Sample-size adjusted Bayesian (SABIC)     417736.285
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.087
    ##   90 Percent confidence interval - lower         0.085
    ##   90 Percent confidence interval - upper         0.088
    ##   P-value H_0: RMSEA <= 0.050                    0.000
    ##   P-value H_0: RMSEA >= 0.080                    1.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.064
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## 
    ## Group 1 [1]:
    ## 
    ## Latent Variables:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism =~                                                      
    ##     M1                   1.000                               0.473    0.490
    ##     M2      (.p2.)       2.012    0.059   34.250    0.000    0.951    0.745
    ##     M3      (.p3.)       1.705    0.053   32.361    0.000    0.806    0.663
    ##     M4      (.p4.)       1.111    0.044   25.179    0.000    0.525    0.437
    ##     M5      (.p5.)       2.205    0.063   35.145    0.000    1.042    0.798
    ##     M6      (.p6.)       2.004    0.059   34.212    0.000    0.947    0.752
    ##     M7      (.p7.)       0.947    0.034   27.885    0.000    0.448    0.511
    ##     M8      (.p8.)       1.769    0.054   32.575    0.000    0.836    0.670
    ##     M9      (.p9.)       1.328    0.042   31.484    0.000    0.628    0.628
    ##   narcissism =~                                                            
    ##     N1                   1.000                               0.622    0.544
    ##     N2      (.11.)      -0.830    0.036  -22.985    0.000   -0.516   -0.398
    ##     N3      (.12.)       1.242    0.038   33.064    0.000    0.772    0.663
    ##     N4      (.13.)       1.171    0.038   30.964    0.000    0.728    0.597
    ##     N5      (.14.)       1.152    0.037   31.535    0.000    0.717    0.611
    ##     N6      (.15.)      -0.800    0.036  -22.273    0.000   -0.498   -0.385
    ##     N7      (.16.)       1.201    0.040   30.397    0.000    0.747    0.582
    ##     N8      (.17.)      -0.995    0.037  -26.572    0.000   -0.619   -0.486
    ##     N9      (.18.)       1.200    0.037   32.079    0.000    0.746    0.633
    ##   psychopathy =~                                                           
    ##     P1                   1.000                               0.811    0.649
    ##     P2      (.20.)      -0.315    0.021  -14.666    0.000   -0.256   -0.211
    ##     P3      (.21.)       0.978    0.024   40.384    0.000    0.793    0.611
    ##     P4      (.22.)       0.722    0.023   31.563    0.000    0.585    0.466
    ##     P5      (.23.)       0.848    0.022   39.345    0.000    0.688    0.596
    ##     P6      (.24.)       1.084    0.024   45.841    0.000    0.879    0.720
    ##     P7      (.25.)      -0.123    0.025   -4.849    0.000   -0.099   -0.070
    ##     P8      (.26.)       0.843    0.027   31.739    0.000    0.684    0.468
    ##     P9      (.27.)       1.253    0.026   47.363    0.000    1.016    0.741
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   risk_taking ~                                                         
    ##     machiavellinsm    0.729    0.472    1.545    0.122    0.345    0.072
    ##     narcissism       -0.028    0.165   -0.170    0.865   -0.018   -0.004
    ##     psychopathy       5.420    0.350   15.477    0.000    4.397    0.914
    ## 
    ## Covariances:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism ~~                                                      
    ##     narcissism           0.198    0.011   18.829    0.000    0.673    0.673
    ##     psychopathy          0.350    0.015   22.667    0.000    0.913    0.913
    ##   narcissism ~~                                                            
    ##     psychopathy          0.379    0.018   21.173    0.000    0.752    0.752
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1                4.180    0.019  216.780    0.000    4.180    4.334
    ##    .M2                3.419    0.026  133.938    0.000    3.419    2.678
    ##    .M3                3.335    0.024  137.126    0.000    3.335    2.741
    ##    .M4                3.494    0.024  145.455    0.000    3.494    2.908
    ##    .M5                3.523    0.026  134.991    0.000    3.523    2.699
    ##    .M6                3.646    0.025  144.848    0.000    3.646    2.896
    ##    .M7                4.381    0.018  249.928    0.000    4.381    4.997
    ##    .M8                3.134    0.025  125.691    0.000    3.134    2.513
    ##    .M9                4.062    0.020  203.185    0.000    4.062    4.062
    ##    .N1                3.256    0.023  142.343    0.000    3.256    2.846
    ##    .N2                3.129    0.026  120.524    0.000    3.129    2.410
    ##    .N3                2.635    0.023  113.124    0.000    2.635    2.262
    ##    .N4                2.650    0.024  108.689    0.000    2.650    2.173
    ##    .N5                3.484    0.023  148.615    0.000    3.484    2.971
    ##    .N6                3.060    0.026  118.521    0.000    3.060    2.369
    ##    .N7                2.864    0.026  111.646    0.000    2.864    2.232
    ##    .N8                2.574    0.025  101.150    0.000    2.574    2.022
    ##    .N9                3.416    0.024  144.984    0.000    3.416    2.899
    ##    .P1                3.080    0.025  123.289    0.000    3.080    2.465
    ##    .P2                3.219    0.024  132.561    0.000    3.219    2.650
    ##    .P3                2.582    0.026   99.407    0.000    2.582    1.987
    ##    .P4                2.289    0.025   91.230    0.000    2.289    1.824
    ##    .P5                3.669    0.023  158.872    0.000    3.669    3.176
    ##    .P6                2.904    0.024  118.993    0.000    2.904    2.379
    ##    .P7                3.567    0.029  124.688    0.000    3.567    2.493
    ##    .P8                2.589    0.029   88.584    0.000    2.589    1.771
    ##    .P9                2.753    0.027  100.363    0.000    2.753    2.006
    ##    .risk_taking      28.100    0.096  292.156    0.000   28.100    5.841
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1                0.707    0.021   34.468    0.000    0.707    0.760
    ##    .M2                0.725    0.023   31.745    0.000    0.725    0.445
    ##    .M3                0.830    0.025   33.119    0.000    0.830    0.561
    ##    .M4                1.168    0.034   34.697    0.000    1.168    0.809
    ##    .M5                0.618    0.020   30.190    0.000    0.618    0.362
    ##    .M6                0.688    0.022   31.565    0.000    0.688    0.434
    ##    .M7                0.568    0.017   34.364    0.000    0.568    0.739
    ##    .M8                0.856    0.026   33.019    0.000    0.856    0.550
    ##    .M9                0.606    0.018   33.504    0.000    0.606    0.606
    ##    .N1                0.922    0.028   32.803    0.000    0.922    0.704
    ##    .N2                1.419    0.041   34.225    0.000    1.419    0.842
    ##    .N3                0.762    0.025   30.547    0.000    0.762    0.561
    ##    .N4                0.957    0.030   31.966    0.000    0.957    0.643
    ##    .N5                0.861    0.027   31.709    0.000    0.861    0.626
    ##    .N6                1.420    0.041   34.306    0.000    1.420    0.851
    ##    .N7                1.088    0.034   32.221    0.000    1.088    0.661
    ##    .N8                1.237    0.037   33.476    0.000    1.237    0.764
    ##    .N9                0.832    0.027   31.252    0.000    0.832    0.599
    ##    .P1                0.904    0.026   34.132    0.000    0.904    0.579
    ##    .P2                1.410    0.040   35.310    0.000    1.410    0.956
    ##    .P3                1.059    0.031   34.409    0.000    1.059    0.627
    ##    .P4                1.233    0.035   34.975    0.000    1.233    0.783
    ##    .P5                0.861    0.025   34.496    0.000    0.861    0.645
    ##    .P6                0.718    0.022   33.311    0.000    0.718    0.482
    ##    .P7                2.038    0.058   35.363    0.000    2.038    0.995
    ##    .P8                1.669    0.048   34.971    0.000    1.669    0.781
    ##    .P9                0.850    0.026   32.946    0.000    0.850    0.452
    ##    .risk_taking       1.054    0.154    6.837    0.000    1.054    0.046
    ##     machiavellinsm    0.224    0.013   16.598    0.000    1.000    1.000
    ##     narcissism        0.387    0.022   17.244    0.000    1.000    1.000
    ##     psychopathy       0.658    0.030   21.678    0.000    1.000    1.000
    ## 
    ## 
    ## Group 2 [0]:
    ## 
    ## Latent Variables:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism =~                                                      
    ##     M1                   1.000                               0.472    0.495
    ##     M2      (.p2.)       2.012    0.059   34.250    0.000    0.949    0.750
    ##     M3      (.p3.)       1.705    0.053   32.361    0.000    0.804    0.667
    ##     M4      (.p4.)       1.111    0.044   25.179    0.000    0.524    0.445
    ##     M5      (.p5.)       2.205    0.063   35.145    0.000    1.040    0.788
    ##     M6      (.p6.)       2.004    0.059   34.212    0.000    0.945    0.739
    ##     M7      (.p7.)       0.947    0.034   27.885    0.000    0.447    0.516
    ##     M8      (.p8.)       1.769    0.054   32.575    0.000    0.834    0.676
    ##     M9      (.p9.)       1.328    0.042   31.484    0.000    0.626    0.634
    ##   narcissism =~                                                            
    ##     N1                   1.000                               0.619    0.537
    ##     N2      (.11.)      -0.830    0.036  -22.985    0.000   -0.514   -0.392
    ##     N3      (.12.)       1.242    0.038   33.064    0.000    0.769    0.663
    ##     N4      (.13.)       1.171    0.038   30.964    0.000    0.725    0.593
    ##     N5      (.14.)       1.152    0.037   31.535    0.000    0.714    0.614
    ##     N6      (.15.)      -0.800    0.036  -22.273    0.000   -0.496   -0.374
    ##     N7      (.16.)       1.201    0.040   30.397    0.000    0.744    0.574
    ##     N8      (.17.)      -0.995    0.037  -26.572    0.000   -0.616   -0.467
    ##     N9      (.18.)       1.200    0.037   32.079    0.000    0.743    0.627
    ##   psychopathy =~                                                           
    ##     P1                   1.000                               0.834    0.656
    ##     P2      (.20.)      -0.315    0.021  -14.666    0.000   -0.263   -0.216
    ##     P3      (.21.)       0.978    0.024   40.384    0.000    0.816    0.631
    ##     P4      (.22.)       0.722    0.023   31.563    0.000    0.602    0.479
    ##     P5      (.23.)       0.848    0.022   39.345    0.000    0.708    0.610
    ##     P6      (.24.)       1.084    0.024   45.841    0.000    0.904    0.719
    ##     P7      (.25.)      -0.123    0.025   -4.849    0.000   -0.102   -0.070
    ##     P8      (.26.)       0.843    0.027   31.739    0.000    0.704    0.483
    ##     P9      (.27.)       1.253    0.026   47.363    0.000    1.045    0.755
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   risk_taking ~                                                         
    ##     machiavellinsm    1.553    0.410    3.784    0.000    0.732    0.149
    ##     narcissism        0.172    0.146    1.174    0.240    0.106    0.022
    ##     psychopathy       4.824    0.288   16.769    0.000    4.025    0.819
    ## 
    ## Covariances:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism ~~                                                      
    ##     narcissism           0.199    0.011   18.882    0.000    0.680    0.680
    ##     psychopathy          0.358    0.016   22.669    0.000    0.910    0.910
    ##   narcissism ~~                                                            
    ##     psychopathy          0.385    0.018   21.079    0.000    0.744    0.744
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1                4.178    0.019  219.228    0.000    4.178    4.386
    ##    .M2                3.484    0.025  137.699    0.000    3.484    2.755
    ##    .M3                3.356    0.024  139.164    0.000    3.356    2.784
    ##    .M4                3.499    0.024  148.391    0.000    3.499    2.969
    ##    .M5                3.582    0.026  135.705    0.000    3.582    2.715
    ##    .M6                3.659    0.026  143.037    0.000    3.659    2.862
    ##    .M7                4.391    0.017  253.660    0.000    4.391    5.075
    ##    .M8                3.146    0.025  127.475    0.000    3.146    2.551
    ##    .M9                4.069    0.020  205.938    0.000    4.069    4.120
    ##    .N1                3.243    0.023  140.630    0.000    3.243    2.814
    ##    .N2                3.097    0.026  117.982    0.000    3.097    2.361
    ##    .N3                2.654    0.023  114.487    0.000    2.654    2.291
    ##    .N4                2.626    0.024  107.275    0.000    2.626    2.146
    ##    .N5                3.540    0.023  152.241    0.000    3.540    3.046
    ##    .N6                3.069    0.027  115.791    0.000    3.069    2.317
    ##    .N7                2.885    0.026  111.246    0.000    2.885    2.226
    ##    .N8                2.622    0.026   99.278    0.000    2.622    1.986
    ##    .N9                3.428    0.024  144.546    0.000    3.428    2.892
    ##    .P1                3.045    0.025  119.634    0.000    3.045    2.394
    ##    .P2                3.199    0.024  131.217    0.000    3.199    2.625
    ##    .P3                2.594    0.026  100.320    0.000    2.594    2.007
    ##    .P4                2.323    0.025   92.461    0.000    2.323    1.850
    ##    .P5                3.717    0.023  160.216    0.000    3.717    3.206
    ##    .P6                2.956    0.025  117.456    0.000    2.956    2.350
    ##    .P7                3.533    0.029  121.604    0.000    3.533    2.433
    ##    .P8                2.594    0.029   89.100    0.000    2.594    1.783
    ##    .P9                2.783    0.028  100.491    0.000    2.783    2.011
    ##    .risk_taking      28.186    0.098  286.765    0.000   28.186    5.738
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1                0.685    0.020   34.433    0.000    0.685    0.755
    ##    .M2                0.699    0.022   31.653    0.000    0.699    0.437
    ##    .M3                0.807    0.024   33.077    0.000    0.807    0.555
    ##    .M4                1.114    0.032   34.652    0.000    1.114    0.802
    ##    .M5                0.660    0.022   30.617    0.000    0.660    0.379
    ##    .M6                0.742    0.023   31.909    0.000    0.742    0.454
    ##    .M7                0.549    0.016   34.324    0.000    0.549    0.734
    ##    .M8                0.826    0.025   32.955    0.000    0.826    0.543
    ##    .M9                0.583    0.017   33.447    0.000    0.583    0.598
    ##    .N1                0.945    0.029   32.823    0.000    0.945    0.711
    ##    .N2                1.457    0.043   34.221    0.000    1.457    0.846
    ##    .N3                0.752    0.025   30.412    0.000    0.752    0.560
    ##    .N4                0.971    0.030   31.966    0.000    0.971    0.649
    ##    .N5                0.841    0.027   31.561    0.000    0.841    0.623
    ##    .N6                1.509    0.044   34.338    0.000    1.509    0.860
    ##    .N7                1.126    0.035   32.287    0.000    1.126    0.671
    ##    .N8                1.363    0.041   33.624    0.000    1.363    0.782
    ##    .N9                0.853    0.027   31.304    0.000    0.853    0.607
    ##    .P1                0.922    0.027   33.922    0.000    0.922    0.570
    ##    .P2                1.415    0.040   35.266    0.000    1.415    0.953
    ##    .P3                1.005    0.029   34.126    0.000    1.005    0.602
    ##    .P4                1.214    0.035   34.846    0.000    1.214    0.770
    ##    .P5                0.844    0.025   34.272    0.000    0.844    0.628
    ##    .P6                0.765    0.023   33.170    0.000    0.765    0.483
    ##    .P7                2.098    0.059   35.334    0.000    2.098    0.995
    ##    .P8                1.623    0.047   34.834    0.000    1.623    0.766
    ##    .P9                0.824    0.025   32.482    0.000    0.824    0.430
    ##    .risk_taking       1.280    0.136    9.441    0.000    1.280    0.053
    ##     machiavellinsm    0.222    0.013   16.607    0.000    1.000    1.000
    ##     narcissism        0.383    0.022   17.198    0.000    1.000    1.000
    ##     psychopathy       0.696    0.032   21.751    0.000    1.000    1.000

    # CFI: 0.787 poor fit
    # TLI: 0.774 poor fit
    # RMSEA: 0.087 poor fit
    # SRMR: 0.064 poor fit

## Scalar Invariance

### Test whether intercepts of observed variables are equal across groups

    fit_scalar <- sem(model, data = data, group = "gender", group.equal = c("loadings", "intercepts"))

    summary(fit_scalar, fit.measures = TRUE, standardized = TRUE)

    ## lavaan 0.6-19 ended normally after 154 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                       181
    ##   Number of equality constraints                    52
    ## 
    ##   Number of observations per group:                   
    ##     1                                             2502
    ##     0                                             2498
    ## 
    ## Model Test User Model:
    ##                                                        
    ##   Test statistic                              14166.077
    ##   Degrees of freedom                                739
    ##   P-value (Chi-square)                            0.000
    ##   Test statistic for each group:
    ##     1                                         6955.666
    ##     0                                         7210.410
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                             63745.498
    ##   Degrees of freedom                               756
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.787
    ##   Tucker-Lewis Index (TLI)                       0.782
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)            -208468.885
    ##   Loglikelihood unrestricted model (H1)    -201385.847
    ##                                                       
    ##   Akaike (AIC)                              417195.770
    ##   Bayesian (BIC)                            418036.488
    ##   Sample-size adjusted Bayesian (SABIC)     417626.571
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.085
    ##   90 Percent confidence interval - lower         0.084
    ##   90 Percent confidence interval - upper         0.086
    ##   P-value H_0: RMSEA <= 0.050                    0.000
    ##   P-value H_0: RMSEA >= 0.080                    1.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.064
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## 
    ## Group 1 [1]:
    ## 
    ## Latent Variables:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism =~                                                      
    ##     M1                   1.000                               0.473    0.490
    ##     M2      (.p2.)       2.013    0.059   34.244    0.000    0.951    0.745
    ##     M3      (.p3.)       1.705    0.053   32.355    0.000    0.806    0.663
    ##     M4      (.p4.)       1.112    0.044   25.174    0.000    0.525    0.437
    ##     M5      (.p5.)       2.206    0.063   35.139    0.000    1.042    0.798
    ##     M6      (.p6.)       2.004    0.059   34.204    0.000    0.947    0.752
    ##     M7      (.p7.)       0.947    0.034   27.881    0.000    0.448    0.511
    ##     M8      (.p8.)       1.769    0.054   32.569    0.000    0.836    0.670
    ##     M9      (.p9.)       1.328    0.042   31.477    0.000    0.628    0.628
    ##   narcissism =~                                                            
    ##     N1                   1.000                               0.622    0.544
    ##     N2      (.11.)      -0.831    0.036  -22.983    0.000   -0.517   -0.398
    ##     N3      (.12.)       1.242    0.038   33.060    0.000    0.772    0.663
    ##     N4      (.13.)       1.171    0.038   30.955    0.000    0.728    0.597
    ##     N5      (.14.)       1.153    0.037   31.528    0.000    0.717    0.611
    ##     N6      (.15.)      -0.800    0.036  -22.268    0.000   -0.498   -0.385
    ##     N7      (.16.)       1.202    0.040   30.393    0.000    0.747    0.582
    ##     N8      (.17.)      -0.994    0.037  -26.558    0.000   -0.618   -0.486
    ##     N9      (.18.)       1.200    0.037   32.076    0.000    0.746    0.633
    ##   psychopathy =~                                                           
    ##     P1                   1.000                               0.811    0.649
    ##     P2      (.20.)      -0.315    0.022  -14.665    0.000   -0.256   -0.211
    ##     P3      (.21.)       0.978    0.024   40.370    0.000    0.793    0.610
    ##     P4      (.22.)       0.722    0.023   31.558    0.000    0.585    0.466
    ##     P5      (.23.)       0.849    0.022   39.335    0.000    0.688    0.596
    ##     P6      (.24.)       1.084    0.024   45.821    0.000    0.879    0.720
    ##     P7      (.25.)      -0.123    0.025   -4.851    0.000   -0.100   -0.070
    ##     P8      (.26.)       0.844    0.027   31.733    0.000    0.684    0.468
    ##     P9      (.27.)       1.253    0.026   47.344    0.000    1.016    0.741
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   risk_taking ~                                                         
    ##     machiavellinsm    0.727    0.472    1.539    0.124    0.344    0.071
    ##     narcissism       -0.029    0.165   -0.173    0.862   -0.018   -0.004
    ##     psychopathy       5.424    0.351   15.465    0.000    4.398    0.914
    ## 
    ## Covariances:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism ~~                                                      
    ##     narcissism           0.198    0.011   18.827    0.000    0.674    0.674
    ##     psychopathy          0.350    0.015   22.663    0.000    0.913    0.913
    ##   narcissism ~~                                                            
    ##     psychopathy          0.379    0.018   21.171    0.000    0.752    0.752
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1      (.65.)    4.171    0.015  272.403    0.000    4.171    4.325
    ##    .M2      (.66.)    3.436    0.023  149.424    0.000    3.436    2.691
    ##    .M3      (.67.)    3.332    0.021  158.741    0.000    3.332    2.740
    ##    .M4      (.68.)    3.488    0.019  187.512    0.000    3.488    2.903
    ##    .M5      (.69.)    3.535    0.024  145.728    0.000    3.535    2.708
    ##    .M6      (.70.)    3.638    0.023  159.052    0.000    3.638    2.889
    ##    .M7      (.71.)    4.379    0.014  311.767    0.000    4.379    4.994
    ##    .M8      (.72.)    3.127    0.022  144.742    0.000    3.127    2.507
    ##    .M9      (.73.)    4.055    0.017  239.372    0.000    4.055    4.056
    ##    .N1      (.74.)    3.246    0.019  171.241    0.000    3.246    2.837
    ##    .N2      (.75.)    3.116    0.020  154.517    0.000    3.116    2.400
    ##    .N3      (.76.)    2.640    0.020  129.061    0.000    2.640    2.266
    ##    .N4      (.77.)    2.634    0.021  127.046    0.000    2.634    2.160
    ##    .N5      (.78.)    3.508    0.020  175.205    0.000    3.508    2.991
    ##    .N6      (.79.)    3.067    0.020  152.823    0.000    3.067    2.375
    ##    .N7      (.80.)    2.870    0.022  132.333    0.000    2.870    2.237
    ##    .N8      (.81.)    2.600    0.021  125.537    0.000    2.600    2.043
    ##    .N9      (.82.)    3.417    0.020  167.366    0.000    3.417    2.900
    ##    .P1      (.83.)    3.055    0.021  143.259    0.000    3.055    2.444
    ##    .P2      (.84.)    3.212    0.018  182.503    0.000    3.212    2.644
    ##    .P3      (.85.)    2.580    0.022  119.392    0.000    2.580    1.986
    ##    .P4      (.86.)    2.301    0.020  116.989    0.000    2.301    1.833
    ##    .P5      (.87.)    3.686    0.019  192.515    0.000    3.686    3.191
    ##    .P6      (.88.)    2.921    0.022  135.097    0.000    2.921    2.392
    ##    .P7      (.89.)    3.551    0.020  173.764    0.000    3.551    2.482
    ##    .P8      (.90.)    2.585    0.023  113.028    0.000    2.585    1.768
    ##    .P9      (.91.)    2.758    0.024  113.055    0.000    2.758    2.010
    ##    .rsk_tkn (.92.)   28.093    0.096  293.091    0.000   28.093    5.839
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1                0.707    0.021   34.469    0.000    0.707    0.760
    ##    .M2                0.726    0.023   31.744    0.000    0.726    0.445
    ##    .M3                0.830    0.025   33.119    0.000    0.830    0.561
    ##    .M4                1.168    0.034   34.697    0.000    1.168    0.809
    ##    .M5                0.618    0.020   30.189    0.000    0.618    0.362
    ##    .M6                0.688    0.022   31.567    0.000    0.688    0.434
    ##    .M7                0.568    0.017   34.365    0.000    0.568    0.739
    ##    .M8                0.856    0.026   33.020    0.000    0.856    0.551
    ##    .M9                0.606    0.018   33.505    0.000    0.606    0.606
    ##    .N1                0.922    0.028   32.803    0.000    0.922    0.705
    ##    .N2                1.419    0.041   34.225    0.000    1.419    0.842
    ##    .N3                0.762    0.025   30.545    0.000    0.762    0.561
    ##    .N4                0.957    0.030   31.968    0.000    0.957    0.643
    ##    .N5                0.862    0.027   31.710    0.000    0.862    0.626
    ##    .N6                1.420    0.041   34.307    0.000    1.420    0.852
    ##    .N7                1.088    0.034   32.220    0.000    1.088    0.661
    ##    .N8                1.238    0.037   33.479    0.000    1.238    0.764
    ##    .N9                0.832    0.027   31.251    0.000    0.832    0.599
    ##    .P1                0.904    0.026   34.135    0.000    0.904    0.579
    ##    .P2                1.410    0.040   35.310    0.000    1.410    0.956
    ##    .P3                1.059    0.031   34.409    0.000    1.059    0.627
    ##    .P4                1.233    0.035   34.975    0.000    1.233    0.783
    ##    .P5                0.861    0.025   34.497    0.000    0.861    0.645
    ##    .P6                0.718    0.022   33.311    0.000    0.718    0.482
    ##    .P7                2.038    0.058   35.363    0.000    2.038    0.995
    ##    .P8                1.669    0.048   34.971    0.000    1.669    0.781
    ##    .P9                0.850    0.026   32.946    0.000    0.850    0.452
    ##    .risk_taking       1.054    0.154    6.830    0.000    1.054    0.046
    ##     machiavellinsm    0.223    0.013   16.594    0.000    1.000    1.000
    ##     narcissism        0.387    0.022   17.242    0.000    1.000    1.000
    ##     psychopathy       0.658    0.030   21.669    0.000    1.000    1.000
    ## 
    ## 
    ## Group 2 [0]:
    ## 
    ## Latent Variables:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism =~                                                      
    ##     M1                   1.000                               0.471    0.495
    ##     M2      (.p2.)       2.013    0.059   34.244    0.000    0.949    0.750
    ##     M3      (.p3.)       1.705    0.053   32.355    0.000    0.804    0.667
    ##     M4      (.p4.)       1.112    0.044   25.174    0.000    0.524    0.445
    ##     M5      (.p5.)       2.206    0.063   35.139    0.000    1.040    0.788
    ##     M6      (.p6.)       2.004    0.059   34.204    0.000    0.945    0.739
    ##     M7      (.p7.)       0.947    0.034   27.881    0.000    0.447    0.516
    ##     M8      (.p8.)       1.769    0.054   32.569    0.000    0.834    0.676
    ##     M9      (.p9.)       1.328    0.042   31.477    0.000    0.626    0.634
    ##   narcissism =~                                                            
    ##     N1                   1.000                               0.619    0.537
    ##     N2      (.11.)      -0.831    0.036  -22.983    0.000   -0.514   -0.392
    ##     N3      (.12.)       1.242    0.038   33.060    0.000    0.769    0.664
    ##     N4      (.13.)       1.171    0.038   30.955    0.000    0.725    0.593
    ##     N5      (.14.)       1.153    0.037   31.528    0.000    0.714    0.614
    ##     N6      (.15.)      -0.800    0.036  -22.268    0.000   -0.495   -0.374
    ##     N7      (.16.)       1.202    0.040   30.393    0.000    0.744    0.574
    ##     N8      (.17.)      -0.994    0.037  -26.558    0.000   -0.616   -0.466
    ##     N9      (.18.)       1.200    0.037   32.076    0.000    0.743    0.627
    ##   psychopathy =~                                                           
    ##     P1                   1.000                               0.834    0.656
    ##     P2      (.20.)      -0.315    0.022  -14.665    0.000   -0.263   -0.216
    ##     P3      (.21.)       0.978    0.024   40.370    0.000    0.816    0.631
    ##     P4      (.22.)       0.722    0.023   31.558    0.000    0.602    0.479
    ##     P5      (.23.)       0.849    0.022   39.335    0.000    0.708    0.610
    ##     P6      (.24.)       1.084    0.024   45.821    0.000    0.904    0.719
    ##     P7      (.25.)      -0.123    0.025   -4.851    0.000   -0.102   -0.070
    ##     P8      (.26.)       0.844    0.027   31.733    0.000    0.703    0.483
    ##     P9      (.27.)       1.253    0.026   47.344    0.000    1.045    0.755
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   risk_taking ~                                                         
    ##     machiavellinsm    1.536    0.412    3.733    0.000    0.724    0.147
    ##     narcissism        0.170    0.147    1.162    0.245    0.105    0.021
    ##     psychopathy       4.836    0.289   16.747    0.000    4.033    0.821
    ## 
    ## Covariances:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism ~~                                                      
    ##     narcissism           0.199    0.011   18.880    0.000    0.680    0.680
    ##     psychopathy          0.358    0.016   22.666    0.000    0.910    0.910
    ##   narcissism ~~                                                            
    ##     psychopathy          0.384    0.018   21.077    0.000    0.745    0.745
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1      (.65.)    4.171    0.015  272.403    0.000    4.171    4.379
    ##    .M2      (.66.)    3.436    0.023  149.424    0.000    3.436    2.717
    ##    .M3      (.67.)    3.332    0.021  158.741    0.000    3.332    2.765
    ##    .M4      (.68.)    3.488    0.019  187.512    0.000    3.488    2.960
    ##    .M5      (.69.)    3.535    0.024  145.728    0.000    3.535    2.679
    ##    .M6      (.70.)    3.638    0.023  159.052    0.000    3.638    2.845
    ##    .M7      (.71.)    4.379    0.014  311.767    0.000    4.379    5.061
    ##    .M8      (.72.)    3.127    0.022  144.742    0.000    3.127    2.535
    ##    .M9      (.73.)    4.055    0.017  239.372    0.000    4.055    4.106
    ##    .N1      (.74.)    3.246    0.019  171.241    0.000    3.246    2.816
    ##    .N2      (.75.)    3.116    0.020  154.517    0.000    3.116    2.375
    ##    .N3      (.76.)    2.640    0.020  129.061    0.000    2.640    2.279
    ##    .N4      (.77.)    2.634    0.021  127.046    0.000    2.634    2.153
    ##    .N5      (.78.)    3.508    0.020  175.205    0.000    3.508    3.018
    ##    .N6      (.79.)    3.067    0.020  152.823    0.000    3.067    2.316
    ##    .N7      (.80.)    2.870    0.022  132.333    0.000    2.870    2.214
    ##    .N8      (.81.)    2.600    0.021  125.537    0.000    2.600    1.969
    ##    .N9      (.82.)    3.417    0.020  167.366    0.000    3.417    2.883
    ##    .P1      (.83.)    3.055    0.021  143.259    0.000    3.055    2.401
    ##    .P2      (.84.)    3.212    0.018  182.503    0.000    3.212    2.636
    ##    .P3      (.85.)    2.580    0.022  119.392    0.000    2.580    1.996
    ##    .P4      (.86.)    2.301    0.020  116.989    0.000    2.301    1.832
    ##    .P5      (.87.)    3.686    0.019  192.515    0.000    3.686    3.179
    ##    .P6      (.88.)    2.921    0.022  135.097    0.000    2.921    2.322
    ##    .P7      (.89.)    3.551    0.020  173.764    0.000    3.551    2.445
    ##    .P8      (.90.)    2.585    0.023  113.028    0.000    2.585    1.776
    ##    .P9      (.91.)    2.758    0.024  113.055    0.000    2.758    1.993
    ##    .rsk_tkn (.92.)   28.093    0.096  293.091    0.000   28.093    5.719
    ##     mchvlln           0.015    0.014    1.081    0.280    0.033    0.033
    ##     nrcsssm           0.007    0.020    0.371    0.711    0.012    0.012
    ##     psychpt           0.016    0.024    0.654    0.513    0.019    0.019
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1                0.685    0.020   34.434    0.000    0.685    0.755
    ##    .M2                0.699    0.022   31.651    0.000    0.699    0.437
    ##    .M3                0.807    0.024   33.076    0.000    0.807    0.555
    ##    .M4                1.114    0.032   34.653    0.000    1.114    0.802
    ##    .M5                0.660    0.022   30.616    0.000    0.660    0.379
    ##    .M6                0.742    0.023   31.910    0.000    0.742    0.454
    ##    .M7                0.549    0.016   34.324    0.000    0.549    0.734
    ##    .M8                0.826    0.025   32.956    0.000    0.826    0.543
    ##    .M9                0.583    0.017   33.447    0.000    0.583    0.598
    ##    .N1                0.945    0.029   32.824    0.000    0.945    0.711
    ##    .N2                1.457    0.043   34.221    0.000    1.457    0.846
    ##    .N3                0.752    0.025   30.410    0.000    0.752    0.560
    ##    .N4                0.971    0.030   31.968    0.000    0.971    0.649
    ##    .N5                0.842    0.027   31.563    0.000    0.842    0.623
    ##    .N6                1.509    0.044   34.339    0.000    1.509    0.860
    ##    .N7                1.126    0.035   32.286    0.000    1.126    0.671
    ##    .N8                1.364    0.041   33.627    0.000    1.364    0.783
    ##    .N9                0.853    0.027   31.303    0.000    0.853    0.607
    ##    .P1                0.923    0.027   33.928    0.000    0.923    0.570
    ##    .P2                1.416    0.040   35.266    0.000    1.416    0.953
    ##    .P3                1.005    0.029   34.129    0.000    1.005    0.602
    ##    .P4                1.215    0.035   34.848    0.000    1.215    0.770
    ##    .P5                0.844    0.025   34.275    0.000    0.844    0.628
    ##    .P6                0.765    0.023   33.175    0.000    0.765    0.483
    ##    .P7                2.099    0.059   35.334    0.000    2.099    0.995
    ##    .P8                1.623    0.047   34.836    0.000    1.623    0.766
    ##    .P9                0.824    0.025   32.487    0.000    0.824    0.430
    ##    .risk_taking       1.277    0.136    9.402    0.000    1.277    0.053
    ##     machiavellinsm    0.222    0.013   16.604    0.000    1.000    1.000
    ##     narcissism        0.383    0.022   17.195    0.000    1.000    1.000
    ##     psychopathy       0.695    0.032   21.743    0.000    1.000    1.000

    # CFI: 0.787 poor fit
    # TLI: 0.782 poor fit
    # RMSEA: 0.085 poor fit
    # SRMR: 0.064 poor fit

## Strict Invariance

### Test whether residual variances are equal across groups

    fit_strict <- sem(model, data = data, group = "gender", group.equal = c("loadings", "intercepts", "residuals"))

    summary(fit_strict, fit.measures = TRUE, standardized = TRUE)

    ## lavaan 0.6-19 ended normally after 166 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                       181
    ##   Number of equality constraints                    80
    ## 
    ##   Number of observations per group:                   
    ##     1                                             2502
    ##     0                                             2498
    ## 
    ## Model Test User Model:
    ##                                                        
    ##   Test statistic                              14192.781
    ##   Degrees of freedom                                767
    ##   P-value (Chi-square)                            0.000
    ##   Test statistic for each group:
    ##     1                                         6969.854
    ##     0                                         7222.927
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                             63745.498
    ##   Degrees of freedom                               756
    ##   P-value                                        0.000
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.787
    ##   Tucker-Lewis Index (TLI)                       0.790
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)            -208482.237
    ##   Loglikelihood unrestricted model (H1)    -201385.847
    ##                                                       
    ##   Akaike (AIC)                              417166.475
    ##   Bayesian (BIC)                            417824.711
    ##   Sample-size adjusted Bayesian (SABIC)     417503.768
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.084
    ##   90 Percent confidence interval - lower         0.082
    ##   90 Percent confidence interval - upper         0.085
    ##   P-value H_0: RMSEA <= 0.050                    0.000
    ##   P-value H_0: RMSEA >= 0.080                    1.000
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.064
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Standard
    ##   Information                                 Expected
    ##   Information saturated (h1) model          Structured
    ## 
    ## 
    ## Group 1 [1]:
    ## 
    ## Latent Variables:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism =~                                                      
    ##     M1                   1.000                               0.472    0.493
    ##     M2      (.p2.)       2.014    0.059   34.228    0.000    0.951    0.748
    ##     M3      (.p3.)       1.707    0.053   32.348    0.000    0.806    0.665
    ##     M4      (.p4.)       1.112    0.044   25.159    0.000    0.525    0.441
    ##     M5      (.p5.)       2.206    0.063   35.116    0.000    1.042    0.793
    ##     M6      (.p6.)       2.004    0.059   34.180    0.000    0.946    0.746
    ##     M7      (.p7.)       0.947    0.034   27.866    0.000    0.447    0.513
    ##     M8      (.p8.)       1.770    0.054   32.552    0.000    0.836    0.674
    ##     M9      (.p9.)       1.328    0.042   31.464    0.000    0.627    0.631
    ##   narcissism =~                                                            
    ##     N1                   1.000                               0.621    0.540
    ##     N2      (.11.)      -0.831    0.036  -22.972    0.000   -0.516   -0.395
    ##     N3      (.12.)       1.242    0.038   33.048    0.000    0.771    0.663
    ##     N4      (.13.)       1.171    0.038   30.939    0.000    0.727    0.595
    ##     N5      (.14.)       1.153    0.037   31.522    0.000    0.716    0.613
    ##     N6      (.15.)      -0.800    0.036  -22.241    0.000   -0.497   -0.380
    ##     N7      (.16.)       1.202    0.040   30.385    0.000    0.746    0.579
    ##     N8      (.17.)      -0.991    0.037  -26.476    0.000   -0.615   -0.475
    ##     N9      (.18.)       1.201    0.037   32.066    0.000    0.745    0.630
    ##   psychopathy =~                                                           
    ##     P1                   1.000                               0.813    0.648
    ##     P2      (.20.)      -0.316    0.022  -14.690    0.000   -0.257   -0.211
    ##     P3      (.21.)       0.978    0.024   40.369    0.000    0.795    0.616
    ##     P4      (.22.)       0.722    0.023   31.567    0.000    0.587    0.469
    ##     P5      (.23.)       0.848    0.022   39.335    0.000    0.689    0.598
    ##     P6      (.24.)       1.084    0.024   45.831    0.000    0.881    0.715
    ##     P7      (.25.)      -0.124    0.025   -4.885    0.000   -0.100   -0.070
    ##     P8      (.26.)       0.843    0.027   31.727    0.000    0.685    0.471
    ##     P9      (.27.)       1.253    0.026   47.352    0.000    1.018    0.744
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   risk_taking ~                                                         
    ##     machiavellinsm    1.009    0.395    2.556    0.011    0.476    0.099
    ##     narcissism        0.073    0.148    0.495    0.621    0.046    0.009
    ##     psychopathy       5.187    0.283   18.356    0.000    4.215    0.876
    ## 
    ## Covariances:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism ~~                                                      
    ##     narcissism           0.198    0.011   18.857    0.000    0.677    0.677
    ##     psychopathy          0.350    0.015   22.652    0.000    0.911    0.911
    ##   narcissism ~~                                                            
    ##     psychopathy          0.379    0.018   21.141    0.000    0.751    0.751
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1      (.65.)    4.171    0.015  272.556    0.000    4.171    4.352
    ##    .M2      (.66.)    3.436    0.023  149.550    0.000    3.436    2.702
    ##    .M3      (.67.)    3.332    0.021  158.798    0.000    3.332    2.751
    ##    .M4      (.68.)    3.488    0.019  187.606    0.000    3.488    2.931
    ##    .M5      (.69.)    3.536    0.024  145.537    0.000    3.536    2.693
    ##    .M6      (.70.)    3.637    0.023  158.792    0.000    3.637    2.866
    ##    .M7      (.71.)    4.379    0.014  311.936    0.000    4.379    5.026
    ##    .M8      (.72.)    3.127    0.022  144.844    0.000    3.127    2.520
    ##    .M9      (.73.)    4.055    0.017  239.539    0.000    4.055    4.080
    ##    .N1      (.74.)    3.246    0.019  171.223    0.000    3.246    2.826
    ##    .N2      (.75.)    3.116    0.020  154.509    0.000    3.116    2.387
    ##    .N3      (.76.)    2.640    0.020  129.217    0.000    2.640    2.271
    ##    .N4      (.77.)    2.633    0.021  127.080    0.000    2.633    2.156
    ##    .N5      (.78.)    3.508    0.020  175.416    0.000    3.508    3.003
    ##    .N6      (.79.)    3.068    0.020  152.730    0.000    3.068    2.345
    ##    .N7      (.80.)    2.870    0.022  132.263    0.000    2.870    2.225
    ##    .N8      (.81.)    2.602    0.021  125.474    0.000    2.602    2.007
    ##    .N9      (.82.)    3.417    0.020  167.316    0.000    3.417    2.890
    ##    .P1      (.83.)    3.055    0.021  143.392    0.000    3.055    2.435
    ##    .P2      (.84.)    3.211    0.018  182.518    0.000    3.211    2.641
    ##    .P3      (.85.)    2.581    0.022  119.568    0.000    2.581    2.001
    ##    .P4      (.86.)    2.301    0.020  117.081    0.000    2.301    1.837
    ##    .P5      (.87.)    3.686    0.019  192.762    0.000    3.686    3.199
    ##    .P6      (.88.)    2.922    0.022  135.190    0.000    2.922    2.371
    ##    .P7      (.89.)    3.551    0.020  173.724    0.000    3.551    2.463
    ##    .P8      (.90.)    2.585    0.023  113.129    0.000    2.585    1.777
    ##    .P9      (.91.)    2.758    0.024  113.250    0.000    2.758    2.015
    ##    .rsk_tkn (.92.)   28.093    0.096  292.952    0.000   28.093    5.838
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1      (.31.)    0.696    0.014   48.673    0.000    0.696    0.757
    ##    .M2      (.32.)    0.712    0.016   44.641    0.000    0.712    0.441
    ##    .M3      (.33.)    0.818    0.018   46.682    0.000    0.818    0.557
    ##    .M4      (.34.)    1.141    0.023   49.001    0.000    1.141    0.805
    ##    .M5      (.35.)    0.639    0.015   42.774    0.000    0.639    0.371
    ##    .M6      (.36.)    0.715    0.016   44.718    0.000    0.715    0.444
    ##    .M7      (.37.)    0.559    0.012   48.516    0.000    0.559    0.736
    ##    .M8      (.38.)    0.841    0.018   46.528    0.000    0.841    0.546
    ##    .M9      (.39.)    0.595    0.013   47.242    0.000    0.595    0.602
    ##    .N1      (.40.)    0.934    0.020   46.110    0.000    0.934    0.708
    ##    .N2      (.41.)    1.438    0.030   48.263    0.000    1.438    0.844
    ##    .N3      (.42.)    0.757    0.018   42.550    0.000    0.757    0.560
    ##    .N4      (.43.)    0.964    0.022   44.819    0.000    0.964    0.646
    ##    .N5      (.44.)    0.852    0.019   44.307    0.000    0.852    0.624
    ##    .N6      (.45.)    1.465    0.030   48.419    0.000    1.465    0.856
    ##    .N7      (.46.)    1.107    0.024   45.248    0.000    1.107    0.665
    ##    .N8      (.47.)    1.302    0.028   47.262    0.000    1.302    0.775
    ##    .N9      (.48.)    0.842    0.019   43.760    0.000    0.842    0.603
    ##    .P1      (.49.)    0.913    0.019   48.091    0.000    0.913    0.580
    ##    .P2      (.50.)    1.413    0.028   49.902    0.000    1.413    0.955
    ##    .P3      (.51.)    1.032    0.021   48.438    0.000    1.032    0.620
    ##    .P4      (.52.)    1.224    0.025   49.357    0.000    1.224    0.780
    ##    .P5      (.53.)    0.853    0.018   48.603    0.000    0.853    0.642
    ##    .P6      (.54.)    0.742    0.016   46.966    0.000    0.742    0.489
    ##    .P7      (.55.)    2.068    0.041   49.990    0.000    2.068    0.995
    ##    .P8      (.56.)    1.646    0.033   49.347    0.000    1.646    0.778
    ##    .P9      (.57.)    0.837    0.018   46.220    0.000    0.837    0.447
    ##    .rsk_tkn (.58.)    1.179    0.102   11.610    0.000    1.179    0.051
    ##     mchvlln           0.223    0.013   16.598    0.000    1.000    1.000
    ##     nrcsssm           0.385    0.022   17.225    0.000    1.000    1.000
    ##     psychpt           0.660    0.030   21.737    0.000    1.000    1.000
    ## 
    ## 
    ## Group 2 [0]:
    ## 
    ## Latent Variables:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism =~                                                      
    ##     M1                   1.000                               0.471    0.492
    ##     M2      (.p2.)       2.014    0.059   34.228    0.000    0.949    0.747
    ##     M3      (.p3.)       1.707    0.053   32.348    0.000    0.805    0.665
    ##     M4      (.p4.)       1.112    0.044   25.159    0.000    0.524    0.440
    ##     M5      (.p5.)       2.206    0.063   35.116    0.000    1.040    0.793
    ##     M6      (.p6.)       2.004    0.059   34.180    0.000    0.945    0.745
    ##     M7      (.p7.)       0.947    0.034   27.866    0.000    0.447    0.513
    ##     M8      (.p8.)       1.770    0.054   32.552    0.000    0.834    0.673
    ##     M9      (.p9.)       1.328    0.042   31.464    0.000    0.626    0.630
    ##   narcissism =~                                                            
    ##     N1                   1.000                               0.620    0.540
    ##     N2      (.11.)      -0.831    0.036  -22.972    0.000   -0.515   -0.395
    ##     N3      (.12.)       1.242    0.038   33.048    0.000    0.770    0.663
    ##     N4      (.13.)       1.171    0.038   30.939    0.000    0.726    0.595
    ##     N5      (.14.)       1.153    0.037   31.522    0.000    0.715    0.612
    ##     N6      (.15.)      -0.800    0.036  -22.241    0.000   -0.496   -0.379
    ##     N7      (.16.)       1.202    0.040   30.385    0.000    0.745    0.578
    ##     N8      (.17.)      -0.991    0.037  -26.476    0.000   -0.615   -0.474
    ##     N9      (.18.)       1.201    0.037   32.066    0.000    0.744    0.630
    ##   psychopathy =~                                                           
    ##     P1                   1.000                               0.833    0.657
    ##     P2      (.20.)      -0.316    0.022  -14.690    0.000   -0.263   -0.216
    ##     P3      (.21.)       0.978    0.024   40.369    0.000    0.814    0.626
    ##     P4      (.22.)       0.722    0.023   31.567    0.000    0.601    0.478
    ##     P5      (.23.)       0.848    0.022   39.335    0.000    0.706    0.608
    ##     P6      (.24.)       1.084    0.024   45.831    0.000    0.903    0.724
    ##     P7      (.25.)      -0.124    0.025   -4.885    0.000   -0.103   -0.071
    ##     P8      (.26.)       0.843    0.027   31.727    0.000    0.702    0.480
    ##     P9      (.27.)       1.253    0.026   47.352    0.000    1.043    0.752
    ## 
    ## Regressions:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   risk_taking ~                                                         
    ##     machiavellinsm    1.306    0.386    3.386    0.001    0.616    0.125
    ##     narcissism        0.101    0.142    0.707    0.480    0.062    0.013
    ##     psychopathy       5.015    0.266   18.819    0.000    4.176    0.850
    ## 
    ## Covariances:
    ##                       Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   machiavellianism ~~                                                      
    ##     narcissism           0.198    0.011   18.843    0.000    0.677    0.677
    ##     psychopathy          0.358    0.016   22.672    0.000    0.911    0.911
    ##   narcissism ~~                                                            
    ##     psychopathy          0.384    0.018   21.092    0.000    0.745    0.745
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1      (.65.)    4.171    0.015  272.556    0.000    4.171    4.353
    ##    .M2      (.66.)    3.436    0.023  149.550    0.000    3.436    2.705
    ##    .M3      (.67.)    3.332    0.021  158.798    0.000    3.332    2.753
    ##    .M4      (.68.)    3.488    0.019  187.606    0.000    3.488    2.932
    ##    .M5      (.69.)    3.536    0.024  145.537    0.000    3.536    2.695
    ##    .M6      (.70.)    3.637    0.023  158.792    0.000    3.637    2.868
    ##    .M7      (.71.)    4.379    0.014  311.936    0.000    4.379    5.029
    ##    .M8      (.72.)    3.127    0.022  144.844    0.000    3.127    2.521
    ##    .M9      (.73.)    4.055    0.017  239.539    0.000    4.055    4.082
    ##    .N1      (.74.)    3.246    0.019  171.223    0.000    3.246    2.827
    ##    .N2      (.75.)    3.116    0.020  154.509    0.000    3.116    2.387
    ##    .N3      (.76.)    2.640    0.020  129.217    0.000    2.640    2.273
    ##    .N4      (.77.)    2.633    0.021  127.080    0.000    2.633    2.157
    ##    .N5      (.78.)    3.508    0.020  175.416    0.000    3.508    3.005
    ##    .N6      (.79.)    3.068    0.020  152.730    0.000    3.068    2.345
    ##    .N7      (.80.)    2.870    0.022  132.263    0.000    2.870    2.226
    ##    .N8      (.81.)    2.602    0.021  125.474    0.000    2.602    2.008
    ##    .N9      (.82.)    3.417    0.020  167.316    0.000    3.417    2.892
    ##    .P1      (.83.)    3.055    0.021  143.392    0.000    3.055    2.410
    ##    .P2      (.84.)    3.211    0.018  182.518    0.000    3.211    2.638
    ##    .P3      (.85.)    2.581    0.022  119.568    0.000    2.581    1.982
    ##    .P4      (.86.)    2.301    0.020  117.081    0.000    2.301    1.827
    ##    .P5      (.87.)    3.686    0.019  192.762    0.000    3.686    3.171
    ##    .P6      (.88.)    2.922    0.022  135.190    0.000    2.922    2.342
    ##    .P7      (.89.)    3.551    0.020  173.724    0.000    3.551    2.463
    ##    .P8      (.90.)    2.585    0.023  113.129    0.000    2.585    1.767
    ##    .P9      (.91.)    2.758    0.024  113.250    0.000    2.758    1.988
    ##    .rsk_tkn (.92.)   28.093    0.096  292.952    0.000   28.093    5.720
    ##     mchvlln           0.015    0.014    1.089    0.276    0.033    0.033
    ##     nrcsssm           0.007    0.020    0.379    0.705    0.012    0.012
    ##     psychpt           0.016    0.024    0.654    0.513    0.019    0.019
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .M1      (.31.)    0.696    0.014   48.673    0.000    0.696    0.758
    ##    .M2      (.32.)    0.712    0.016   44.641    0.000    0.712    0.441
    ##    .M3      (.33.)    0.818    0.018   46.682    0.000    0.818    0.558
    ##    .M4      (.34.)    1.141    0.023   49.001    0.000    1.141    0.806
    ##    .M5      (.35.)    0.639    0.015   42.774    0.000    0.639    0.372
    ##    .M6      (.36.)    0.715    0.016   44.718    0.000    0.715    0.445
    ##    .M7      (.37.)    0.559    0.012   48.516    0.000    0.559    0.737
    ##    .M8      (.38.)    0.841    0.018   46.528    0.000    0.841    0.547
    ##    .M9      (.39.)    0.595    0.013   47.242    0.000    0.595    0.603
    ##    .N1      (.40.)    0.934    0.020   46.110    0.000    0.934    0.709
    ##    .N2      (.41.)    1.438    0.030   48.263    0.000    1.438    0.844
    ##    .N3      (.42.)    0.757    0.018   42.550    0.000    0.757    0.561
    ##    .N4      (.43.)    0.964    0.022   44.819    0.000    0.964    0.647
    ##    .N5      (.44.)    0.852    0.019   44.307    0.000    0.852    0.625
    ##    .N6      (.45.)    1.465    0.030   48.419    0.000    1.465    0.856
    ##    .N7      (.46.)    1.107    0.024   45.248    0.000    1.107    0.666
    ##    .N8      (.47.)    1.302    0.028   47.262    0.000    1.302    0.775
    ##    .N9      (.48.)    0.842    0.019   43.760    0.000    0.842    0.603
    ##    .P1      (.49.)    0.913    0.019   48.091    0.000    0.913    0.568
    ##    .P2      (.50.)    1.413    0.028   49.902    0.000    1.413    0.953
    ##    .P3      (.51.)    1.032    0.021   48.438    0.000    1.032    0.609
    ##    .P4      (.52.)    1.224    0.025   49.357    0.000    1.224    0.772
    ##    .P5      (.53.)    0.853    0.018   48.603    0.000    0.853    0.631
    ##    .P6      (.54.)    0.742    0.016   46.966    0.000    0.742    0.476
    ##    .P7      (.55.)    2.068    0.041   49.990    0.000    2.068    0.995
    ##    .P8      (.56.)    1.646    0.033   49.347    0.000    1.646    0.769
    ##    .P9      (.57.)    0.837    0.018   46.220    0.000    0.837    0.435
    ##    .rsk_tkn (.58.)    1.179    0.102   11.610    0.000    1.179    0.049
    ##     mchvlln           0.222    0.013   16.592    0.000    1.000    1.000
    ##     nrcsssm           0.384    0.022   17.216    0.000    1.000    1.000
    ##     psychpt           0.693    0.032   21.816    0.000    1.000    1.000

    # CFI: 0.787 acceptable fit
    # TLI: 0.790 acceptable fit
    # RMSEA: 0.084 acceptable fit
    # SRMR: 0.064 good fit

## Model Comparison

    lavTestLRT(fit_configural, fit_metric, fit_scalar, fit_strict)

    ## 
    ## Chi-Squared Difference Test
    ## 
    ##                 Df    AIC    BIC Chisq Chisq diff RMSEA Df diff Pr(>Chisq)
    ## fit_configural 690 417247 418407 14120                                    
    ## fit_metric     714 417222 418226 14142     22.667     0      24     0.5395
    ## fit_scalar     739 417196 418036 14166     23.774     0      25     0.5325
    ## fit_strict     767 417166 417825 14193     26.705     0      28     0.5344

    # fit_configural
    # Used as the reference point.
    # Constructs are conceptually similar across groups.
    # The same factor structure (pattern of factor loadings) holds for both genders.

    # fit_metric
    # Chi-Square Difference: 22.667, DF Difference: 24, p = 0.5395
    # No significant difference from fit_configural; metric invariance holds.
    # Factor loadings are equal across groups.
    # Allows comparison of relationships (e.g., regressions or covariances) between constructs across genders.

    # fit_scalar
    # Chi-Square Difference: 23.774, DF Difference: 25, p = 0.5325
    # No significant difference from fit_metric; scalar invariance holds.
    # Factor loadings and intercepts are equivalent across groups.
    # Any differences in latent means (e.g., gender differences in constructs like "psychopathy" or "narcissism") are meaningful and not due to measurement bias.

    # fit_strict
    # Chi-Square Difference: 26.705, DF Difference: 28, p = 0.5344
    # No significant difference from fit_scalar; strict invariance holds.
    # Residual variances are equal across groups.
    # This supports the strongest form of measurement invariance, indicating no group-specific differences in how consistently the items measure their constructs.
