# Chapter 1: Horizontal Movement

This chapter of my thesis focused on the horizontal movement and habitat preferences of young white sharks (Carcharodon carcharias) in the northwestern Atlantic Ocean nursery habitat.

The objective of this study is to determine how young-of-the-year and juvenile white sharks use the nursery habitat in the New York Bight. The null hypotheses to be tested are as follows:
  1. Young white sharks will be homogenously distributed across the NY bight.
  2. Young white sharks will show no preference for underlying bathymetry.
  3. Young white sharks will show no preference for sea surface temperatures.
  4. Young white sharks will show no preference for sea surface salinities.
  5. Young white sharks will show no preference for Chlorophyll a concentration.
 
The analyses for this study were written in R and include the following:
  1. An **Analysis of Variance (ANOVA)** was conducted on the total length of the tagged individuals caught in 2016, 2017, and 2019 to verify tagged individuals had similar mean total lengths. This was done to rule out size as a factor in horizontal movement behavior (i.e., a significantly larger individual may be physiologically able to travel farther away from the coast than a smaller individual). **Shapiro-Wilks** and **Levene’s tests** were also calculated to determine if the data met the normality and homogeneity assumptions required in order to conduct the ANOVA. *(R code found under file: **ReturnMigrants_ANOVA_ttest**)*
  
  2. A **two-sampled t-test** was conducted to analyze size differences between the individuals that did not return to the New York Bight and the return migrants. This was completed to determine if size was indicative of return migration. *(R code found under file: **ReturnMigrants_ANOVA_ttest**)*
  
  3. A **log-likelihood chi-square test** was conducted to determine the preferred habitat of the tagged individuals. The preferred habitats tested include **bathymetry**, **sea surface temperatures**, **sea surface salinities**, and **Chlorophyll a concentration**. A log-likelihood chi-squared test compares the goodness-of-fit of the hypothesized model against the observed model, and can be used to compute a p-value. Following Rogers and White (2007), **three log-likelihood chi-square statistics were calculated**. The first chi-square statistic was used to determine if the sharks were using the various habitats in a similar fashion. The null hypothesis states that all sampled individuals are using the habitats in the same proportions as each other. Using the same notation found in Rogers and White (2007), 𝑢𝑖𝑗 is the amount of habitat type i used by sharks j ; 𝑢𝑖+ is the amount of habitat type i used by all sharks; 𝑢+𝑗 is the total amount of habitat units used by sharks j ; and 𝑢++ is the total number of habitat units used by all sharks. The **first chi-square statistic** is:
  
       <img width="329" alt="image" src="https://user-images.githubusercontent.com/99918352/186493896-a529fc22-e1bc-42fd-9afe-49ff4af498bc.png">

     where E(uij) = ui+u+j/u++. The degrees of freedom were calculated as (I – 1) (n – 1) df, with I being the number of habitat categories and n being the number of sharks. A p-value <0.05 indicates evidence for heterogeneity, signifying individuals were using the various habitats in different proportions.

     A second chi-square statistic was calculated to examine if selection was occurring for individual habitat types by some of the sharks. The null hypothesis states selection is not occurring in at least some of the sharks. The **second chi-square statistic** is:
     
     <img width="328" alt="image" src="https://user-images.githubusercontent.com/99918352/186494070-9163db7a-8b68-4a18-88ec-8600c8ee2670.png">

     where E(𝑢𝑖𝑗) = 𝜋𝑖𝑢+𝑗 and 𝜋𝑖 is the proportion of available habitat units that are in category i. A p-value <0.05 indicates at least some of the sampled sharks were selective in the types of habitats they used.
     
     The **final chi-square statistic** was calculated by taking the difference between the first two:
     
     <img width="162" alt="image" src="https://user-images.githubusercontent.com/99918352/186494172-c2f72f6c-89ff-4459-8bc3-35c01eedb126.png">
     
     This statistic describes whether, on average, sharks were using the various habitat types in proportion to their availability, regardless of which ones were selected. A p-value <0.05 indicates strong selection for certain habitat types.
     
     In order to determine if there was a preference for specific habitats or environmental variable ranges, **selection ratios** were calculated. Once again, the notation from Rogers and White (2007) is used:
     
     <img width="181" alt="image" src="https://user-images.githubusercontent.com/99918352/186494262-0a49baa9-2b4b-451c-a5be-fce183e67062.png">
     
     A selection ratio greater than one indicates preference for that habitat, with a selection ratio less than one indicating avoidance for that particular habitat.
     
     Additionally, **standard errors** were also calculated as that number was required to calculate confidence intervals. The standard error was found by:
     
     <img width="429" alt="image" src="https://user-images.githubusercontent.com/99918352/186494345-b851e2d9-2539-4341-a572-5b40fc04af4f.png">
     
     with **confidence intervals** being calculated as:
     
     <img width="161" alt="image" src="https://user-images.githubusercontent.com/99918352/186494402-31642f99-842a-44c8-b3b4-586c71cc00d5.png">
     
     *R code for chi-squared analyses and selection ratio graphs found under files: **ChiSquaredAnalysis_Bathymetry, ChiSquaredAnalysis_Chla, ChiSquaredAnalysis_SSS, ChiSquaredAnalysis_SST**.* 
     
     *R code for frequency plots found under file: **ChiSquaredResults_Histograms***

All statistical tests were conducted in R Studio (version 1.1.453).



Rogers, K. B., and G. C. White. 2007. Analysis of movement and habitat use from telemetry data. Pages 625-676 in C. S. Guy, and M. L. Brown, editors. Analysis and interpretation of freshwater fisheries data. American Fisheries Society, Bethesda, Maryland.
     
     
