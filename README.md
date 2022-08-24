# Chapter 1: Horizontal Movement

This chapter of my thesis focused on the horizontal movement and habitat preferences of young white sharks (Carcharodon carcharias) in the northwestern Atlantic Ocean nursery habitat.

The objective of this study is to determine how young-of-the-year and juvenile white sharks use the nursery habitat in the New York Bight. The null hypotheses to be tested are as follows:
  1. Young white sharks will be homogenously distributed across the NY bight.
  2. Young white sharks will show no preference for underlying bathymetry.
  3. Young white sharks will show no preference for sea surface temperatures.
  4. Young white sharks will show no preference for sea surface salinities.
  5. Young white sharks will show no preference for Chlorophyll a concentration.
 
The analyses for this study were written in R and include the following:
  1. An **Analysis of Variance (ANOVA)** was conducted on the total length of the tagged individuals caught in 2016, 2017, and 2019 to verify tagged individuals had    similar mean total lengths. This was done to rule out size as a factor in horizontal movement behavior (i.e., a significantly larger individual may be physiologically able to travel farther away from the coast than a smaller individual). **Shapiro-Wilks** and **Levene’s tests** were also calculated to determine if the data met the normality and homogeneity assumptions required in order to conduct the ANOVA.
  
  2. A **two-sampled t-test** was conducted to analyze size differences between the individuals that did not return to the New York Bight and the return migrants. This was completed to determine if size was indicative of return migration.
  
  3. A **log-likelihood chi-square test** was conducted to determine the preferred habitat of the tagged individuals. The preferred habitats tested include **bathymetry**, **sea surface temperatures**, **sea surface salinities**, and **Chlorophyll a concentration**. A log-likelihood chi-squared test compares the goodness-of-fit of the hypothesized model against the observed model, and can be used to compute a p-value. Following Rogers and White (2007), three log-likelihood chi-square statistics were calculated. The first chi-square statistic was used to determine if the sharks were using the various habitats in a similar fashion. The null hypothesis states that all sampled individuals are using the habitats in the same proportions as each other. Using the same notation found in Rogers and White (2007), 𝑢𝑖𝑗 is the amount of habitat type i used by sharks j ; 𝑢𝑖+ is the amount of habitat type i used by all sharks; 𝑢+𝑗 is the total amount of habitat units used by sharks j ; and 𝑢++ is the total number of habitat units used by all sharks. The first chi-square statistic is: 
  
      𝜒𝐿12=2ΣΣ𝑢𝑖𝑗𝑙𝑜𝑔𝑒[𝑢𝑖𝑗/𝐸(𝑢𝑖𝑗)]𝑙𝑖=1𝑛𝑗=1

    where E(uij) = ui+u+j/u++. The degrees of freedom were calculated as (I – 1) (n – 1) df, with I being the number of habitat categories and n being the number of  sharks. A p-value <0.05 indicates evidence for heterogeneity, signifying individuals were using the various habitats in different proportions.

      A second chi-square statistic was calculated to examine if selection was occurring for individual habitat types by some of the sharks. The null hypothesis states selection is not occurring in at least some of the sharks. The second chi-square statistic is: 

      𝜒𝐿22=2ΣΣ𝑢𝑖𝑗𝑙𝑜𝑔𝑒[𝑢𝑖𝑗/𝐸(𝑢𝑖𝑗)]𝑙𝑖=1𝑛𝑗=1

    where E(𝑢𝑖𝑗) = 𝜋𝑖𝑢+𝑗 and 𝜋𝑖 is the proportion of available habitat units that are in category i. A p-value <0.05 indicates at least some of the sampled sharks were selective in the types of habitats they used.

      The final chi-square statistic was calculated by taking the difference between the first two: 

      𝜒𝐿32=𝜒𝐿22− 𝜒𝐿12

    This statistic describes whether, on average, sharks were using the various habitat types in proportion to their availability, regardless of which ones were selected. A p-value <0.05 indicates strong selection for certain habitat types.

      In order to determine if there was a preference for specific habitats or environmental variable ranges, selection ratios were calculated. Once again, the notation from Rogers and White (2007) is used:

      𝑤̂𝑖= 𝑢𝑖+/(𝜋𝑖𝑢++)

    A selection ratio greater than one indicates preference for that habitat, with a selection ratio less than one indicating avoidance for that particular habitat.

    Additionally, standard errors were also calculated as that number was required to calculate confidence intervals. The standard error was found by: 

    𝑆𝐸(𝑤̂𝑖)=√𝑛(𝑛−1)(𝑢++)2Σ(𝑢𝑖𝑗𝜋𝑖−𝑤̂𝑖(𝑢+𝑗))2𝑛𝑗=1

    with confidence intervals being calculated as: 

    𝑤̂𝑖± 𝑧𝛼2𝐼𝑆𝐸(𝑤̂𝑖)

All statistical tests were conducted in R Studio (version 1.1.453).
