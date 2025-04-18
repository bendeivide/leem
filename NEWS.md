# leem 0.1.0

- Start GUI development

# leem 0.2.0

- **Functions for Basic Statistics**
  - [X] *Tables grouped and raw datas*: `tabfreq()`
    - [X] *help*   
    - [X] continuous
    - [X] discrete
      - [X] numerical
      - [X] categorical 
        - [X] ordered
        - [X] no ordered
    - [X] Pendency: change variable names
    - [X] Help document
  - [ ] *Plots*
    - [X] Translated labels, title, ...
    - [ ] Boxplot
    - [X] Histogram:
      - [X] *help* 
      - [X] continuous
      - [X] discrete
        - Coerced to barplot
          - [X] numerical
          - [X] categorical
    - [X] Barplot
      - [X] *help*   
      - [X] discrete
        - [X] numerical
        - [X] categorical
          - [X] ordered
          - [X] no ordered
      - [X] continuous
        - [X] coerced to histogram
    - [X] Stick plot
      - [X] *help*  
      - [X] continuous
      - [X] discrete
        - [X] numerical
        - [X] categorical
          - [X] ordered
          - [X] no ordered
    - [X] Polygon
      - [X] *help* 
      - [X] continuous
      - [X] discrete
        - [X] numerical
        - [X] categorical 
          - [X] ordered
          - [X] no ordered
    - [X] Ogive
      - [X] *help* 
      - [X] continuous
      - [X] discrete
        - [X] numerical
        - [X] categorical 
          - [X] ordered
          - [X] no ordered
    - [X] Pie
      - [X] *help* 
      - [X] continuous
      - [X] discrete
        - [X] numerical
        - [X] categorical 
          - [X] ordered
          - [X] no ordered
  - *Measures of position*:
    - Mean: `mean()`
      - [X] *help* 
      - [X] grouped data
      - [X] raw data
      - [X] Insert into plots: insert()
    - Median: `median()`
       - [X] *help* 
       - [X] grouped data
       - [X] raw data
       - [X] Insert into plots: insert()
    -  [X] Mode: `mfreq()`
      - [X] *help* 
      - [X] grouped data
      - [X] raw data
      - [X] Insert into plots: insert()
  - [X] *Output*: `print.leem()` 
  - [X] *Probability Function*: `P()`
    - Distributions:
      - [X] T-student
        - [X] lower.tail = T/F
        - [X] regions
      - [X] Gumbel
        - [X] lower.tail = T/F
        - [X] regions
        - [X] additional_distributions.R
      - [X] Normal
        - [X] lower.tail = T/F
        - [X] regions
      - [X] Poisson
        - [X] lower.tail = T/F
        - [X] regions
      - [X] Beta
        - [X] lower.tail = T/F
        - [X] regions
      - [X] Exponential
        - [X] lower.tail = T/F
        - [X] regions
      - [X] Binomial
        - [X] lower.tail = T/F
        - [X] regions
      - [X] Hypergeometric(`hyper`)
        - [X] lower.tail = T/F
        - [X] regions
      - [ ] Negative Binomial(`nbinom`)
        - [ ] lower.tail = T/F
        - [ ] regions
      - [X] Geometric
        - [X] lower.tail = T/F
        - [X] regions
  - [X] *Quantitative Function*: `Q()`
    - Distributions:
      - [X] T-Student
          - [X] lower.tail = T/F
      - [X] Gumbel
          - [X] lower.tail = T/F
          - [X] additional_distributions.R
      - [X] Normal
          - [X] lower.tail = T/F
      - [X] Poisson
          - [X] lower.tail = T/F
      - [X] Beta
          - [X] lower.tail = T/F
      - [X] Exponential(`exp`)
          - [X] lower.tail = T/F
      - [X] Binomial
          - [X] lower.tail = T/F
      - [X] Hypergeometric(`hyper`)
          - [X] lower.tail = T/F
      - [ ] Negative Binomial(`nbinom`)
          - [ ] lower.tail = T/F
      - [X] Geometric
          - [X] lower.tail = T/F
          
# leem 0.2.1
  - fixed bugs for P() function:
    - Normal distribution
    - Student's t-distribution
    - Chi-squared distribution
    - F distribution
# leem 0.3.0
  - [X] Bugs fixed in plots, no longer needing to use the tabfreq() function directly;
  - [X] *Test of Hypothesis*: `th()`
    - Distributions:
      - [X] Test of Normal: `ztest`
        - [X] one population
        - [X] two population
        - [X] test of proportion
        - [X] `print.leem()`
        - [X] alternatives
          - [X] two.sided
          - [X] less
          - [X] greater
      - [X] Test of T-Student: `ttest`
        - [X] one population
        - [X] two population
        - [ ] test of proportion
        - [X] `print.leem()`
        - [X] alternatives
          - [X] two.sided
          - [X] less
          - [X] greater

# leem 1.0.0

- [ ] Gradient colors of graphics take into account the weight of frequencies
- [ ] Create 3d plots 
  - [ ] Insert shadows on the bars, giving them a 3d appearance
- [ ] Insert the `details` argument in the `P` function to print the expected and variance values of said distributions

