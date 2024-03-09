# Statistical-Hypothesis-Testing


### Statement of the Problem


- Emergency responders like firefighters are crucial for community safety, yet the demanding nature of their work can lead to chronic stress, impaired decision-making, and poor sleep quality (Ángel et al., 2022). Improving these aspects could enhance the emergency services, and firefighter’s well-being, potentially saving more lives. 

- Many studies have shown mindfulness practices reduce anxiety and depression, and improve concentration and attention (American Psychological Association, 2019), these are crucial for effective decision-making in high-stress situations which are typical for firefighters.

- This research aims to provide empirical evidence on mindfulness meditation’s efficacy in promoting better decision-making under stress and improved sleep quality among firefighters.

![Preview](https://github.com/ilonayoyoo/Statistical-Hypothesis-Testing/blob/main/asset/images/firefighter.png)


### Research Question 
#### Research Question 1: Relative to firefighters who do not practice mindfulness meditation, would the mindfulness meditation program lead to better decision-making accuracy under stress among firefighters?
(Refer to Operational Procedures for detailed information on how variables are measured)

**Null Hypothesis H0A**: 
There is no significant increase in the mean accuracy of decision-making under stress between the two groups.

**Alternative Hypothesis H1A**: 
There is a significant increase in the mean accuracy of decision-making under stress between the two groups:

- _H0A_: _μT_ − _μC_ ≤ 0
- _H1A_ : _μT_ − _μC_ > 0 _Where_ :
- _μT_ : The mean accuracy of decision-making under stress for firefighters who practice regular mindfulness meditation.
- _μC_ : The mean accuracy of decision-making under stress for firefighters who do not practice regular mindfulness meditation.


#### Research Question 2: Relative to firefighters who do not practice mindfulness meditation, would the mindfulness meditation program lead to a better decision-making reaction time under stress among firefighters?
(Refer to Operational Procedures for detailed information on how variables are measured)

**Null Hypothesis H0B**: 
The mean reaction time under stress of emergency responders who practise regular mindfulness meditation is not significantly faster than those who do not.

**Alternative Hypothesis H1B**: 
The mean reaction time under stress of emergency responders who practice regular mindfulness meditation is significantly faster than those who do not

- _H0B_: _rT_ − _rC_ ≤ 0
- _H1B_ : _rT_ − _rC_ > 0 _Where_ :
- _гT_: The mean reaction time under stress for firefighters who practice regular mindfulness meditation.
- _гC_: The mean reaction time under stress for firefighters who do not practice regular mindfulness meditation.

#### Research question 3: Relative to firefighters who do not practice mindfulness meditation, would one mindfulness meditation program lead to better sleep quality among firefighters?
(Refer to Operational Procedures for detailed information on how variables are measured)

**Null Hypothesis H0C**: 
The mean sleep quality score of emergency responders who practice regular mindfulness meditation is not significantly higher than those who do not.

**Alternative Hypothesis H1C**: 
The mean sleep quality score of emergency responders who practice regular mindfulness meditation is significantly higher than those who do not.

- _H0C_ : _𝘲T_ − _𝘲C_ ≤0
- _H1C_ : _𝘲T_ − _𝘲C_ >0 Where:
- _𝘲T_ : The mean sleep quality of firefighters who practice regular mindfulness meditation.
- _𝘲C_ : The mean sleep quality of firefighters who do not practice regular mindfulness meditation. 


### Research Plan
#### Population of Interest

The population of interest for this study focuses on firefighters across various states in the United States, with the research plan focusing on a randomized sampling method that minimizes bias. 

Given the challenge of splitting individuals into treatment and control groups within the same firefighting units, especially when they work closely together in the same station that the decision made by one firefighter may have a significant impact on their peers, our study proposes randomization at the firefighting station level.

This approach not only considered potential biases due to working relationships among firefighters but also the stations selected for this study were carefully chosen to reflect the diverse geographic locations, sizes, and demographic compositions found across the entire country, ensuring a comprehensive and representative sample that mirrors the national landscape of firefighting stations.


#### Sample Selection


According to the federal data of wildfire statistics and fire report from the National Fire Protection Association (Policygenius, n.d.), we decided to choose fire departments from the top 10 states with the highest number of wildfires per year and highest incidents of residential fires. We ensure to include locations in both urban and rural areas: California, Texas, Colorado, Arizona, Idaho, Washington, Oklahoma, Oregon, Montana, and Utah. Across all of the states, there is a cumulative number of 27,166 fire stations. 



### Data Colleciton
For our data collection process, we will focus on measuring two key outcomes: sleep quality and decision-making accuracy and speed. To assess sleep quality, we will utilize data extracted from smartwatches worn by the firefighters during their sleep. This method allows for a non-intrusive recording of various sleep parameters and ensures accuracy. The smartwatches will track metrics such as duration of sleep, percentage of time of deep sleep, heart rate, and restfulness, providing a comprehensive overview of the firefighters’ sleep quality. This data will be collected for both the control and treatment groups, enabling a thorough comparison between them.

To gauge decision accuracy, we will invite senior police officers and fire managers to evaluate the participants’ response accuracy. They will use a rating scale ranging from 0 to 10, with higher scores indicating greater accuracy. For measuring reaction time, the time taken by participants to respond to specific scenarios will be recorded using a stopwatch, with the measurements being captured in seconds. This dual-metric approach allows us to comprehensively assess both the quality and speed of decision-making among the firefighter participants.
_Variables_

     Decision-making accuracy
     Decision-making reaction time
     Sleep Quality
     Stress Level

_Treatments_  (Independent Variables):

    Two months of mindfulness meditation training

_Other Variables_:

    Age
    Working experience( length of working)
    Gender
    Physical health conditions (fair / poor)
    Mental health history (problems of sleep loss occurred before? (Yes/ No)
    Marital status

### Statistical Analysis Plan

#### Research Question 1: Decision-Making Accuracy
- Statistical Method: Two-sample, One-sided t-test.
- Dependent Variable (DV): Decision-making accuracy under stress score scaling from 0 to 10

This test is ideal for comparing the means of treatment and control groups firefighters practicing mindfulness meditation vs. those who do not. Given that decision-making accuracy is a continuous variable, the t-test will efficiently test the hypothesis of whether there is a significant difference in mean accuracy between the two groups.

#### Research Question 2: Decision-Making Reaction Time
- Statistical Method: Two-sample, One-sided t-test.
- Dependent Variable (DV): Reaction time under stress, record by time watch.

Rationale: Since reaction time is a continuous variable, a two-sample t-test is appropriate to compare the mean reaction times of treatment and control groups. This method will assess if the mindfulness meditation program leads to a statistically significant improvement in reaction time.

#### Research Question 3: Sleep Quality
- Statistical Method: Two-sample, One-sided t-test.
- Dependent Variable (DV): Sleep quality scores, continuous score based on sleep metrics.

Rationale: As with the previous variables, sleep quality scores are continuous and will be compared between treatment and control groups. The two-sample t-test is suitable for testing if there is a significant difference in mean sleep quality scores between those who practice mindfulness meditation and those who do not.
Sample Size


The sample size is determined by using the sample size formula: These are approximate because they are based on the normal distribution rather than the t distribution. Since σ and δ are not known exactly, the additional error added by using the normal distribution is not important unless the sample sizes are very small.


n=2274

Description:
- n
is the sample size needed per group.
- Zα/2 is the Z-score corresponding to the desired confidence level (95% confidence level, so Zα/2 is approximately 1.96).
- Zβ is the Z-score corresponding to the desired power (90% power, so Zβ is approximately 1.28).
- δ is the expected difference in means (effect size) (δ is chosen as approximately 5%, i.e. 0.05).
- σ is the standard deviation of the outcome measure (σ

is chosen to be 0.52 based on a similar research study done).

Simulation please see R code

