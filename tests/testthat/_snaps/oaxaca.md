# threefold results with bootstraps haven't changed

    Code
      summary(threefold)
    Output
      Oaxaca Blinder Decomposition model
      ----------------------------------
      Type: threefold
      Formula: ln_real_wage ~ age + education | female
      Data: chicago_long
      
      Descriptives
                  n    %n mean(ln_real_wage)
      female==0 412 57.9%                2.7
      female==1 300 42.1%                2.5
      
      Gap: 0.21
      % Diff: 7.62%
                   coefficient   % of gap          se        2.5%        97.5% pval
      endowments         -0.02     -10.5% 0.020692651 -0.05943318 -0.000469790  0.2
      coefficients        0.24     116.8% 0.022550846  0.21480438  0.273919983  0.0
      interaction        -0.01      -6.3% 0.009932174 -0.03398801 -0.004804524  0.0

---

    Code
      coef(threefold, ci = TRUE)
    Output
            coef_type                     term   coefficient          se
      1  coefficients              (Intercept)  0.3126576745 0.111913536
      2  coefficients                      age -0.0665392529 0.096589828
      3  coefficients       education.baseline -0.0219068899 0.014770123
      4  coefficients educationadvanced.degree  0.0056567440 0.011817640
      5  coefficients         educationcollege -0.0060065385 0.009154471
      6  coefficients     educationhigh.school  0.0063320725 0.016764225
      7  coefficients    educationsome.college  0.0102343510 0.019198315
      8    endowments              (Intercept)  0.0000000000 0.000000000
      9    endowments                      age  0.0140029972 0.014523549
      10   endowments       education.baseline -0.0258039733 0.012120247
      11   endowments educationadvanced.degree -0.0102776355 0.006161455
      12   endowments         educationcollege -0.0006763985 0.004047663
      13   endowments     educationhigh.school -0.0052066766 0.007409398
      14   endowments    educationsome.college  0.0063629297 0.006337580
      15  interaction              (Intercept)  0.0000000000 0.000000000
      16  interaction                      age -0.0027328733 0.005006486
      17  interaction       education.baseline -0.0064926603 0.005515694
      18  interaction educationadvanced.degree -0.0018538909 0.005522065
      19  interaction         educationcollege  0.0002097521 0.001701718
      20  interaction     educationhigh.school  0.0004117630 0.001020211
      21  interaction    educationsome.college -0.0024367502 0.004272839
                  2.5%         97.5% pval
      1   0.1676892982  0.5076361160  0.0
      2  -0.2080100134  0.0918145056  0.2
      3  -0.0462741625  0.0006824008  0.2
      4  -0.0045993505  0.0303170117  0.4
      5  -0.0180129791  0.0056651748  0.4
      6  -0.0203106051  0.0300759086  0.8
      7  -0.0240752159  0.0287183945  0.6
      8   0.0000000000  0.0000000000  0.0
      9  -0.0061312589  0.0356309584  0.2
      10 -0.0533409527 -0.0172267929  0.0
      11 -0.0234066642 -0.0077666026  0.0
      12 -0.0085444075  0.0029986197  0.8
      13 -0.0168213031  0.0062207001  0.6
      14  0.0009117397  0.0177683331  0.0
      15  0.0000000000  0.0000000000  0.0
      16 -0.0134980866  0.0004285314  0.4
      17 -0.0165542883  0.0003562039  0.2
      18 -0.0132856154  0.0021842629  0.4
      19 -0.0013819543  0.0033242113  0.8
      20 -0.0017353171  0.0012638658  1.0
      21 -0.0091418736  0.0029662950  0.6

# twofold results with bootstraps haven't changed

    Code
      rounded_coefs
    Output
             coef_type                     term coefficient    se   2.5%  97.5% pval
      1      explained              (Intercept)       0.000 0.000  0.000  0.000  0.0
      2      explained                      age       0.012 0.012 -0.006  0.029  0.2
      3      explained       education.baseline      -0.030 0.013 -0.060 -0.023  0.0
      4      explained educationadvanced.degree      -0.011 0.007 -0.025 -0.008  0.0
      5      explained         educationcollege      -0.001 0.004 -0.008  0.003  0.8
      6      explained     educationhigh.school      -0.005 0.007 -0.017  0.006  0.6
      7      explained    educationsome.college       0.005 0.005  0.001  0.014  0.0
      8    unexplained              (Intercept)       0.313 0.112  0.168  0.508  0.0
      9    unexplained                      age      -0.068 0.098 -0.211  0.092  0.2
      10   unexplained       education.baseline      -0.025 0.017 -0.052  0.001  0.2
      11   unexplained educationadvanced.degree       0.005 0.009 -0.003  0.023  0.4
      12   unexplained         educationcollege      -0.006 0.009 -0.016  0.005  0.4
      13   unexplained     educationhigh.school       0.006 0.017 -0.021  0.031  0.8
      14   unexplained    educationsome.college       0.009 0.017 -0.023  0.025  0.6
      15 unexplained_a              (Intercept)       0.035 0.055 -0.039  0.129  0.2
      16 unexplained_a                      age      -0.027 0.045 -0.095  0.046  0.2
      17 unexplained_a       education.baseline      -0.011 0.009 -0.026  0.001  0.2
      18 unexplained_a educationadvanced.degree       0.002 0.004 -0.002  0.010  0.4
      19 unexplained_a         educationcollege      -0.003 0.004 -0.007  0.002  0.4
      20 unexplained_a     educationhigh.school       0.001 0.009 -0.014  0.013  1.0
      21 unexplained_a    educationsome.college       0.004 0.008 -0.014  0.010  0.6
      22 unexplained_b              (Intercept)       0.277 0.059  0.207  0.390  0.0
      23 unexplained_b                      age      -0.040 0.055 -0.117  0.047  0.2
      24 unexplained_b       education.baseline      -0.013 0.008 -0.026  0.000  0.2
      25 unexplained_b educationadvanced.degree       0.003 0.005 -0.002  0.013  0.4
      26 unexplained_b         educationcollege      -0.003 0.005 -0.009  0.003  0.6
      27 unexplained_b     educationhigh.school       0.005 0.008 -0.008  0.018  0.6
      28 unexplained_b    educationsome.college       0.006 0.009 -0.010  0.016  0.6

# bootstrapped gaps haven't changed

    Code
      obd$bootstraps$gaps
    Output
                       se       2.5%      97.5% pval
      gap     0.023386760 0.15953913 0.23560859    0
      pct_gap 0.008383587 0.05962221 0.08679097    0
      EY_a    0.028655738 2.66036591 2.74872068    0
      EY_b    0.028167210 2.47231616 2.55630572    0

