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
                   coefficient   % of gap          se        2.5%        97.5%
      endowments         -0.02     -10.5% 0.020692651 -0.05943318 -0.000469790
      coefficients        0.24     116.8% 0.022550846  0.21480438  0.273919983
      interaction        -0.01      -6.3% 0.009932174 -0.03398801 -0.004804524

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
                  2.5%         97.5%
      1   0.1676892982  0.5076361160
      2  -0.2080100134  0.0918145056
      3  -0.0462741625  0.0006824008
      4  -0.0045993505  0.0303170117
      5  -0.0180129791  0.0056651748
      6  -0.0203106051  0.0300759086
      7  -0.0240752159  0.0287183945
      8   0.0000000000  0.0000000000
      9  -0.0061312589  0.0356309584
      10 -0.0533409527 -0.0172267929
      11 -0.0234066642 -0.0077666026
      12 -0.0085444075  0.0029986197
      13 -0.0168213031  0.0062207001
      14  0.0009117397  0.0177683331
      15  0.0000000000  0.0000000000
      16 -0.0134980866  0.0004285314
      17 -0.0165542883  0.0003562039
      18 -0.0132856154  0.0021842629
      19 -0.0013819543  0.0033242113
      20 -0.0017353171  0.0012638658
      21 -0.0091418736  0.0029662950

# twofold results with bootstraps haven't changed

    Code
      summary(twofold)
    Output
      Oaxaca Blinder Decomposition model
      ----------------------------------
      Type: twofold
      Formula: ln_real_wage ~ age + education | female
      Data: chicago_long
      
      Descriptives
                  n    %n mean(ln_real_wage)
      female==0 412 57.9%                2.7
      female==1 300 42.1%                2.5
      
      Gap: 0.21
      % Diff: 7.62%
                    coefficient   % of gap           se          2.5%         97.5%
      explained           -0.03     -14.1% 2.038143e-02 -7.192541e-02 -1.736619e-02
      unexplained          0.23     114.1% 2.090135e-02  2.049169e-01  2.636727e-01
      unexplained_a        0.00      -0.0% 8.466634e-16 -2.025108e-15  7.248163e-16
      unexplained_b        0.23     114.1% 2.090135e-02  2.049169e-01  2.636727e-01

---

    Code
      coef(twofold, ci = TRUE)
    Output
             coef_type                     term   coefficient          se
      1      explained              (Intercept)  0.0000000000 0.000000000
      2      explained                      age  0.0123479550 0.011879755
      3      explained       education.baseline -0.0296772350 0.012697180
      4      explained educationadvanced.degree -0.0112121907 0.006723279
      5      explained         educationcollege -0.0005733317 0.003766588
      6      explained     educationhigh.school -0.0048623829 0.007300416
      7      explained    educationsome.college  0.0050367060 0.004683195
      8    unexplained              (Intercept)  0.3126576745 0.111913536
      9    unexplained                      age -0.0676170840 0.098146865
      10   unexplained       education.baseline -0.0245262886 0.016668166
      11   unexplained educationadvanced.degree  0.0047374082 0.008856773
      12   unexplained         educationcollege -0.0058998532 0.008598669
      13   unexplained     educationhigh.school  0.0063995418 0.017086506
      14   unexplained    educationsome.college  0.0091238244 0.017349927
      15 unexplained_a              (Intercept)  0.0351819655 0.054553031
      16 unexplained_a                      age -0.0273205680 0.044598713
      17 unexplained_a       education.baseline -0.0114575135 0.008588602
      18 unexplained_a educationadvanced.degree  0.0018858169 0.003792390
      19 unexplained_a         educationcollege -0.0029483951 0.003963667
      20 unexplained_a     educationhigh.school  0.0011050094 0.009143358
      21 unexplained_a    educationsome.college  0.0035536849 0.008490329
      22 unexplained_b              (Intercept)  0.2774757091 0.059287771
      23 unexplained_b                      age -0.0402965160 0.054728585
      24 unexplained_b       education.baseline -0.0130687751 0.008449203
      25 unexplained_b educationadvanced.degree  0.0028515914 0.005073600
      26 unexplained_b         educationcollege -0.0029514581 0.004699436
      27 unexplained_b     educationhigh.school  0.0052945324 0.008327313
      28 unexplained_b    educationsome.college  0.0055701396 0.009015144
                 2.5%         97.5%
      1   0.000000000  0.0000000000
      2  -0.005925916  0.0288129406
      3  -0.060089024 -0.0233457057
      4  -0.025260487 -0.0082279632
      5  -0.008347641  0.0028801042
      6  -0.016702373  0.0062478321
      7   0.001212994  0.0136971876
      8   0.167689298  0.5076361160
      9  -0.211456050  0.0919213711
      10 -0.052010755  0.0010933697
      11 -0.003099679  0.0232127059
      12 -0.016381129  0.0053023100
      13 -0.020792915  0.0305048078
      14 -0.023137619  0.0248635963
      15 -0.038970978  0.1293566717
      16 -0.094836936  0.0458877988
      17 -0.025799251  0.0012911013
      18 -0.001640929  0.0097426959
      19 -0.007480007  0.0020825045
      20 -0.014453395  0.0126289455
      21 -0.013535780  0.0096153479
      22  0.206660276  0.3899313093
      23 -0.116619114  0.0471869032
      24 -0.026211504 -0.0001977316
      25 -0.001583360  0.0134700101
      26 -0.008986461  0.0032964776
      27 -0.008264279  0.0180079029
      28 -0.009601839  0.0157995873

# bootstrapped gaps haven't changed

    Code
      obd$bootstraps$gaps
    Output
                       se       2.5%      97.5%
      gap     0.023386760 0.15953913 0.23560859
      pct_gap 0.008383587 0.05962221 0.08679097
      EY_a    0.028655738 2.66036591 2.74872068
      EY_b    0.028167210 2.47231616 2.55630572

