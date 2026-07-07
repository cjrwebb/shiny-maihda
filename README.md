# Interactive tool for exploring MAIHDA strata predictions
Dr. Calum Webb & Prof. Andy Bell
07-07-26

This tool allows you to explore estimates from a MAIHDA model — where level 2 units have been defined based on combinations of other variables. For more on how to use these models, see this <a href="https://www.sciencedirect.com/science/article/pii/S235282732400065X">tutorial paper</a>. A live version of the app can be viewed here: [https://webb.shinyapps.io/maihda-cocoon/](https://webb.shinyapps.io/maihda-cocoon/)<br>

In order to use the app, you will need two sets of level 2 estimates (as .csv files): one that are full predictions (based on both the fixed part and the level 2 random part of a MAIHDA model), and another that are the multiplicative predictions (based on the random effects estimates from MAIHDA model). The files should have a column for each strata defining variable, and either standard errors or confidence intervals for the estimates\' uncertainty.  To see some example .csv files, which you can also use to test the app, see the <a href="">github repository</a>.<br>

This tool will then produce interactive graphs displaying those predictions, that allow you to identify particular strata, or particular combinations of strata-defining variables. Please note that no data you upload is stored in any way beyond your session.<br>

To cite the app, please use the following citation: Webb, C. and Bell, A. (2026). <em>Interactive tool for exploring MAIHDA strata predictions.</em> Available at <a href="https://webb.shinyapps.io/maihda-cocoon/">https://webb.shinyapps.io/maihda-cocoon/</a> doi: 10.15131/shef.data.32925566
