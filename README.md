# Canadian_Finance_and_Populism

This project investigates if there is a causal effect of personal financial situation on the degree of support for
populist political opinions, using data from the Canada Election Study (CES) survey for the 2021 Canadian
federal elections. The treatment and control groups are determined based on responses to a question about
how one’s financial situation has changed over the past year. The measure for support for populism is
formed by aggregating responses from several questions. I use several estimators for the average causal
effect, including the regression outcome, Horvitz-Thompson, Hajek and doubly robust estimators. These
estimators show a small positive effect. This outcome holds when using different sets of covariates and
different pre-processing methods.

This was my final project for Stanford's STATS209 Causal Inference course in Fall 2023.

The repository includes:

* R code for cleaning and analyzing the 2021 CES survey data
* The final report on the project
