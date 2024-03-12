---
title: | 
  Partisanship, Mobilization, and Persuasion in Group Voting: A Study of the Gender Gap
author: |
  Barry C. Burden and Michael G. DeCrescenzo
---

# Abstract

Groups in the electorate vote differently from one another.
Racial minorities and young voters prefer Democratic candidates on average, while evangelicals and rural voters prefer Republican candidates.
How do these group differences affect party vote shares, given that the underlying groups are differently sized? We provide a theoretical framework to understand group differences in voting and party vote by expressing each as the result of partisan predispositions in the electorate that are transformed through partisan mobilization, partisan defection, and the choices of unaffiliated voters.
We apply this method to the gender gap in U.S. presidential voting.
We find that the size of the gender gap has no necessary bearing on the partisan vote outcome.
Rather, the relationship is contingent on the changing numerical impact of partisanship, partisan mobilization, and persuasion over time.
Although the gender gap and the Democratic vote in presidential elections have both increased over the years, this relationship is spurious.
The primary cause of the gender gap (partisan change) was actually harmful to the Democrats.
Meanwhile, forces that increased the Democratic vote (mobilization and persuasion) were minor influences on the gender gap.


# Setup instructions

### Micromamba for package management

Installation instructions [here](https://mamba.readthedocs.io/en/latest/installation/micromamba-installation.html),

```sh
"${SHELL}" <(curl -L micro.mamba.pm/install.sh)
```

### Conda environment

```sh
micromamba env create -f conda/gaps.yml --no-rc -y -r conda
micromamba activate conda/envs/gaps
```

```{r}
# these need to be versioned...
renv::install("box")
renv::install("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
```

```sh
# download ANES cumulative file
curl -LO https://electionstudies.org/anes_timeseries_cdf_stata_20220916
# move it to data folder
mv anes_timeseries_cdf_stata_20220916 data/anes_2020.zip
# unzip
unzip data/anes_2020.zip -d data/anes_2020
```


```sh
Rscript R/data_normalization/aggregate_anes_to_cycle.R
Rscript R/data_normalization/clean_anes_respondent_level.R
Rscript R/data_normalization/clean_state_exit_polls.R
Rscript R/regression.R
Rscript R/mcmc.R
Rscript R/post_mc.R

quarto render paper/gaps.qmd --to pdf
```


# Other notes

- The history for this repository was destroyed and recreated because large data files were checked in and difficult to remove.
History for this repository before August 2, 2019, can be found at <https://bitbucket.org/mikedecrescenzo/gender-gap/src>.

