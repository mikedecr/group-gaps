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

## Computational environment handled by `Micromamba`

Installation instructions [here](https://mamba.readthedocs.io/en/latest/installation/micromamba-installation.html),

```sh
"${SHELL}" <(curl -L micro.mamba.pm/install.sh)
```

## Scripts for managing local development

1. `build_scripts/01_setup_environment.sh`: builds and activates conda / micromamba environment.
   Installs R, R packages, Stan, and Quarto for rendering the paper.
2. `build_scripts/02_download_data.sh`: downloads ANES 2020 cumulative data file with `curl`
3. `build_scripts/03_analysis.sh`: executes R scripts that clean and analyze data.
   Artifacts are saved for consumption by the paper.
4. `build_scripts/04_compile_paper.sh`: compiles the paper with Quarto.

The commands in each of these scripts is perfectly appropriate for interactive use in the terminal as well.


# Other notes

- The history for this repository was destroyed and recreated because large data files were checked in and difficult to remove.
History for this repository before August 2, 2019, can be found at <https://bitbucket.org/mikedecrescenzo/gender-gap/src>.

