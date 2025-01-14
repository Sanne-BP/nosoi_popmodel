nosoi <img src="man/figures/logo.png" align="right" alt="" width="120" />
===============
<!-- badges: start -->
[![R-CMD-check](https://github.com/slequime/nosoi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/slequime/nosoi/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/slequime/nosoi/branch/master/graph/badge.svg)](https://codecov.io/gh/slequime/nosoi)
[![](https://img.shields.io/github/license/slequime/nosoi)](http://slequime.github.io/nosoi/)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/nosoi)](https://cran.r-project.org/package=nosoi)
<!-- badges: end -->

The aim of `nosoi` (pronounced no.si) is to provide a flexible agent-based stochastic transmission chain/epidemic simulator. It is named after the *daimones* of plague, sickness and disease that escaped Pandora's jar in the Greek mythology. `nosoi` is able to take into account the influence of multiple variable on the transmission process (e.g. dual-host systems (such as arboviruses), within-host viral dynamics, transportation, population structure), alone or taken together, to create complex but relatively intuitive epidemiological simulations.

## Installation
To get the current released version from CRAN:
```R
install.packages("nosoi")
```

To get the latest (and possibly unstable) version, you can use the [`devtools`](https://github.com/hadley/devtools) package:
```R
install.packages("devtools")
devtools::install_github(repo = "slequime/nosoi")
```

## Documentation

You can find package documentation, with reference, tutorials and examples here: http://slequime.github.io/nosoi/ (built with [`pkgdown`](https://github.com/hadley/pkgdown)).

## Citation

Sebastian Lequime, Paul Bastide, Simon Dellicour, Philippe Lemey & Guy Baele (2020) *nosoi: A stochastic agent-based transmission chain simulation framework in R*. **Methods in Ecology and Evolution** 11:1002-1007 [doi:10.1111/2041-210X.13422](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13422)

### `nosoi` in published manuscripts

* Lequime et al. (2020) *Modeling intra-mosquito dynamics of Zika virus and its dose-dependence confirms the low epidemic potential of * Aedes albopictus. **PLoS Pathogens** 16(12):e1009068. [doi:10.1371/journal.ppat.1009068](https://doi.org/10.1371/journal.ppat.1009068)
* Aubry, Jacobs & Darmuzey et al. (2021) *Recent African strains of Zika virus display higher transmissibility and fetal pathogenicity than Asian strains*. **Nature Communications** 12:916 [doi:10.1038/s41467-021-21199-z](https://doi.org/10.1038/s41467-021-21199-z)
* Giovanetti et al. (2021) *SARS-CoV-2 shifting transmission dynamics and hidden reservoirs potentially limit efficacy of public health interventions in Italy*. **Communications Biology** 4:489 [doi:10.1038/s42003-021-02025-0](https://doi.org/10.1038/s42003-021-02025-0)
* Goldstein et al. (2022) *Using genetic data to identify transmission risk factors: Statistical assessment and application to tuberculosis transmission*. **PLoS Computational Biology** 18(12): e1010696 [doi:10.1371/journal.pcbi.1010696](https://doi.org/10.1371/journal.pcbi.1010696)
* Marini et al. (2022) *Optimizing viral genome subsampling by genetic diversity and temporal distribution (TARDiS) for phylogenetics*. **Bioinformatics** 38(3):856–860 [doi:https://doi.org/10.1093/bioinformatics/btab725](https://doi.org/10.1093/bioinformatics/btab725)
* Vignier et al. (2023) *Chikungunya intra-vector dynamics in* Aedes albopictus *from Lyon (France) upon exposure to a human viremia-like dose range reveals vector barrier’s permissiveness and supports local epidemic potential*. **PCI Journal** 3:e96 [doi:10.24072/pcjournal.326](https://doi.org/10.24072/pcjournal.326)
* Bastide et al. (2024) *Modeling the velocity of evolving lineages and predicting dispersal patterns*. **Proceedings of the National Academy of Sciences**  121(47):e2411582121 [doi:10.1073/pnas.2411582121](https://doi.org/10.1073/pnas.2411582121)
* Magalis et al. (2024) *Novel insights on unraveling dynamics of transmission clusters in outbreaks using phylogeny-based methods* **Infection, Genetics and Evolution** 124:105661 [doi:10.1016/j.meegid.2024.105661](https://doi.org/10.1016/j.meegid.2024.105661)
* Sun et al. (2024) DeepDynaForecast: *Phylogenetic-informed graph deep learning for epidemic transmission dynamic prediction*. **PLoS Computational Biology** 20(4): e1011351 [doi:10.1371/journal.pcbi.1011351](https://doi.org/10.1371/journal.pcbi.1011351)
* Sun et al. (2024) *Phylogenetic-informed graph deep learning to classify dynamic transmission clusters in infectious disease epidemics*. **Bioinformatics Advances** 4(1):vbae158 [doi:10.1093/bioadv/vbae158](https://doi.org/10.1093/bioadv/vbae158)
