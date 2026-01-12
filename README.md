# Economic benefit calculator (Shiny)

This repository contains the Shiny application developed for the manuscript:

Mueller, J.P. et al. 2026. **"Economic Benefit of Genetic Progress in Five Wool Sheep Breeds of Argentina."**
*Journal of Animal Breeding and Genetics* 1–11. 
https://doi.org/10.1111/jbg.70040.

---

## Online version of the application

A fully deployed online version of the Shiny application is available at:

**https://alanmaxs.shinyapps.io/Beneficio_economico_provino**

This deployment allows users to explore the economic benefit calculations interactively without installing R or the code environment.

---

## Run locally (reproducible)

To reproduce the application locally, clone the repository and restore the R environment using **renv**:

```bash
git clone https://github.com/alanmaxsp/economic-benefit-shiny.git
cd economic-benefit-shiny
R -q -e "install.packages('renv'); renv::restore(); shiny::runApp()"
```
