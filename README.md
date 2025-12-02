# Economic benefit calculator (Shiny)

This repository contains the Shiny application developed for the manuscript:

**"Economic benefit of genetic progress in five wool sheep breeds of Argentina"**  
*Mueller et al., 2025 (submitted).*

Once the manuscript is accepted, a link to the published article will be added here.

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
