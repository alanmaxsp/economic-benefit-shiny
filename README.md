# Economic benefit calculator (Shiny)

Shiny app to compute the economic benefits of genetic progress in sheep programs (Mueller et al., 2025).

**Online demo:** https://alanmaxs.shinyapps.io/Beneficio_economico_provino

## Run locally (reproducible)
```bash
git clone https://github.com/alanmaxsp/economic-benefit-shiny.git
cd economic-benefit-shiny
R -q -e "install.packages('renv'); renv::restore(); shiny::runApp()"
```
