library(shiny)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(6, tags$img(src="provino_solo.png", width=400)),
    column(6, tags$img(src="inta.png", width=100), align="right")
  ),
  titlePanel(
    h1("Calculation of the economic benefits",
       h5("Mueller et al. (2025): Economic benefit of genetic progress in five wool sheep breeds of Argentina"))
  ),
  sidebarLayout(
    sidebarPanel(
      textInput("raza","Breed name",NULL),
      numericInput("T","Years of genetic improvement (T)",NULL),
      numericInput("H","Years of total planning horizon (H, H≥T)",NULL),
      numericInput("c","Number of breeding ewes in studs (c)",NULL),
      numericInput("b1","Rams produced per ewe and year in T1 (b1)",NULL),
      numericInput("b2","Rams produced per ewe and year in T2 (b2)",NULL),
      numericInput("f","Mating ratio - ewes per ram (f)",NULL),
      numericInput("p","Lambs weaned per ewe mated in T2 and T3 (p)",NULL,step=0.01),
      numericInput("m2","Annual survival of ewes after first shearing (s1)",NULL,step=0.01),
      numericInput("m1","Annual survival of rams after first mating (s2)",NULL,step=0.01),
      numericInput("u1","Maximum number of matings of ewes (u1)",NULL,min=2,max=8),
      numericInput("u2","Maximum number of matings of rams (u2)",NULL,min=2,max=6),
      numericInput("ecd","Annual genetic progress in WW (sd)",NULL),
      numericInput("eca","Annual genetic progress in CFW and FD (sa)",NULL),
      numericInput("ecm","Annual genetic progress in NLW (sm)",NULL),
      numericInput("ece","Annual genetic progress in AW (sf)",NULL),
      numericInput("costoi","Initial costs in T1 (ci)",NULL),
      numericInput("costoa","Annual costs in T1 (ca)",NULL),
      numericInput("ram1","Additonal ram buying cost in T2",NULL),
      numericInput("ram2","Additonal ram buying cost in T3",NULL),
      numericInput("r","Annual discount rate (r)",NULL,step=0.01, min=0,max=1),
      #actionButton("go","Calculate",class="btn btn-primary")
    ),
    # mainPanel(
    #   tableOutput("resultados"),
    #   plotOutput("plot",height="400px"),
    #   downloadButton("downloadData","Download plot data")
    # )
    mainPanel(
      tableOutput("resultados"),
      actionButton("go", "Calculate", class = "btn btn-primary btn-lg"),
      h6("Economic benefit with discount rate 'r', accumulated over 'T' years of genetic improvement and 'H-T' years of residual effects."),
      div(style = "display: flex; flex-direction: column; align-items: center;",
          div(style = "display: flex; justify-content: center; align-items: center; height: 600px;",
              plotOutput("plot", width = "600px", height = "500px")
          ),
          br(),
          conditionalPanel(
            condition = "input.go > 0",
            downloadButton("downloadData", "Download plot data", class = "btn btn-success")
          )
      )
    )
  )
)

server <- function(input, output, session) {
  datos <- reactiveVal(NULL)
  
  calcular <- function(c, b1, b2, f, p, m1, m2, u1, u2,
                       ecd, eca, ecm, ece,
                       costoi, costoa, ram1, ram2, r, T, H) {
    #T <- 10; H <- 20; 
    jmax <- 5
    
    # ---- initial values ----
    carE1 <- rep(0, T); progE2 <- rep(0, T)
    for(k in 1:u2) {
      carE1[k] <- c * b1 * m1^(k-1)
      progE2[k] <- carE1[k] * f * p
    }
    ncarE1 <- sum(carE1)
    noveE2 <- ncarE1 * f
    carE2 <- rep(0, T); progE3 <- rep(0, T)
    for(k in 1:u2) {
      carE2[k] <- noveE2 * b2 * m1^(k-1)
      progE3[k] <- carE2[k] * f * p
    }
    
    # ---- Probabilities by age ----
    tt <- rep(0, H)
    acum <- sum(m2^(0:(u1-1)))
    tt[1] <- 1/acum
    if(u1 > 1) for(i in 2:u1) tt[i] <- tt[i-1]*m2
    wm <- rep(0, H); wm[3] <- tt[1]/p # modified 6/12/25
    for(i in 4:(u1+2)) wm[i] <- wm[i-1] * m2 # modified 6/12/25
    wa <- wm; if(3 <= H) wa[2] <- wa[3]/m2
    wf <- rep(0, H); if(u1+2 <= H) wf[u1+2] <- wm[u1+2]
    
    # ---- Expression matrices ----
    d <- matrix(0,H,jmax); m_mat <- matrix(0,H,jmax)
    a_mat <- matrix(0,H,jmax); e_mat <- matrix(0,H,jmax)
    d[1,1] <- 0.5
    
    for(j in 1:jmax) {
      for(i in 1:H) {
        # Maternal
        temp <- 0
        for(k in 1:i) temp <- temp + d[i+1-k, j] * wm[k]
        m_mat[i,j] <- temp
        # next direct expression
        if(j < jmax) d[i, j+1] <- m_mat[i,j] * p * 0.5 # modified dec-2025 temp * p
      }
    }
    # Annuals
    for(j in 1:jmax) for(i in 1:H) {
      temp <- 0
      for(k in 1:i) temp <- temp + d[i+1-k, j] * wa[k]
      a_mat[i,j] <- temp
    }
    # Finals
    for(j in 1:jmax) for(i in 1:H) {
      temp <- 0
      for(k in 1:i) temp <- temp + d[i+1-k, j] * wf[k]
      e_mat[i,j] <- temp
    }
    
    # ---- summations ----
    sumd <- rowSums(d)
    suma <- rowSums(a_mat)
    summ <- rowSums(m_mat)
    sume <- rowSums(e_mat)
    
    # ---- Economic values ----
    vecd <- c((1:T)*ecd, rep(T*ecd, H-T))
    veca <- c((1:T)*eca, rep(T*eca, H-T))
    vecm <- c((1:T)*ecm, rep(T*ecm, H-T))
    vece <- c((1:T)*ece, rep(T*ece, H-T))
    
    # ---- tempo matrix ----
    tempo <- matrix(0,H,H)
    for(i in 1:H) for(j in i:H) {
      idx <- j+1-i
      if(idx >= 1 && idx <= H) {
        tempo[i,j] <- vecd[i]*sumd[idx] + veca[i]*suma[idx] +
          vecm[i]*summ[idx] + vece[i]*sume[idx]
      }
    }
    
    # ---- bE2 and sumbe2 ----
    n_progE2 <- u2
    bE2 <- matrix(0, H, H)
    for(i in 1:H) for(j in i:H) {
      acc <- 0
      for(k in 1:min(i, n_progE2)) {
        ii <- i - k + 1; jj <- j - k + 1
        if(ii >= 1 && jj >= 1 && ii <= H && jj <= H) {
          acc <- acc + tempo[ii, jj] * progE2[k]
        }
      }
      bE2[i, j] <- acc
    }
    sumbe2 <- sapply(1:H, function(j) sum(bE2[1:j, j]))
    fr <- (1/(1+r))^((1:H)-1) #modified 6/12/25
    IA2 <- sum(fr * sumbe2)
    
    # ---- bE3 and sumbe3 ----
    n_progE3 <- u2
    bE3 <- matrix(0,H,H)
    for(i in 1:(H-2)) {  # modified 6/12/25: H-2
      for(j in (i+2):H) {  # modified 6/12/25: j starting i+2

          acc <- 0
          for(k in 1:min(i, n_progE3)) {
            ii <- i - k + 1; jj <- j - k - 1
            if(ii >= 1 && jj >= 1) {
              acc <- acc + tempo[ii, jj] * progE3[k]
            }
          }
          bE3[i+2, j] <- acc #modified 6/12/25: starting i+2
        }
      }
    sumbe3 <- sapply(1:H, function(j) sum(bE3[1:j, j])/2)
    IA3 <- sum(fr * sumbe3)
    
    # ---- Costs ----
    cosT1 <- rep(0, H); cosT1[1] <- costoa + costoi
    if(T > 1) for(i in 2:T) cosT1[i] <- costoa * fr[i] #modified 6/12/25
    CB1 <- sum(cosT1)
    cosT2 <- rep(0, H); cosT2[1] <- ram1 * c * b1
    if(T > 1) for(i in 2:T) cosT2[i] <- ram1 * c * b1 * fr[i] #modified 6/12/25
    CB2 <- sum(cosT2)
    cosT3 <- rep(0, H)
    for(i in 3:(T+2)) if(i <= H) cosT3[i] <- ram2 * noveE2 * b2 * fr[i] #modified 6/12/25
    CB3 <- sum(cosT3)
    costot <- CB1 + CB2 + CB3
    
    # Extended results
    Benefit_stud   <- IA2 + IA3 - CB1
    Benefit_all    <- IA2 + IA3 - CB1 - CB2 - CB3
    Benefit_T1     <- CB2 - CB1
    ICR_stud       <- (IA2 + IA3) / CB1
    ICR_T2_IA2     <- ifelse(CB2 == 0, NA, IA2 / CB2)
    ICR_T2_IA3     <- ifelse(CB3 == 0, NA, IA3 / CB3)
    ROI            <- (IA2 + IA3 - CB1) / CB1
    ICR_all        <- (IA2 + IA3) / (CB1 + CB2 + CB3)
    CIR            <- CB1 / (IA2 + IA3)
    
    # add symbol column!
    values <- round(c(
      IA2/1000, IA3/1000, CB1/1000, CB2/1000, CB3/1000,
      Benefit_stud/1000, Benefit_all/1000, Benefit_T1/1000,
      ICR_stud, ICR_T2_IA2, ICR_T2_IA3,
      ROI, ICR_all, CIR*100
    ), 1)
    
    values_char <- as.character(values)
    values_char[length(values_char)] <- paste0(values_char[length(values_char)], " %")
    
    tbl <- data.frame(
      Parameter = c(
        "Income in T2 (meat & wool)", "Income in T3 (meat & wool)",
        "Cost in T1 (stud costs)", "Cost in T2 (= income in T1; rams)", "Cost in T3 (= income in T2; rams)",
        "Benefit (meat & wool, stud costs)", 
        "Benefit (meat & wool, all costs)",
        "Benefit in T1 (ram income, stud costs)",
        "Income to cost ratio (only stud costs)",
        "Income to cost ratio in T2",
        "Income to cost ratio in T3",
        "Return to investment (ROI)",
        "Income to cost ratio (all costs)",
        "Cost to income ratio (CIR)"
      ),
      Symbol = c(
        "A2", "A3",
        "B1", "B2", "B3",
        "A2+A3-B1", 
        "A2+A3-B1-B2-B3",
        "B2-B1",
        "(A2+A3)/B1",
        "A2/B2",
        "A3/B3",
        "(A2+A3-B1)/B1",
        "(A2+A3)/(B1+B2+B3)",
        "B1/(A2+A3)"
      ),
      Value = values_char
    )
    
    colnames(tbl)[3] <- input$raza
    
 
    # ---- Time series to plot ----
    
    library(tidyr)
    library(ggplot2)
    library(dplyr)
    
    A2_an <- c(0, fr * sumbe2)
    A3_an <- c(0, fr * sumbe3)
    B1_an <- c(cosT1, 0)
    B2_an <- c(cosT2, 0)
    B3_an <- c(cosT3, 0)
    
    Benefit_in_T2         <- A2_an - B1_an
    Benefit_in_T2T3       <- A2_an + A3_an - B1_an
    Benefit_in_T2T3_all   <- A2_an + A3_an - B1_an - B2_an - B3_an
    
    # simple names without sign
    df_time <- data.frame(
      Year      = 0:H,
      T2        = Benefit_in_T2,
      T2T3      = Benefit_in_T2T3,
      T2T3_all  = Benefit_in_T2T3_all
    )
    
    df_long <- df_time %>%
      pivot_longer(-Year, names_to = "Scenario", values_to = "Benefit") %>%
      mutate(Scenario = factor(
        Scenario,
        levels = c("T2", "T2T3", "T2T3_all"),
        labels = c(
          "Annual discounted benefit \n in T2",
          "Annual discounted benefit \n in T2 + T3 - BT1",
          "Annual discounted benefit \n in T2 + T3 - BT1 - BT2 - BT3"
        )
      ))
    
    plot_obj <- ggplot(df_long, aes(x = Year, y = Benefit, color = Scenario)) +
      geom_smooth(se = FALSE, method = "loess", size = 1.1) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(
        title= paste0("Aggregate annual discounted economic benefit for the ", input$raza, " breed"),
        x = "Years since birth of first progeny of improved rams",
        y = "Discounted economic benefit (USD)",
        color = "Benefit type"
      ) +
      theme_minimal() +
      theme(legend.position = "top")
    
    list(table = tbl, plot = plot_obj, data = df_time)
    
  }
  
  observeEvent(input$go, {
    params <- reactiveValuesToList(input)[c(
      "c","b1","b2","f","p","m1","m2","u1","u2",
      "ecd","eca","ecm","ece","costoi","costoa","ram1","ram2","r","T","H"
    )]
    res <- do.call(calcular, params)
    output$resultados <- renderTable(res$table, digits = 1)
    output$plot       <- renderPlot(res$plot)
    datos(res$data)
  })
  
  output$downloadData <- downloadHandler(
    filename=function() paste0("plot_data_",Sys.Date(),".csv"),
    content=function(file) write.csv(datos(), file, row.names=FALSE)
  )
}

shinyApp(ui, server)
