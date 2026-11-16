# Pachete necesare: interfata Shiny, grafice ggplot2, distributia normala bivariata (MASS)
library(shiny)
library(ggplot2)
library(MASS)

# Aplica transformarea g(X) aleasa de utilizator asupra vectorului X
transformare <- function(nr, X) {
  return(switch(as.character(nr),
                "1" = X ^ 2,          # g(x) = x^2
                "2" = abs(X),         # g(x) = |x|
                "3" = log(ifelse(X > 0, X, NA)),   # g(x) = log(x), NA pentru valori <= 0
                "4" = (1 / (1 + exp(-X))),         # g(x) = sigmoida
                "5" = exp(X)          # g(x) = e^x
  ))
}

# Calculeaza indicatorii statistici descriptivi pentru un vector de date
# Elimina valorile lipsa (NA) inainte de calcul
calculeaza_statistici <- function(date, nume_variabila) {
  date_valide <- na.omit(date) 
  
  data.frame(
    Variabila = nume_variabila,
    Media = mean(date_valide),
    Dispersia = var(date_valide),
    Deviatia_Std = sd(date_valide),
    Minim = min(date_valide),
    Q1 = quantile(date_valide, 0.25),
    Mediana = median(date_valide),
    Q3 = quantile(date_valide, 0.75),
    Maxim = max(date_valide)
  )
}

# Intoarce densitatea teoretica f_X(x) pentru distributia aleasa
calc_dens_X <- function(x, params) {
  switch(params$distr,
         "norm"  = dnorm(x, params$mu, params$sigma),
         "exp"   = dexp(x, params$lambda),
         "unif"  = dunif(x, params$min_a, params$max_b),
         "gamma" = dgamma(x, shape = params$alfa, rate = params$theta)
  )
}

# Calculeaza densitatea teoretica f_Y(y) a variabilei transformate Y = g(X)
# folosind formula de schimbare de variabila (regula lantului / jacobian)
calc_dens_Y <- function(y, params) {
  f_X <- function(x) calc_dens_X(x, params)
  
  sapply(y, function(val) {
    switch(as.character(params$trans),
           # Y = X^2 => f_Y(y) = [f_X(sqrt(y)) + f_X(-sqrt(y))] / (2*sqrt(y))
           "1" = if (val > 0) (f_X(sqrt(val)) + f_X(-sqrt(val))) / (2 * sqrt(val)) else 0,
           # Y = |X| => f_Y(y) = f_X(y) + f_X(-y), doar pentru y > 0
           "2" = if (val > 0) f_X(val) + f_X(-val) else 0,
           # Y = log(X) => f_Y(y) = f_X(e^y) * e^y
           "3" = f_X(exp(val)) * exp(val),
           # Y = sigmoida(X) => f_Y(y) = f_X(logit(y)) / (y*(1-y)), pentru y in (0,1)
           "4" = if (val > 0 && val < 1) f_X(log(val / (1 - val))) / (val * (1 - val)) else 0,
           # Y = e^X => f_Y(y) = f_X(log(y)) / y, doar pentru y > 0
           "5" = if (val > 0) f_X(log(val)) / val else 0
    )
  })
}

# ---- INTERFATA UTILIZATOR (UI) ----
ui <- navbarPage(
  title = "Laborator Variabile Aleatoare",
  
  # ---- TAB 1: Transformari 1D ----
  tabPanel("Transformari 1D",
           sidebarLayout(
             sidebarPanel(
               # Selectia distributiei lui X
               selectInput("distributie", "1. Alege repartitia lui X:",
                           choices = c("Normala" = "norm", 
                                       "Exponentiala" = "exp", 
                                       "Uniforma" = "unif", 
                                       "Gamma" = "gamma")),
               
               # Parametrii aferenti fiecarei distributii (apar dinamic)
               conditionalPanel(
                 condition = "input.distributie == 'norm'",
                 numericInput("mu", "Media (mu):", value = 0),
                 numericInput("sigma", "Deviatia Standard (sigma):", value = 1, min = 0.01)
               ),
               conditionalPanel(
                 condition = "input.distributie == 'exp'",
                 numericInput("lambda", "Rata (lambda):", value = 1, min = 0.01)
               ),
               conditionalPanel(
                 condition = "input.distributie == 'unif'",
                 numericInput("min_a", "Minim (a):", value = 0),
                 numericInput("max_b", "Maxim (b):", value = 1)
               ),
               conditionalPanel(
                 condition = "input.distributie == 'gamma'",
                 numericInput("alfa", "Forma (alfa):", value = 2, min = 0.01),
                 numericInput("theta", "Rata (theta):", value = 1, min = 0.01)
               ),
               
               hr(),
               # Selectia transformarii g(X)
               selectInput("tip_transformare", "2. Alege transformarea g(X):",
                           choices = list("g(x) = x^2" = 1,
                                          "g(x) = |x|" = 2,
                                          "g(x) = log(x)" = 3,
                                          "g(x) = 1/(1+e^-x) (Sigmoida)" = 4,
                                          "g(x) = e^x" = 5)),
               
               # Dimensiunea esantionului si butonul de simulare
               numericInput("n", "3. Volumul esantionului (n):", value = 1000, min = 10, max = 100000),
               actionButton("btn_simuleaza", "Simuleaza 1D", class = "btn-primary", style = "width: 100%;")
             ),
             
             mainPanel(
               tabsetPanel(
                 # Sub-tab cu cele 3 grafice: X original, Y empiric, Y teoretic
                 tabPanel("Vizualizare Grafica", 
                          fluidRow(
                            column(4, plotOutput("grafic_X")),
                            column(4, plotOutput("grafic_Y")),
                            column(4, plotOutput("grafic_Y_teoretic"))
                          )
                 ),
                 # Sub-tab cu tabelul de statistici si interpretarea automata
                 tabPanel("Indicatori Statistici", 
                          br(),
                          h4("Statistici pentru esantioanele generate"),
                          tableOutput("tabel_statistici"),
                          hr(),
                          h4("Interpretare Automata"),
                          verbatimTextOutput("mesaj_interpretare")
                 )
               )
             )
           )
  ),
  
  # ---- TAB 2: Transformari 2D ----
  tabPanel("Transformari 2D",
           sidebarLayout(
             sidebarPanel(
               # Alegerea metodei de generare a perechii (X, Y)
               radioButtons("tip_generare_2d", "1. Metoda de generare (X, Y):",
                            choices = c("X si Y independente" = "indep", 
                                        "Normala Bidimensionala" = "bivnorm")),
               
               hr(),
               
               # Parametrii pentru X si Y independente (distributii separate)
               conditionalPanel(
                 condition = "input.tip_generare_2d == 'indep'",
                 h5("Parametrii pentru X:"),
                 selectInput("distr_X_2d", "Repartitia lui X:", 
                             choices = c("Normala" = "norm", "Exponentiala" = "exp", "Uniforma" = "unif", "Gamma" = "gamma")),
                 
                 conditionalPanel(condition = "input.distr_X_2d == 'norm'",
                                  fluidRow(column(6, numericInput("mu_X_indep", "Media mu (X):", 0)), column(6, numericInput("sigma_X_indep", "Dev. Std. sigma (X):", 1, min = 0.01)))
                 ),
                 conditionalPanel(condition = "input.distr_X_2d == 'exp'",
                                  numericInput("lambda_X_indep", "Rata lambda (X):", 1, min = 0.01)
                 ),
                 conditionalPanel(condition = "input.distr_X_2d == 'unif'",
                                  fluidRow(column(6, numericInput("min_X_indep", "Minim (X):", 0)), column(6, numericInput("max_X_indep", "Maxim (X):", 1)))
                 ),
                 conditionalPanel(condition = "input.distr_X_2d == 'gamma'",
                                  fluidRow(column(6, numericInput("alfa_X_indep", "Forma alfa (X):", 2, min = 0.01)), column(6, numericInput("theta_X_indep", "Rata theta (X):", 1, min = 0.01)))
                 ),
                 
                 hr(),
                 
                 h5("Parametrii pentru Y:"),
                 selectInput("distr_Y_2d", "Repartitia lui Y:", 
                             choices = c("Normala" = "norm", "Exponentiala" = "exp", "Uniforma" = "unif", "Gamma" = "gamma")),
                 
                 conditionalPanel(condition = "input.distr_Y_2d == 'norm'",
                                  fluidRow(column(6, numericInput("mu_Y_indep", "Media mu (Y):", 0)), column(6, numericInput("sigma_Y_indep", "Dev. Std. sigma (Y):", 1, min = 0.01)))
                 ),
                 conditionalPanel(condition = "input.distr_Y_2d == 'exp'",
                                  numericInput("lambda_Y_indep", "Rata lambda (Y):", 1, min = 0.01)
                 ),
                 conditionalPanel(condition = "input.distr_Y_2d == 'unif'",
                                  fluidRow(column(6, numericInput("min_Y_indep", "Minim (Y):", 0)), column(6, numericInput("max_Y_indep", "Maxim (Y):", 1)))
                 ),
                 conditionalPanel(condition = "input.distr_Y_2d == 'gamma'",
                                  fluidRow(column(6, numericInput("alfa_Y_indep", "Forma alfa (Y):", 2, min = 0.01)), column(6, numericInput("theta_Y_indep", "Rata theta (Y):", 1, min = 0.01)))
                 )
               ),
               
               # Parametrii pentru distributia normala bidimensionala (mu, sigma, rho)
               conditionalPanel(
                 condition = "input.tip_generare_2d == 'bivnorm'",
                 h5("Parametrii Normal-Bidimensionali:"),
                 fluidRow(
                   column(6, numericInput("mu_X", "Media X (mu X):", value = 0)),
                   column(6, numericInput("mu_Y", "Media Y (mu Y):", value = 0))
                 ),
                 fluidRow(
                   column(6, numericInput("sd_X", "Dev. Std. X (sigma X):", value = 1, min = 0.01)),
                   column(6, numericInput("sd_Y", "Dev. Std. Y (sigma Y):", value = 1, min = 0.01))
                 ),
                 # Coeficientul de corelatie intre X si Y
                 sliderInput("rho", "Coeficient de corelatie (rho):", min = -0.99, max = 0.99, value = 0, step = 0.05)
               ),
               
               hr(),
               
               # Selectia transformarii Z = h(X, Y)
               selectInput("tip_transformare_2d", "2. Alege transformarea Z = h(X, Y):",
                           choices = list("Z = X + Y" = "add",
                                          "Z = X - Y" = "sub",
                                          "Z = X * Y" = "mul",
                                          "Z = sqrt(X^2 + Y^2)" = "dist")),
               
               # Dimensiunea esantionului si butonul de simulare 2D
               numericInput("n_2d", "3. Volumul esantionului (n):", value = 1000, min = 10),
               actionButton("btn_simuleaza_2d", "Simuleaza 2D", class = "btn-success", style = "width: 100%;")
             ),
             
             mainPanel(
               tabsetPanel(
                 # Sub-tab cu scatterplot si histogramele lui X, Y, Z
                 tabPanel("Vizualizare 2D", 
                          br(),
                          plotOutput("scatterplot_XY", height = "400px"),
                          hr(),
                          h4("Histogramele Variabilelor"),
                          fluidRow(
                            column(4, plotOutput("hist_X_2d")),
                            column(4, plotOutput("hist_Y_2d")),
                            column(4, plotOutput("hist_Z_2d"))
                          )
                 ),
                 # Sub-tab cu statistici individuale si relatia (covarianta, corelatie) dintre X si Y
                 tabPanel("Indicatori Statistici 2D", 
                          br(),
                          tableOutput("tabel_stat_2d_individuale"),
                          hr(),
                          h4("Relatia dintre X si Y"),
                          tableOutput("tabel_stat_2d_relatie")
                 )
               )
             )
           )
  )
)

# ---- LOGICA SERVER ----
server <- function(input, output, session) {
  
  # Reactive 1D: se declanseaza la apasarea butonului "Simuleaza 1D"
  # Valideaza parametrii, genereaza X si calculeaza Y = g(X)
  date_simulate <- eventReactive(input$btn_simuleaza, {
    
    # Verifica ca n >= 10
    validate(
      need(input$n >= 10, "Eroare: Volumul esantionului trebuie sa fie cel putin 10!")
    )
    
    # Validare parametrii specifici fiecarei distributii
    if (input$distributie == "norm") {
      validate(need(input$sigma > 0, "Eroare: Deviatia standard (sigma) trebuie sa fie strict pozitiva!"))
    } else if (input$distributie == "exp") {
      validate(need(input$lambda > 0, "Eroare: Rata (lambda) trebuie sa fie strict pozitiva!"))
    } else if (input$distributie == "unif") {
      validate(need(input$min_a < input$max_b, "Eroare: Valoarea minima (a) trebuie sa fie strict mai mica decat valoarea maxima (b)!"))
    } else if (input$distributie == "gamma") {
      validate(
        need(input$alfa > 0, "Eroare: Forma (alfa) trebuie sa fie strict pozitiva!"),
        need(input$theta > 0, "Eroare: Rata (theta) trebuie sa fie strict pozitiva!")
      )
    }
    
    # Genereaza esantionul X din distributia selectata
    X <- switch(input$distributie,
                "norm"  = rnorm(input$n, input$mu, input$sigma),
                "exp"   = rexp(input$n, input$lambda),
                "unif"  = runif(input$n, input$min_a, input$max_b),
                "gamma" = rgamma(input$n, input$alfa, input$theta)
    )
    
    # Aplica transformarea g(X) pentru a obtine Y
    Y <- transformare(input$tip_transformare, X)
    
    # Returneaza esantioanele si parametrii necesari pentru grafice si densitate
    return(list(
      X = X, 
      Y = Y,
      distr = input$distributie,
      trans = input$tip_transformare,
      mu = input$mu,
      sigma = input$sigma,
      lambda = input$lambda,
      min_a = input$min_a,
      max_b = input$max_b,
      alfa = input$alfa,
      theta = input$theta
    ))
  })
  
  # Histograma lui X cu curba densitatii teoretice suprapuse
  output$grafic_X <- renderPlot({
    date <- date_simulate()
    df <- data.frame(valori = date$X)
    
    ggplot(df, aes(x = valori)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
      stat_function(fun = function(x) calc_dens_X(x, date), color = "black", linewidth = 1) +
      labs(title = "Distributia lui X (Original)", x = "Valori X", y = "Densitate") +
      theme_minimal()
  })
  
  # Histograma empirica a lui Y = g(X) (fara curba teoretica)
  output$grafic_Y <- renderPlot({
    date <- date_simulate()
    df <- data.frame(valori = na.omit(date$Y))
    
    ggplot(df, aes(x = valori)) +
      geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "red", color = "white", alpha = 0.7) +
      labs(title = "Distributia lui Y (Histograma Empirica)", x = "Valori Y", y = "Densitate") +
      theme_minimal()
  })
  
  # Curba densitatii teoretice a lui Y, calculata prin schimbare de variabila
  output$grafic_Y_teoretic <- renderPlot({
    date <- date_simulate()
    df <- data.frame(valori = na.omit(date$Y))
    
    ggplot(df, aes(x = valori)) +
      stat_function(fun = function(y) calc_dens_Y(y, date), color = "black", linewidth = 1) +
      labs(title = "Distributia lui Y (Curba Teoretica)", x = "Valori Y", y = "Densitate") +
      theme_minimal()
  })
  
  # Tabel cu statisticile descriptive pentru X si Y
  output$tabel_statistici <- renderTable({
    date <- date_simulate()
    statistici_X <- calculeaza_statistici(date$X, "Variabila X")
    statistici_Y <- calculeaza_statistici(date$Y, "Variabila Y")
    rbind(statistici_X, statistici_Y)
  })
  
  # Mesaj textual cu interpretarea efectului transformarii aplicate
  output$mesaj_interpretare <- renderText({
    date <- date_simulate()
    
    switch(as.character(date$trans),
           "1" = "Transformarea a produs valori strict pozitive si a accentuat valorile extreme.",
           "2" = "Transformarea a produs valori strict pozitive si a modificat simetria distributiei.",
           "3" = "Transformarea a comprimat valorile mari si a accentuat asimetria pentru valori mici.",
           "4" = "Transformarea a comprimat valorile extreme intr-un interval fix (0, 1).",
           "5" = "Transformarea a accentuat extrem de puternic valorile mari si a produs valori strict pozitive."
    )
  })
  
  # Reactive 2D: se declanseaza la apasarea butonului "Simuleaza 2D"
  # Genereaza perechea (X, Y) si calculeaza Z = h(X, Y)
  date_simulate_2d <- eventReactive(input$btn_simuleaza_2d, {
    
    # Verifica ca n >= 10
    validate(
      need(input$n_2d >= 10, "Eroare: Volumul esantionului trebuie sa fie cel putin 10!")
    )
    
    if (input$tip_generare_2d == "indep") {
      # Validare parametrii pentru X si Y independente
      if (input$distr_X_2d == "norm")  validate(need(input$sigma_X_indep > 0,  "Eroare X: Deviatia standard (sigma) trebuie sa fie strict pozitiva!"))
      if (input$distr_X_2d == "exp")   validate(need(input$lambda_X_indep > 0, "Eroare X: Rata (lambda) trebuie sa fie strict pozitiva!"))
      if (input$distr_X_2d == "unif")  validate(need(input$min_X_indep < input$max_X_indep, "Eroare X: Valoarea minima trebuie sa fie strict mai mica decat valoarea maxima!"))
      if (input$distr_X_2d == "gamma") validate(need(input$alfa_X_indep > 0 && input$theta_X_indep > 0, "Eroare X: Parametrii alfa si theta trebuie sa fie strict pozitivi!"))
      
      if (input$distr_Y_2d == "norm")  validate(need(input$sigma_Y_indep > 0,  "Eroare Y: Deviatia standard (sigma) trebuie sa fie strict pozitiva!"))
      if (input$distr_Y_2d == "exp")   validate(need(input$lambda_Y_indep > 0, "Eroare Y: Rata (lambda) trebuie sa fie strict pozitiva!"))
      if (input$distr_Y_2d == "unif")  validate(need(input$min_Y_indep < input$max_Y_indep, "Eroare Y: Valoarea minima trebuie sa fie strict mai mica decat valoarea maxima!"))
      if (input$distr_Y_2d == "gamma") validate(need(input$alfa_Y_indep > 0 && input$theta_Y_indep > 0, "Eroare Y: Parametrii alfa si theta trebuie sa fie strict pozitivi!"))
      
      # Genereaza X independent din distributia selectata
      X <- switch(input$distr_X_2d,
                  "norm"  = rnorm(input$n_2d, input$mu_X_indep, input$sigma_X_indep),
                  "exp"   = rexp(input$n_2d, input$lambda_X_indep),
                  "unif"  = runif(input$n_2d, input$min_X_indep, input$max_X_indep),
                  "gamma" = rgamma(input$n_2d, input$alfa_X_indep, input$theta_X_indep))
      
      # Genereaza Y independent din distributia selectata
      Y <- switch(input$distr_Y_2d,
                  "norm"  = rnorm(input$n_2d, input$mu_Y_indep, input$sigma_Y_indep),
                  "exp"   = rexp(input$n_2d, input$lambda_Y_indep),
                  "unif"  = runif(input$n_2d, input$min_Y_indep, input$max_Y_indep),
                  "gamma" = rgamma(input$n_2d, input$alfa_Y_indep, input$theta_Y_indep))
      
    } else if (input$tip_generare_2d == "bivnorm") {
      # Validare parametrii pentru normala bidimensionala
      validate(
        need(input$sd_X > 0, "Eroare: Deviatia standard pentru X trebuie sa fie strict pozitiva!"),
        need(input$sd_Y > 0, "Eroare: Deviatia standard pentru Y trebuie sa fie strict pozitiva!"),
        need(input$rho > -1 && input$rho < 1, "Eroare: Coeficientul de corelatie (rho) trebuie sa fie strict intre -1 si 1!")
      )
      
      # Construieste vectorul de medii si matricea de covarianta
      medii <- c(input$mu_X, input$mu_Y)
      covarianta <- input$rho * input$sd_X * input$sd_Y
      matrice_cov <- matrix(c(input$sd_X^2, covarianta,
                              covarianta, input$sd_Y^2),
                            nrow = 2, ncol = 2)
      
      # Genereaza esantionul din distributia normala bidimensionala
      date_biv <- mvrnorm(n = input$n_2d, mu = medii, Sigma = matrice_cov)
      X <- date_biv[, 1]
      Y <- date_biv[, 2]
    }
    
    # Calculeaza Z = h(X, Y) in functie de transformarea 2D selectata
    Z <- switch(input$tip_transformare_2d,
                "add"  = X + Y,
                "sub"  = X - Y,
                "mul"  = X * Y,
                "dist" = sqrt(X^2 + Y^2))
    
    return(data.frame(X = X, Y = Y, Z = Z)) 
  })
  
  # Scatterplot X vs Y pentru a vizualiza relatia / corelatie dintre cele doua variabile
  output$scatterplot_XY <- renderPlot({
    date <- date_simulate_2d()
    req(date) 
    
    ggplot(date, aes(x = X, y = Y)) +
      geom_point(alpha = 0.5, color = "purple") +
      labs(title = "Scatterplot X vs Y", x = "Valori X", y = "Valori Y") +
      theme_minimal()
  })
  
  # Histograma marginala a lui X
  output$hist_X_2d <- renderPlot({
    date <- date_simulate_2d()
    req(date)
    
    ggplot(date, aes(x = X)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.7) +
      labs(title = "Histograma lui X", x = "Valori X", y = "Frecventa") +
      theme_minimal()
  })
  
  # Histograma marginala a lui Y
  output$hist_Y_2d <- renderPlot({
    date <- date_simulate_2d()
    req(date)
    
    ggplot(date, aes(x = Y)) +
      geom_histogram(bins = 30, fill = "darkorange", color = "white", alpha = 0.7) +
      labs(title = "Histograma lui Y", x = "Valori Y", y = "Frecventa") +
      theme_minimal()
  })
  
  # Histograma lui Z = h(X, Y), variabila derivata din transformarea 2D
  output$hist_Z_2d <- renderPlot({
    date <- date_simulate_2d()
    req(date)
    
    ggplot(date, aes(x = Z)) +
      geom_histogram(bins = 30, fill = "forestgreen", color = "white", alpha = 0.7) +
      labs(title = "Histograma lui Z = h(X,Y)", x = "Valori Z", y = "Frecventa") +
      theme_minimal()
  })
  
  # Tabel cu statisticile descriptive individuale pentru X, Y si Z
  output$tabel_stat_2d_individuale <- renderTable({
    date <- date_simulate_2d()
    req(date)
    
    statistici_X <- calculeaza_statistici(date$X, "Variabila X")
    statistici_Y <- calculeaza_statistici(date$Y, "Variabila Y")
    statistici_Z <- calculeaza_statistici(date$Z, "Variabila Z")
    rbind(statistici_X, statistici_Y, statistici_Z)
  })
  
  # Tabel cu covarianta si coeficientul de corelatie empiric dintre X si Y
  output$tabel_stat_2d_relatie <- renderTable({
    date <- date_simulate_2d()
    req(date)
    
    data.frame(
      Indicator = c("Covarianta empirica", "Coeficientul de corelatie (rho)"),
      Valoare = c(cov(date$X, date$Y), cor(date$X, date$Y))
    )
  })
  
}

# Porneste aplicatia Shiny
shinyApp(ui = ui, server = server)