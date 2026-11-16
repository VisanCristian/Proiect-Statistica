library(tidyverse)
library(ggplot2)
library(patchwork)
library(gridExtra)

set.seed(2026) # pentru repetiție

# ─────────────────────────────────────────────────────────────
# Generează traficul zilnic de cereri folosind distribuția
# Binomială Negativă, care permite supradispersie față de Poisson.
# ─────────────────────────────────────────────────────────────
genereaza_trafic_zilnic_nb <- function(zile = 365, medie_dorita = 1000, dispersie_size = 20) {
  rnbinom(zile, size = dispersie_size, mu = medie_dorita)
}

# Calculează câte cereri sunt suspecte într-o zi,
# ca fracție p din traficul total al zilei respective.
calculeaza_cereri_suspecte <- function(p, trafic_zilnic) {
  as.integer(round(p * trafic_zilnic))
}

# ─────────────────────────────────────────────────────────────
# Simulează detecțiile pentru o strategie dată pe parcursul unui an.
#
# Strategii suportate:
#   "aleatoare_simpla"  – procent fix de verificare în fiecare zi
#   "adaptiva_clasica"  – dacă traficul depășește media istorică,
#                         procentul se dublează (maxim 100%)
#   "propusa_echipa"    – creștere proporțională cu raportul
#                         trafic_azi / medie_pana_ieri (maxim 100%)
#
# ─────────────────────────────────────────────────────────────
simuleaza_detectii_strategie <- function(trafic_anual, cereri_suspecte, strategy_type, procent_verificare) {
  zile <- length(trafic_anual)
  
  # Media cumulativă a traficului până în ziua precedentă (folosită de strategiile adaptive)
  medie_pana_ieri <- c(trafic_anual[1], cummean(trafic_anual)[1:(zile - 1)])
  
  # Ajustează procentul de verificare în funcție de strategia aleasă
  procent_verificare <- switch(
    strategy_type,
    "aleatoare_simpla" = procent_verificare,
    "adaptiva_clasica" = if_else(
      trafic_anual > medie_pana_ieri,
      pmin(procent_verificare * 2, 1.0),   # max 100%
      procent_verificare
    ),
    "propusa_echipa" = if_else(
      trafic_anual > medie_pana_ieri,
      pmin(procent_verificare + ((trafic_anual / medie_pana_ieri) - 1) * procent_verificare, 1.0),
      procent_verificare
    )
  )
  
  # Numărul efectiv de cereri verificate în fiecare zi
  cereri_verificate <- as.integer(round(procent_verificare * trafic_anual))
  
  # Detecții simulate: distribuție hipergeometrică (eșantionare fără înlocuire)
  detectate <- as.integer(if_else(
    cereri_suspecte > 0,
    rhyper(
      zile,
      m = cereri_suspecte,                  # cereri suspecte în urnă
      n = trafic_anual - cereri_suspecte,   # cereri normale în urnă
      k = cereri_verificate                 # câte tragem
    ),
    0
  ))
  
  # Probabilitatea teoretică zilnică de a detecta cel puțin o cerere suspectă:
  # P(X >= 1) = 1 - P(X = 0), unde X ~ Hypergeometric
  prob_detectie_zilnica <- if_else(
    cereri_suspecte > 0,
    1 - dhyper(0, m = cereri_suspecte, n = trafic_anual - cereri_suspecte, k = cereri_verificate),
    0
  )
  
  # Media anuală a probabilităților zilnice
  prob_teoretica_anuala <- mean(prob_detectie_zilnica)
  
  # Calculează indicatorii agregați anuali
  indicatori <- calculeaza_indicatori_anuali(trafic_anual, cereri_suspecte, detectate, cereri_verificate)
  
  list(
    strategy_type          = strategy_type,
    cereri_totale_pe_zi    = trafic_anual,
    cereri_suspecte_pe_zi  = cereri_suspecte,
    cereri_detectate_pe_zi = detectate,
    prob_teoretica_anuala  = prob_teoretica_anuala,
    date_pe_an             = indicatori
  )
}

# ─────────────────────────────────────────────────────────────
# Calculează indicatorii de performanță anuali:
#   - rata_detectie       = detectate / suspecte
#   - rata_verificare     = verificate / totale
#   - indicator_eficienta = rata_detectie / rata_verificare
#     (câtă detecție obținem per unitate de efort de verificare)
#   - cost_total          = c1 * verificate + c2 * nedetectate
#     (c1 = cost per verificare, c2 = penalizare per suspectă nedetectată)
# ─────────────────────────────────────────────────────────────
calculeaza_indicatori_anuali <- function(trafic_anual, cereri_suspecte, detectate, cereri_verificate, c1 = 1, c2 = 50) {
  
  totale_cereri      <- sum(trafic_anual)
  totale_suspecte    <- sum(cereri_suspecte)
  totale_verificate  <- sum(cereri_verificate)
  totale_detectate   <- sum(detectate)
  totale_nedetectate <- totale_suspecte - totale_detectate
  
  rata_detectie  <- if (totale_suspecte > 0) totale_detectate / totale_suspecte else 0
  rata_verificare <- if (totale_cereri > 0) totale_verificate / totale_cereri else 1
  indicator_eficienta <- if (rata_verificare > 0) rata_detectie / rata_verificare else 0
  
  cost_total <- (c1 * totale_verificate) + (c2 * totale_nedetectate)
  
  list(
    total_trafic        = totale_cereri,
    total_suspecte      = totale_suspecte,
    total_verificate    = totale_verificate,
    total_detectate     = totale_detectate,
    rata_detectie       = rata_detectie,
    rata_verificare     = rata_verificare,
    indicator_eficienta = indicator_eficienta,
    cost_total          = cost_total
  )
}

# ─────────────────────────────────────────────────────────────
# Rulează `nr_simulari` repetări independente pentru toate cele
# trei strategii, cu același trafic și aceleași cereri suspecte
# în fiecare iterație
# ─────────────────────────────────────────────────────────────
ruleaza_simulari_comparate <- function(nr_simulari, procent_verificare, p = 0.005) {
  strategy_types <- c("aleatoare_simpla", "adaptiva_clasica", "propusa_echipa")
  
  # Pre-alocăm liste goale pentru fiecare strategie
  rezultate <- setNames(
    lapply(strategy_types, function(s) vector("list", nr_simulari)),
    strategy_types
  )
  
  for (i in seq_len(nr_simulari)) {
    trafic   <- genereaza_trafic_zilnic_nb()
    suspecte <- calculeaza_cereri_suspecte(p, trafic)
    
    for (s in strategy_types) {
      rezultate[[s]][[i]] <- simuleaza_detectii_strategie(
        trafic_anual       = trafic,
        cereri_suspecte    = suspecte,
        strategy_type      = s,
        procent_verificare = procent_verificare
      )
    }
  }
  
  rezultate
}

# ─── Simularea principală: 1000 de iterații, 10% verificare ────────────────
simulari <- ruleaza_simulari_comparate(nr_simulari = 1000, procent_verificare = 0.10)

# Extragem prima simulare pentru vizualizări zi-de-zi
simularea_1_aleatoare <- simulari$aleatoare_simpla[[1]]
simularea_1_adaptiva  <- simulari$adaptiva_clasica[[1]]

# Data frame cu evoluția zilnică din prima simulare
date_zi_de_zi <- data.frame(
  Zi                = 1:365,
  Cereri_Suspecte   = simularea_1_aleatoare$cereri_suspecte_pe_zi,
  Detectate_Aleator = simularea_1_aleatoare$cereri_detectate_pe_zi,
  Detectate_Adaptiv = simularea_1_adaptiva$cereri_detectate_pe_zi
)

# ── G1: Distribuția numărului de cereri suspecte per zi ───────────────────────
g1 <- ggplot(date_zi_de_zi, aes(x = Cereri_Suspecte)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "white", alpha = 0.8) +
  labs(title = "Histograma cererilor suspecte / zi, p=0.005", x = "Nr. cereri suspecte", y = "Frecventa (Zile)") +
  theme_minimal()
g1

# ── G2: Distribuția detecțiilor zilnice (strategie aleatoare) ─────────────────
g2 <- ggplot(date_zi_de_zi, aes(x = Detectate_Aleator)) +
  geom_histogram(binwidth = 1, fill = "red", color = "white", alpha = 0.8) +
  labs(title = "Histograma cererilor detectate (Aleator)", x = "Nr. cereri detectate", y = "Frecventa (Zile)") +
  theme_minimal()
g2

# ─────────────────────────────────────────────────────────────
# Analiză comparativă pentru trei valori ale lui p:
#   p = 0.001
#   p = 0.005
#   p = 0.020 
# ─────────────────────────────────────────────────────────────
scenarii_p  <- c(0.001, 0.005, 0.02)
procent_baza <- 0.10
date_analiza_completa <- data.frame()

for (p_curent in scenarii_p) {
  sim_p <- ruleaza_simulari_comparate(nr_simulari = 1000, procent_verificare = procent_baza, p = p_curent)
  
  # Media ratei de detecție (detectate/suspecte) pe cele 1000 de simulări
  prop_aleator <- mean(sapply(sim_p$aleatoare_simpla, function(x) x$date_pe_an$rata_detectie))
  prop_adaptiv <- mean(sapply(sim_p$adaptiva_clasica, function(x) x$date_pe_an$rata_detectie))
  prop_propusa <- mean(sapply(sim_p$propusa_echipa,   function(x) x$date_pe_an$rata_detectie))
  
  # Media costului anual pe cele 1000 de simulări
  cost_aleator <- mean(sapply(sim_p$aleatoare_simpla, function(x) x$date_pe_an$cost_total))
  cost_adaptiv <- mean(sapply(sim_p$adaptiva_clasica, function(x) x$date_pe_an$cost_total))
  cost_propusa <- mean(sapply(sim_p$propusa_echipa,   function(x) x$date_pe_an$cost_total))
  
  date_analiza_completa <- rbind(date_analiza_completa, data.frame(
    Probabilitate_P     = as.character(p_curent),
    Strategie           = c("Adaptiva Clasica", "Aleatoare Simpla", "Propusa Echipa"),
    Proportie_Detectata = c(prop_adaptiv, prop_aleator, prop_propusa),
    Cost_Mediu          = c(cost_adaptiv, cost_aleator, cost_propusa)
  ))
}

# ── G3: Rata de detecție medie per strategie și scenariu p ────────────────────
g3 <- ggplot(date_analiza_completa, aes(x = Probabilitate_P, y = Proportie_Detectata, fill = Strategie)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("purple", "blue", "hotpink")) +
  labs(
    title = "Comparatie intre strategii: cereri detectate/suspecte",
    x     = "Probabilitatea p ca o cerere sa fie suspecta",
    y     = "Proportie medie detectata",
    fill  = "Strategie"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "right")

# ── G5: Cost mediu anual per strategie și scenariu p ─────────────────────────
g5 <- ggplot(date_analiza_completa, aes(x = Probabilitate_P, y = Cost_Mediu, fill = Strategie)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("purple", "blue", "hotpink")) +
  labs(
    title = "Comparatie Costuri Medii Anuale intre Strategii",
    x     = "Probabilitatea p ca o cerere sa fie suspecta",
    y     = "Cost Mediu",
    fill  = "Strategie"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5), legend.position = "right")

g3 + g5  # Afișăm cele două grafice alăturat (patchwork)

# ── G4: Evoluție zilnică primele 60 de zile (suspecte vs. detectate) ──────────
g4 <- ggplot(date_zi_de_zi[1:60, ], aes(x = Zi)) +
  geom_line(aes(y = Cereri_Suspecte,   color = "Totale Suspecte"),    size = 1) +
  geom_line(aes(y = Detectate_Aleator, color = "Detectate (Aleator)"), size = 1) +
  scale_color_manual(values = c(
    "Totale Suspecte"    = "orange",
    "Detectate (Aleator)" = "red"
  )) +
  labs(title = "Evolutia zilnica (Zilele 1-60)", x = "Zi", y = "Numar Cereri", color = "Legenda") +
  theme_minimal()
g4

# ─────────────────────────────────────────────────────────────
#variăm atât p cât și procentul de verificare
# ─────────────────────────────────────────────────────────────
procente_testate <- c(0.01, 0.05, 0.10, 0.20, 0.30)
scenarii_p       <- c(0.001, 0.005, 0.02)
rezultate_complete_7 <- data.frame()
tabel_comparativ     <- data.frame()

for (p_curent in scenarii_p) {
  for (proc in procente_testate) {
    
    sim_temporar <- ruleaza_simulari_comparate(nr_simulari = 1000, procent_verificare = proc, p = p_curent)
    
    # Probabilitatea empirică = media fracției de zile cu cel puțin o detecție
    prob_aleator <- mean(sapply(sim_temporar$aleatoare_simpla, function(sim) {
      sum(sim$cereri_detectate_pe_zi > 0) / 365
    }))
    prob_teoretica_aleator <- mean(sapply(sim_temporar$aleatoare_simpla, function(sim) sim$prob_teoretica_anuala))
    
    prob_adaptiv <- mean(sapply(sim_temporar$adaptiva_clasica, function(sim) {
      sum(sim$cereri_detectate_pe_zi > 0) / 365
    }))
    prob_teoretica_adaptiv <- mean(sapply(sim_temporar$adaptiva_clasica, function(sim) sim$prob_teoretica_anuala))
    
    prob_propusa <- mean(sapply(sim_temporar$propusa_echipa, function(sim) {
      sum(sim$cereri_detectate_pe_zi > 0) / 365
    }))
    prob_teoretica_propusa <- mean(sapply(sim_temporar$propusa_echipa, function(sim) sim$prob_teoretica_anuala))
    
    # Adăugăm câte un rând per strategie în data frame-ul pentru graficul final
    rezultate_complete_7 <- rbind(rezultate_complete_7, data.frame(
      Procent_Verificare     = proc * 100,
      Probabilitate_Detectie = prob_aleator,
      Strategie              = "Aleatoare Simpla",
      Scenariu_P             = paste("p =", p_curent)
    ))
    
    rezultate_complete_7 <- rbind(rezultate_complete_7, data.frame(
      Procent_Verificare     = proc * 100,
      Probabilitate_Detectie = prob_adaptiv,
      Strategie              = "Adaptiva Clasica",
      Scenariu_P             = paste("p =", p_curent)
    ))
    
    rezultate_complete_7 <- rbind(rezultate_complete_7, data.frame(
      Procent_Verificare     = proc * 100,
      Probabilitate_Detectie = prob_propusa,
      Strategie              = "Propusa Echipa",
      Scenariu_P             = paste("p =", p_curent)
    ))
    
    # Tabel comparativ empiric vs. teoretic pentru fiecare combinație (p, procent, strategie)
    tabel_comparativ <- rbind(tabel_comparativ, data.frame(
      Scenariu_P        = paste("p =", p_curent),
      Procent_Verificat = paste0(proc * 100, "%"),
      Strategie         = c("Aleatoare Simpla", "Adaptiva Clasica", "Propusa Echipa"),
      Empiric           = c(round(prob_aleator, 4), round(prob_adaptiv, 4), round(prob_propusa, 4)),
      Teoretic          = c(round(prob_teoretica_aleator, 4), round(prob_teoretica_adaptiv, 4), round(prob_teoretica_propusa, 4))
    ))
  }
}

# ── Curbe de detecție în funcție de procentul de verificare ──────────
# Fiecare panou corespunde unui scenariu p; liniile separă strategiile.
g_final <- ggplot(rezultate_complete_7, aes(x = Procent_Verificare, y = Probabilitate_Detectie, color = Strategie, group = Strategie)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_color_manual(values = c("Aleatoare Simpla" = "blue", "Adaptiva Clasica" = "purple", "Propusa Echipa" = "hotpink")) +
  facet_wrap(~ Scenariu_P) +
  labs(
    title  = "Efectul cresterii numarului de verificari asupra probabilitatii de detectie",
    x      = "Procent de verificare din total cereri (%)",
    y      = "Probabilitatea empirica de detectie zilnica",
    color  = "Strategie"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5))

print(g_final)

# ── comparație empiric vs. teoretic pentru toate combinațiile ────
g_tabel <- tableGrob(tabel_comparativ, rows = NULL)
grid.arrange(g_tabel)