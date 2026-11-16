# Proiect Probabilități și Statistică

## Descriere

În acest repo se află proiectul pentru cursul de `Probabilități și Statistică`, făcut în anul II la `Calculatoare și Tehnologia Informației (CTI)`, FMI, Universitatea din București. Proiectul este scris în `R` și conține două probleme.

##### Conținutul repo-ului
- `Problema1.R` -> simularea traficului unui server și a detecției cererilor suspecte.
- `Problema2.R` -> aplicația `Shiny` pentru transformări de variabile aleatoare.
- `Documentatie.pdf` -> documentația proiectului.

---

## Problema 1

Am simulat traficul unui server pe parcursul unui an folosind `repartiția binomială negativă`, împreună cu cererile suspecte care apar în el. Pentru detectarea lor am comparat trei strategii de verificare:
- verificarea unui `procent fix` de cereri, alese aleator
- o strategie `adaptivă clasică`
- o strategie `adaptivă` propusă de noi

Simulările sunt repetate de mai multe ori, iar la final probabilitățile empirice sunt comparate cu cele teoretice.

---

## Problema 2

O aplicație interactivă făcută în `R Shiny` pentru transformări de variabile aleatoare `1D` și `2D`. Parametrii repartițiilor sunt preluați din interfață și validați, iar pentru fiecare transformare aplicația afișează densitățile, statistici descriptive și grafice.

---

## Echipa

- `Vișan Cristian-Andrei` (lider) -> P1: simulările și formatarea datelor; P2: interfața `Shiny`, documentația și analiza transformărilor bidimensionale.
- `Cojocaru Ruxandra Maria` -> P1: alegerea repartiției, a modelului probabilistic și a metodelor de verificare; P2: generarea variabilelor aleatoare, preluarea și validarea parametrilor repartițiilor.
- `Dutescu Anda Elena` -> P1: documentația și compararea datelor empirice cu cele teoretice; P2: aplicarea transformărilor 1D și 2D și formatarea datelor.
- `Brînzei Sabina Maria` -> P1: prelucrarea datelor finale și interpretarea graficelor și tabelelor; P2: prelucrarea datelor finale, interpretarea datelor empirice și compararea cu cele teoretice pentru transformările 1D.

---

## Pachete necesare

- `tidyverse`, `ggplot2`, `patchwork`, `gridExtra`, `shiny`, `MASS`

Le poți instala din consola `R` cu:
```r
install.packages(c("tidyverse", "ggplot2", "patchwork", "gridExtra", "shiny", "MASS"))
```

---

## Cum se rulează

- Clonează repo-ul și intră în folderul lui.
- Pentru `Problema 1` rulează:
```bash
Rscript Problema1.R
```
- Pentru `Problema 2` deschide `Problema2.R` în `R` sau `RStudio` și rulează tot scriptul. Aplicația `Shiny` pornește prin `shinyApp` și se deschide în browser.
