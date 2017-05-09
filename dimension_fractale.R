## ----echo=FALSE, results='hide'------------------------------------------
require(knitr)

## ------------------------------------------------------------------------
require(ggplot2)

## ------------------------------------------------------------------------
longueur.baton.km <- c(10, 5, 2.5)
nombre.de.batons <- c(18, 52, 132)
mesures.baton <- data.frame(longueur.baton.km, nombre.de.batons)
print(mesures.baton)

## ------------------------------------------------------------------------
print(mesures.baton$longueur.baton.km)
print(mesures.baton$nombre.de.batons)

## ------------------------------------------------------------------------
mesures.baton$longueur.km <- mesures.baton$longueur.baton.km * mesures.baton$nombre.de.batons
print(mesures.baton$longueur.km)

## ------------------------------------------------------------------------
qplot(x = longueur.baton.km, nombre.de.batons, data = mesures.baton)

## ------------------------------------------------------------------------
dim.fractale <- log(52.0 / 18.0) / log(10.0 / 5.0)
print(dim.fractale)

## ------------------------------------------------------------------------
qplot(longueur.baton.km, mesures.baton$nombre.de.batons, data = mesures.baton, log = 'xy')

## ------------------------------------------------------------------------
qplot(mesures.baton$longueur.baton.km, mesures.baton$nombre.de.batons, log = 'xy') + geom_smooth(method = lm, se=FALSE)

## ------------------------------------------------------------------------
mesures.baton$log.longeur.baton.km <- log(mesures.baton$longueur.baton.km)
mesures.baton$log.nombre.de.batons <- log(mesures.baton$nombre.de.batons)

## ------------------------------------------------------------------------
modele.dimension.fractale <- lm(log(nombre.de.batons) ~ log(longueur.baton.km), mesures.baton)
print(modele.dimension.fractale)

## ------------------------------------------------------------------------
summary(modele.dimension.fractale)

## ----echo=FALSE, results='hide'------------------------------------------
purl('dimension_fractale.Rmd')

