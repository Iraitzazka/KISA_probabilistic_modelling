# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)

# Parámetros de la distribución beta
a <- 18
b <- 96

# Crear un rango de valores x
x <- seq(0, 0.3, length.out = 1000)

# Calcular la función de densidad
posterior <- dbeta(x, a, b)

# Calcular media, moda y cuartiles
media <- a / (a + b)
mode <- (a - 1) / (a + b - 2)
Q1 <- qbeta(0.025, a, b)
Q2 <- qbeta(0.975, a, b)

# Crear un data frame para ggplot
data <- data.frame(x = x, posterior = posterior)

# Crear la gráfica
ggplot(data, aes(x = x, y = posterior)) +
  geom_line(color = 'red', linetype = 'dashed') +
  geom_vline(xintercept = media, color = 'blue') +
  geom_vline(xintercept = mode, color = 'lightblue') +
  geom_vline(xintercept = Q1, color = 'green') +
  geom_vline(xintercept = Q2, color = 'green') +
  geom_area(data = filter(data, x >= Q1 & x <= Q2), aes(y = posterior), fill = 'lightgreen', alpha = 0.5) +
  scale_x_continuous(breaks = c(media, mode, Q1, Q2), 
                     labels = c(paste0("Media: ", round(media, 2)), 
                                paste0("Mode: ", round(mode, 2)), 
                                paste0("Q1: ", round(Q1, 2)), 
                                paste0("Q2: ", round(Q2, 2)))) +
  labs(title = 'Exercise', x = 'p', y = 'Densidad de probabilidad') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))





#H0: p >= 0.15
#H1: p < 0.15

alpha0 <- 1-pbeta(0.15,18,96)
alpha1 <- pbeta(0.15,18,96)

posterior_odds <- alpha1/alpha0
posterior_odds #alpha1 es bigger, so I go with H1

f1 <- pbeta(0.15,10,90)
f0 <- 1-pbeta(0.15,10,90)

prior_odds <- f1/f0
prior_odds

bf <- posterior_odds/prior_odds
bf
#Si es 1, los nuevos datos no nos han dado nada nuevo