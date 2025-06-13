library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
library(gridExtra)
data(murders)
.
## -----------------------------------------------------------------
ggplot(data = murders)
murders %>% ggplot()
p <- ggplot(data = murders)
class(p)
print(p) # esto es equivalente a simplemente escribir p
p

murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total))

# agregar capa de puntos a objeto ggplot predefinido
p <- ggplot(data = murders) + geom_point(aes(population/10^6, total))

p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))

## -----------------------------------------------------------------
# cambiar el tamaño de los puntos
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb))

# mover etiquetas de texto ligeramente a la derecha
p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1)

# simplificar código añadiendo mapeo estético global
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) +
  geom_text(nudge_x = 1.5)

# los mapeos estéticos locales sobrescriben los globales
p + geom_point(size = 3) +
  geom_text(aes(x = 10, y = 800, label = "Hello there!"))

## -----------------------------------------------------------------
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))

# escalar en logaritmo base 10 el eje x y el eje y
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

# escalar eficientemente en logaritmo los ejes
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()

p + geom_point(size = 3) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Población en millones (escala logarítmica)") +
  ylab("Número total de asesinatos (escala logarítmica)") +
  ggtitle("Asesinatos por Armas de Fuego en EE.UU. en 2010")

# redefinir p para ser todo excepto la capa de puntos
p <- murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Población en millones (escala logarítmica)") +
  ylab("Número total de asesinatos (escala logarítmica)") +
  ggtitle("Asesinatos por Armas de Fuego en EE.UU. en 2010")

# hacer todos los puntos azules
p + geom_point(size = 3, color = "blue")

# colorear puntos por región
p + geom_point(aes(col = region), size = 3)

# definir la tasa media de asesinatos
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

# línea básica con la tasa media de asesinatos para el país
p <- p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r)) # la pendiente es por defecto de 1

# cambiar la línea a discontinua y gris oscuro, línea debajo de los puntos
p +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3)

p <- p + scale_color_discrete(name = "Región") # capitalizar el título de la leyenda

## -----------------------------------------------------------------
# tema utilizado para gráficos en el libro de texto y el curso
ds_theme_set()

# temas de ggthemes
p + theme_economist() # estilo de la revista The Economist
p + theme_fivethirtyeight() # estilo del sitio web FiveThirtyEight

# definir la intersección
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate

# crear el gráfico, combinando todos los elementos
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Población en millones (escala logarítmica)") +
  ylab("Número total de asesinatos (escala logarítmica)") +
  ggtitle("Asesinatos por Armas de Fuego en EE.UU. en 2010") +
  scale_color_discrete(name = "Región") +
  theme_economist()

## -----------------------------------------------------------------
# definir p
p <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height))
# histogramas básicos
p + geom_histogram()
p + geom_histogram(binwidth = 1)

# histograma con relleno azul, contorno negro, etiquetas y título
p + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Alturas masculinas en pulgadas") +
  ggtitle("Histograma")

p + geom_density()
p + geom_density(fill = "blue")

# gráfico QQ básico
p <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
p + geom_qq()

# gráfico QQ contra una distribución normal con la misma media/desviación estándar que los datos
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
  geom_abline()

# gráfico QQ de datos escalados contra la distribución normal estándar
heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

# definir gráficos p1, p2, p3
p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")

# organizar gráficos uno al lado del otro en 1 fila, 3 columnas
grid.arrange(p1, p2, p3, ncol = 3)
