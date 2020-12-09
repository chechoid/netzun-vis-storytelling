library(tidyverse)          # Transformación y limpieza de datos
library(scales)             # Manipula formatos de número
theme_set(theme_minimal())  # Setea el estilo de los gráficos                                                                             

# Cargar archivos -------------------------------

# KPIs Ausentismo y Horas Extras
kpi <- read_delim("Datos/kpi_aus_he.csv", delim = ";")


# Transformar el dataset a uno largo
kpi_long <- kpi %>% 
  pivot_longer(cols = c("ene":"dic"),
               names_to = "Mes", 
               values_to = "Tasa")

# Encuesta de Sueldos
encuesta <- read_delim("https://raw.githubusercontent.com/r4hr/r4hr_introduccion_dplyr/main/Datos/encuesta_sueldos.csv",
                       delim = ",")

# Organiza la jerarquía en los puestos de la encuesta.
encuesta <- encuesta %>% 
  mutate(puesto = factor(puesto, levels = c("Director", "Gerente", "Jefe",
                                            "Responsable", "HRBP", "Analista",
                                            "Administrativo")))






# Ejemplos --------------------------------------

# Definir orden de variable Mes
kpi_long <- kpi_long %>% 
  mutate(Mes = factor(Mes, levels = c("ene", "feb", "mar", "abr",
                                      "may", "jun", "jul", "ago",
                                      "sep", "oct", "nov", "dic")))



# Gráfico de líneas según la grámatica de los gráficos

ggplot(kpi_long,               # Fuente de datos a graficar
       aes(x = Mes,            # Variable del eje horizontal
           y = Tasa,           # Variable del eje vertical
           group = KPI,        # Criterio para agrupar las líneas del gráfico
           color = KPI)) +     # Define el color de la línea
  geom_line(size = 1) +        # Tipo de gráfico y grosor de la línea
  facet_wrap(~Sector)          # Crea subgráficos divididos por sector






# Etiquetas de los ejes -------------------------

# Creamos un dataframe para calcular los sueldos promedios por puestos

sueldos_puestos <- encuesta %>%    # Crea un dataframe nuevo llamado sueldos_puestos
  select(puesto, sueldo_bruto) %>% # Elige las variables para trabajar
  group_by(puesto) %>%             # Agrupa los resultados por puestos
  summarise(sueldo_promedio = mean(sueldo_bruto)) # Cálculo de sueldo promedio



# Gráfico sin separador de miles en eje vertical y nombres originales de los ejes

ggplot(sueldos_puestos,               # Fuente de datos a graficar
       aes(x = puesto,                # Variable del eje horizontal
           y = sueldo_promedio)) +    # Variable del eje vertical
  geom_col(fill = "#5B61DC")          # Define el tipo de gráfico y el color



# Mismo gráfico, con separador miles y sin nombres originales

ggplot(sueldos_puestos,               # Fuente de datos a graficar
       aes(x = puesto,                # Variable del eje horizontal
           y = sueldo_promedio)) +    # Variable del eje vertical
  geom_col(fill = "#5B61DC")  +       # Define el tipo de gráfico y el color
# Agrega el separador de miles al eje y
 scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
 labs(x="", y = "",                   # Elimina los nombres originales de las variables
      title = "Sueldo promedio por puesto") # Agrega un título




# Creamos un dataframe para calcular los sueldos promedio por rubro
sueldos_rubros <- encuesta %>%         # Crea un dataframe nuevo llamado sueldos_rubros
  select(rubro, sueldo_bruto) %>%      # Selecciona las variables rubro y sueldo_bruto
  group_by(rubro) %>%                  # Agrupa por rubro
  summarise(sueldo_promedio = mean(sueldo_bruto),   # Calcula sueldo promedio
            cantidad = n()) %>%        # Agrega una columna con cantidad de casos
  arrange(-cantidad) %>%               # Ordena descendentemente las respuestas
  # Cambia el nombre de algunas respuestas para que sean más cortas
  mutate(rubro = fct_recode(rubro, "IT" = "Tecnologías de Información, Sistemas, y afines",
                            "Servicios financieros" = "Servicios financieros; seguros",
                            "Terminales automotrices" = "Terminales automotrices, fábricas autopartistas, y afines")) %>% 
  filter(rubro != "Otros") %>%        # Elimina las respuestas "Otros" del campo rubro
  top_n(10)                           # Nos quedamos con el top 10 de respuestas



# Guardamos en un objeto el gráfico base así simplificamos el código despues
grafico_rubros <- ggplot(sueldos_rubros,                           # Fuente de datos a graficar
                         aes(x = reorder(rubro, sueldo_promedio),  # Datos en el eje horizontal ordenados por sueldo_promedio
                                             y = sueldo_promedio)) + # Datos en el eje vertical
  geom_col(fill = "#5B61DC") +    # Tipo de gráfico y color de relleno
  labs(x = "", y = "") +          # Elimina nombres originales de los ejes
  # Agrega separador de miles al eje vertical
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))


# Gráfico base, con los nombres de las etiquetas del eje x superpuestos
grafico_rubros

# Gráficos con etiquetas rotadas
grafico_rubros +
  theme(axis.text.x = element_text(angle = 90))

# Gráfico con etiquetas escalonadas
grafico_rubros +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Rotar gráfico
grafico_rubros +
  coord_flip()


