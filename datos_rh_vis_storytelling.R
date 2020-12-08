library(tidyverse)
library(scales)
theme_set(theme_minimal())

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



# Gráfico de líneas
ggplot(kpi_long, aes(x = Mes, y = Tasa, group = KPI, color = Sector)) +
  geom_line(size = 1)


ggplot(kpi_long, aes(x = Mes, y = Tasa, group = KPI, color = KPI)) +
  geom_line(size = 1) +
  facet_wrap(~Sector) +
  theme_minimal()






# Etiquetas de los ejes -------------------------


sueldos_puestos <- encuesta %>% 
  select(puesto, sueldo_bruto) %>% 
  group_by(puesto) %>% 
  summarise(sueldo_promedio = mean(sueldo_bruto))

ggplot(sueldos_puestos, aes(x = puesto, y = sueldo_promedio)) +
  geom_col(fill = "#5B61DC") #+
#  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
# labs(x="", y = "", title = "Sueldo promedio por puesto")


sueldos_rubros <- encuesta %>% 
  select(rubro, sueldo_bruto) %>% 
  group_by(rubro) %>% 
  summarise(sueldo_promedio = mean(sueldo_bruto),
            cantidad = n()) %>% 
  arrange(-cantidad) %>% 
  mutate(rubro = fct_recode(rubro, "IT" = "Tecnologías de Información, Sistemas, y afines",
                            "Servicios financieros" = "Servicios financieros; seguros",
                            "Terminales automotrices" = "Terminales automotrices, fábricas autopartistas, y afines")) %>% 
  filter(rubro != "Otros") %>% 
  top_n(10)

sueldos_rubros

# Gráfico base
grafico_rubros <- ggplot(sueldos_rubros, aes(x = reorder(rubro, sueldo_promedio),
                                             y = sueldo_promedio)) +
  geom_col(fill = "#5B61DC") + labs(x = "", y = "") +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))


# Etiquetas rotadas
grafico_rubros +
  theme(axis.text.x = element_text(angle = 90))

# Etiquetas solapadas
grafico_rubros +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Rotar gráfico
grafico_rubros +
  coord_flip()


