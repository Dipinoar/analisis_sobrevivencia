# Dockerfile para desplegar app_analisis_completo.R
# Base image: Shiny Server sobre Debian/Ubuntu
# Forzamos linux/amd64 para compatibilidad con Render y Macs M1/M2 (vía Rosetta)
FROM --platform=linux/amd64 rocker/shiny:4.3.1

# 1. Instalar dependencias del sistema necesarias para compilar paquetes de R
# libnlopt-dev es necesario para 'car' / 'lme4'
# libxml2-dev para 'XML'/'xml2'
# cmake a veces se requiere
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libnlopt-dev \
    cmake \
    libglpk-dev \
    libgmp3-dev \
    libmpfr-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# 2. Instalar paquetes de R
# Se instalan explícitamente los paquetes usados en app_analisis_completo.R
RUN R -e "install.packages(c('shinydashboard', 'DT', 'ggplot2', 'dplyr', 'tidyr', 'survival', 'survminer', 'plotly', 'scales', 'robustbase', 'car', 'rlang'), repos='https://cran.rstudio.com/')"

# 3. Limpiar el directorio por defecto de Shiny Server
RUN rm -rf /srv/shiny-server/*

# 4. Copiar los archivos de la aplicación
# En la carpeta deploy_shinyapps, el archivo ya se llama app.R
COPY app.R /srv/shiny-server/
COPY datos_modelo_final.rds /srv/shiny-server/
COPY modelo_final.rds /srv/shiny-server/
COPY a35ad07d-eceb-45e0-8c34-1ac40f634652.csv /srv/shiny-server/

# 5. Exponer el puerto
EXPOSE 3838

# 6. Comando de inicio (ya definido en la imagen base, pero explícito aquí)
CMD ["/usr/bin/shiny-server"]
