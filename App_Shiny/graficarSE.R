N = 55
# --------- AMBIENTAL ---------------
Uno = Ambiental_df[1:55,]
A = sum(Uno$RESPUESTA == 'BASURERO')
B = sum(Uno$RESPUESTA ==  'NINGUNO')
C = sum(Uno$RESPUESTA == 'RECREATIVO')
D = sum(Uno$RESPUESTA == 'OTRO')

total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
n = c(A, B, C, D)

RESPUESTA = c("BASURERO", "RECREATIVO", "NINGUNO", "OTRO")
VI04 <- data.frame(n,RESPUESTA,total)
#P1 Qu´W uso le dan sus vecinos a la salina?
AMB1 = graficarPlot(VI04,"RESPUESTA", "Número de personas", "AMBIENTAL - P1 - USO LE DAN SUS VECINOS A LA SALINA")
TAMB1 = graficarTable(VI04,"RESPUESTA", "Número de personas", "AMBIENTAL - P1 - USO LE DAN SUS VECINOS A LA SALINA")


Uno = Ambiental_df[57:111,]
A = sum(Uno$RESPUESTA == 'NINGUNO')
B = sum(Uno$RESPUESTA ==  'SOCIAL')
C = sum(Uno$RESPUESTA == 'AMBIENTAL')
Uno$RESPUESTA[Uno$RESPUESTA == 'ECONï¿½MICO'] <- "ECONÓMICO"
D = sum(Uno$RESPUESTA == 'ECONÓMICO')
total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
n = c(A, B, C, D)
RESPUESTA = c("NINGUNO", "SOCIAL", "AMBIENTAL", "ECONÓMICO")
VI04 <- data.frame(n,RESPUESTA,total)
#P2  �Qu� beneficio recibe de vivir aqu�?
AMB2 = graficarPlot(VI04,"VENTAJAS", "Número de personas", "AMBIENTAL - P2 - VENTAJAS DE VIVIR CERCA DE LA SALINA" )
TAMB2 = graficarTable(VI04,"VENTAJAS", "Número de personas", "AMBIENTAL - P2 - VENTAJAS DE VIVIR CERCA DE LA SALINA" )

Uno = Ambiental_df[113:167,]
A = sum(Uno$RESPUESTA == 'NINGUNO')
B = sum(Uno$RESPUESTA ==  'OTRA')
C = sum(Uno$RESPUESTA == 'AMBIENTAL')
Uno$RESPUESTA[Uno$RESPUESTA == 'ECONï¿½MICA'] <- "ECONÓMICA"
D = sum(Uno$RESPUESTA == 'ECONÓMICA')
total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
n = c(A, B, C, D)
RESPUESTA = c("NINGUNO", "AMBIENTAL", "ECONÓMICO", "OTRA")
VI04 <- data.frame(n,RESPUESTA,total)
#P3 Qu� desventajas recibe de vivir aqu� cerca de la salina?
AMB3 = graficarPlot(VI04,"DESVENTAJAS", "Número de personas", "AMBIENTAL - P3 - DESVENTAJAS DE VIVIR CERCA DE LA SALINA " )
TAMB3 = graficarTable(VI04,"DESVENTAJAS", "Número de personas", "AMBIENTAL - P3 - DESVENTAJAS DE VIVIR CERCA DE LA SALINA " )

Uno = Ambiental_df[169:223,]
A = sum(Uno$RESPUESTA == 'CONTAMINADA')
B = sum(Uno$RESPUESTA ==  'SUCIA')
C = sum(Uno$RESPUESTA == 'LIMPIA')
D = sum(Uno$RESPUESTA == 'OTRO')
total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
n = c(A, B, C, D)
RESPUESTA = c("CONTAMINADA", "SUCIA", "LIMPIA", "OTRO")
VI04 <- data.frame(n,RESPUESTA,total)
#P4 �En qu� condiciones considera que se encuentra la salina?
AMB4 = graficarPlot(VI04,"PERCEPCIÓN ", "Número de personas", "AMBIENTAL - P4 - PERCEPCIÓN DEL ESTADO DE LA SALINA" )
TAMB4 = graficarTable(VI04,"PERCEPCIÓN ", "Número de personas", "AMBIENTAL - P4 - PERCEPCIÓN DEL ESTADO DE LA SALINA" )


Uno = Ambiental_df[225:279,]
A = sum(Uno$RESPUESTA == 'OLORES')
B = sum(Uno$RESPUESTA ==  'INUNDACIONES')
C = sum(Uno$RESPUESTA == 'FLORA Y FAUNA')
D = sum(Uno$RESPUESTA == 'OTRO')
total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
n = c(A, B, C, D)
RESPUESTA = c("BASURERO", "RECREATIVO", "NINGUNO", "OTRO")
VI04 <- data.frame(n,RESPUESTA,total)
#P5 Qu� efectos genera la condici�n (sucia o contaminada) de la Salina? 
AMB5 = graficarPlot(VI04,"CONSECUENCIAS", "Número de personas", "AMBIENTAL - P5 - CONSECUENCIAS DEL ESTADO ACTUAL DE LA SALINA" )
TAMB5 = graficarTable(VI04,"CONSECUENCIAS", "Número de personas", "AMBIENTAL - P5 - CONSECUENCIAS DEL ESTADO ACTUAL DE LA SALINA" )


# ---------  SOCIAL ---------------

Uno = Social_df_2[1:55,]
A = sum(Uno$RESPUESTA == 'SI')
B = sum(Uno$RESPUESTA ==  'NO')
#C = sum(Uno$RESPUESTA == 'RECREATIVO')
#D = sum(Uno$RESPUESTA == 'OTRO')
total = c(A, B)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)

n = c(A, B)
RESPUESTA = c("SI", "NO")
VI04 <- data.frame(n,RESPUESTA,total)
#P1 Entre los vecinos, realizan alguna actividad en com�n:
#1.		Entre los vecinos, realizan alguna actividad en común:  fiestas, reuniones vecinales, levantar quejas etc
SOC1 = graficarPlot(VI04,"RESPUESTA", "Número de personas", "SOCIAL - P1 - ACTIVIDADES VECINALES EN COMUN" )
TSOC1 = graficarTable(VI04,"RESPUESTA", "Número de personas", "SOCIAL - P1 - ACTIVIDADES VECINALES EN COMUN" )


Uno = Social_df[1:55,]
A = sum(Uno$RESPUESTA == 'BUENA')
B = sum(Uno$RESPUESTA ==  'NO LOS CONOZCO')
C = sum(Uno$RESPUESTA == 'MALA')
D = sum(Uno$RESPUESTA == 'OTRO')
total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
n = c(A, B, C, D)
RESPUESTA = c("BUENA", "NO LOS CONOZCO", "MALA", "OTRO")
VI04 <- data.frame(n,RESPUESTA,total)
#2.		¿Cómo es la relación con sus vecinos?
#	Buena		Mala		No los conozco		Otro	
SOC2 = graficarPlot(VI04,"PROBLEMAS", "Número de personas", "SOCIAL - P2 - RELACIÓN ENTRE VECINOS " )
TSOC2 = graficarTable(VI04,"PROBLEMAS", "Número de personas", "SOCIAL - P2 - RELACIÓN ENTRE VECINOS " )

Uno = Social_df[56:111,]
A = sum(Uno$RESPUESTA == 'SIEMPRE')
B = sum(Uno$RESPUESTA ==  'A VECES')
C = sum(Uno$RESPUESTA == 'NUNCA')
D = sum(Uno$RESPUESTA == 'OTRO')
total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
n = c(A, B, C, D)
RESPUESTA = c("SIEMPRE", "A VECES", "NUNCA", "OTRO")
VI04 <- data.frame(n,RESPUESTA,total)
#3.		¿Ha tenido problemas con sus vecinos: pleitos, demandas…?
#	Siempre		A veces		Nunca		Otro	
SOC3 =graficarPlot(VI04,"RESPUESTA ", "Número de personas", "SOCIAL - P3 - ¿Ha tenido problemas con sus vecinos: pleitos, demandas?" )
TSOC3 =graficarTable(VI04,"RESPUESTA ", "Número de personas", "SOCIAL - P3 - ¿Ha tenido problemas con sus vecinos: pleitos, demandas?" )


Uno = Social_df[113:167,]
A = sum(Uno$RESPUESTA == 'SIEMPRE')
B = sum(Uno$RESPUESTA ==  'A VECES')
C = sum(Uno$RESPUESTA == 'NUNCA')
#D = sum(Uno$RESPUESTA == 'OTRO')
total = c(A, B, C)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
n = c(A, B, C)
RESPUESTA = c("SIEMPRE", "A VECES", "NUNCA")
VI04 <- data.frame(n,RESPUESTA,total)
#4.		¿Con qué frecuencia se hacen favores entre vecinos?
#	Siempre		A veces		Nunca		
SOC4 =graficarPlot(VI04,"RESPUESTA ", "Número de personas", "SOCIAL - P4 - ¿Con qué frecuencia se hacen favores entre vecinos?" )
TSOC4 =graficarTable(VI04,"RESPUESTA ", "Número de personas", "SOCIAL - P4 - ¿Con qué frecuencia se hacen favores entre vecinos?" )


Uno = Social_df[169:223,]
A = sum(Uno$RESPUESTA == 'SIEMPRE')
B = sum(Uno$RESPUESTA ==  'A VECES')
C = sum(Uno$RESPUESTA == 'NUNCA')
#D = sum(Uno$RESPUESTA == 'OTRO')
total = c(A, B, C)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
n = c(A, B, C)
RESPUESTA = c("SIEMPRE", "A VECES", "NUNCA")
VI04 <- data.frame(n,RESPUESTA,total)
#5.		¿En algún problema que se le presente, sus vecinos le ayudan?
#	Siempre		A veces		Nunca		
SOC5 = graficarPlot(VI04 ,"RESPUESTA ", "Número de personas", "SOCIAL - P5 - ¿En algún problema que se le presente, sus vecinos le ayudan?" )
TSOC5 = graficarTable(VI04 ,"RESPUESTA ", "Número de personas", "SOCIAL - P5 - ¿En algún problema que se le presente, sus vecinos le ayudan?" )


Uno = Social_df[225:279,]
A = sum(Uno$RESPUESTA == 'COMERCIAL')
B = sum(Uno$RESPUESTA ==  'RELIGIOSA')
C = sum(Uno$RESPUESTA == 'POLÍTICA')
D = sum(Uno$RESPUESTA == 'VECINAL')
E = sum(Uno$RESPUESTA == 'NINGUNA')
total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C, D,E)
RESPUESTA = c("COMERCIAL", "RELIGIOSA", "POLITICA", "VECINAL", "NINGUNA")
VI04 <- data.frame(n,RESPUESTA,total)
#6.		¿Pertenecen a alguna organización? 
#	Comercial		Religiosa		Política		Vecinal 		Ninguna	
SOC6 = graficarPlot(VI04 ,"RESPUESTA ", "Número de personas", "SOCIAL - P6 - ¿Pertenecen a alguna organización? " )
TSOC6 = graficarTable(VI04 ,"RESPUESTA ", "Número de personas", "SOCIAL - P6 - ¿Pertenecen a alguna organización? " )


#-----------------------------------------------
# ECONOMICO 

Uno = Economico_df[1:55,]
A = sum(Uno$RESPUESTA == 'TODOS')
B = sum(Uno$RESPUESTA ==  'UNO')
C = sum(Uno$RESPUESTA == 'NINGUNO')
D = sum(Uno$RESPUESTA == 'OTRO')
total = c(A, B, C, D)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
n = c(A, B, C,D)
RESPUESTA = c("TODOS", "UNO", "NINGUNO", "OTRO")
VI04 <- data.frame(n,RESPUESTA,total)
#1.		¿Cuántas personas de esta familia trabajan? 
#	Todos		Uno		Ninguno		Otro
EC1 = graficarPlot(VI04 ,"RESPUESTA ", "Número de personas", "ECONÓMICO - P1 - ¿Cuántas personas de esta familia trabajan? " )
TEC1 = graficarTable(VI04 ,"RESPUESTA ", "Número de personas", "ECONÓMICO - P1 - ¿Cuántas personas de esta familia trabajan? " )

Uno = Economico_df[57:111,]
A = sum(Uno$RESPUESTA == 'PESCA')
B = sum(Uno$RESPUESTA ==  'TURISMO')
C = sum(Uno$RESPUESTA == 'GOBIERNO')
D = sum(Uno$RESPUESTA == 'OTRO')
E = sum(Uno$RESPUESTA == 'NINGUNO')
total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C,D,E)
RESPUESTA = c("PESCA", "TURISMO", "GOBIERNO", "OTRO","NINGUNO")
VI04 <- data.frame(n,RESPUESTA,total)
#2.		¿En qué trabajan?
#	Pesca		Turismo		Gobierno		Comercio		Otro		
#Mencione en dónde
EC2 = graficarPlot(VI04 ,"RESPUESTA ", "Número de personas", "ECONÓMICO - P2 - ¿En qué trabajan? " )
TEC2 = graficarTable(VI04 ,"RESPUESTA ", "Número de personas", "ECONÓMICO - P2 - ¿En qué trabajan? " )

Uno = Economico_df[113:167,]
A = sum(Uno$RESPUESTA == 'ARTESANIA')
B = sum(Uno$RESPUESTA ==  'MANUALIDAD')
C = sum(Uno$RESPUESTA == 'OFICIO')
D = sum(Uno$RESPUESTA == 'CULTIVO')
E = sum(Uno$RESPUESTA == 'NINGUNA')
total = c(A, B, C, D, E)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
E = as.integer((E/N)*100)
n = c(A, B, C,D,E)
RESPUESTA = c("ARTESANÍA", "MANUALIDAD", "OFICIO", "CULTIVO","NINGUNA")
VI04 <- data.frame(n,RESPUESTA,total)
#3.		¿Realizan alguna actividad productiva por su cuenta: manualidad, artesanía u oficio, cultivo de hortalizas?
#	Artesanía		Manualidad		Oficio		Cultivo		Ninguna	
EC3 = graficarPlot(VI04 ,"RESPUESTA ", "Número de personas", "ECONÓMICO - P3 - ¿Realizan alguna actividad productiva por su cuenta: manualidad, artesanía u oficio, cultivo de hortalizas?" )
TEC3 = graficarTable(VI04 ,"RESPUESTA ", "Número de personas", "ECONÓMICO - P3 - ¿Realizan alguna actividad productiva por su cuenta: manualidad, artesanía u oficio, cultivo de hortalizas?" )

Uno = Economico_df[169:223,]
A = sum(Uno$RESPUESTA == 'SIEMPRE')
B = sum(Uno$RESPUESTA ==  'A VECES')
C = sum(Uno$RESPUESTA == 'NUNCA')
#D = sum(Uno$RESPUESTA == 'OTRO')
total = c(A, B, C)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)

n = c(A, B, C)
RESPUESTA = c("SIEMPRE", "A VECES", "NUNCA")
VI04 <- data.frame(n,RESPUESTA,total)
#4.		¿Intercambian productos con sus vecinos?
#	Siempre		A veces		Nunca		
EC4 = graficarPlot(VI04 ,"RESPUESTA ", "Número de personas", "ECONÓMICO - P4 - ¿Intercambian productos con sus vecinos? " )
TEC4 = graficarTable(VI04 ,"RESPUESTA ", "Número de personas", "ECONÓMICO - P4 - ¿Intercambian productos con sus vecinos? " )

Uno = Economico_df[225:279,]
A = sum(Uno$RESPUESTA == 'SIEMPRE')
B = sum(Uno$RESPUESTA ==  'A VECES')
C = sum(Uno$RESPUESTA == 'NUNCA')
#D = sum(Uno$RESPUESTA == 'OTRO')

total = c(A, B, C)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
n = c(A, B, C)
RESPUESTA = c("SIEMPRE", "A VECES", "NUNCA")
VI04 <- data.frame(n,RESPUESTA,total)
#5.		¿Se ayudan entre vecinos para algún trabajo que beneficie la economía familiar dentro de sus casas?
#	Siempre		A veces		Nunca		
EC5 = graficarPlot(VI04 ,"RESPUESTA ", "Número de personas", "ECONÓMICO - P5 - ¿Se ayudan entre vecinos para algún trabajo que beneficie la economía familiar dentro de sus casas? " )
TEC5 = graficarTable(VI04 ,"RESPUESTA ", "Número de personas", "ECONÓMICO - P5 - ¿Se ayudan entre vecinos para algún trabajo que beneficie la economía familiar dentro de sus casas? " )

