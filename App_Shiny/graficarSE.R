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

RESPUESTA = c("BASURERO", "NINGUNO","RECREATIVO",  "OTRO")
VI04 <- data.frame(n,RESPUESTA,total)
#P1 Qu´W uso le dan sus vecinos a la salina?
AMB1 = graficarPlot(VI04,"RESPUESTA", "Número de personas", "1.-¿QUÉ USO LE DAN SUS VECINOS A LA SALINA?")
TAMB1 = graficarTable(VI04,"RESPUESTA", "Número de personas", "1.-¿QUÉ USO LE DAN SUS VECINOS A LA SALINA?")

N = 55
Uno = Ambiental_df[57:111,]
A = sum(Uno$RESPUESTA == 'NINGUNO')
B = sum(Uno$RESPUESTA ==  'SOCIAL')
C = sum(Uno$RESPUESTA == 'AMBIENTAL')
#Uno$RESPUESTA[Uno$RESPUESTA == 'ECONï¿½MICO'] <- "ECONÓMICO"
D = sum(Uno$RESPUESTA == 'ECONï¿½MICO')
total = c(A, B, C, 5)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((5/N)*100)
n = c(A, B, C, 5)
RESPUESTA = c("NINGUNO", "SOCIAL", "AMBIENTAL", "ECONÓMICO")
VI2 <- data.frame(n,RESPUESTA,total)
#P2  �Qu� beneficio recibe de vivir aqu�?
AMB2 = graficarPlot(VI2,"VENTAJAS", "Número de personas", "2.-¿QUÉ BENEFICIO RECIBE DE VIVIR AQUÍ?" )
TAMB2 = graficarTable(VI2,"VENTAJAS", "Número de personas", "2.-¿QUÉ BENEFICIO RECIBE DE VIVIR AQUÍ?" )

Uno = Ambiental_df[113:167,]
A = sum(Uno$RESPUESTA == 'NINGUNO')
B = sum(Uno$RESPUESTA ==  'OTRA')
C = sum(Uno$RESPUESTA == 'AMBIENTAL')
Uno$RESPUESTA[Uno$RESPUESTA == 'ECONï¿½MICA'] <- "ECONÓMICA"
D = sum(Uno$RESPUESTA == 'ECONÓMICA')
total = c(A, B, C, 1)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((1/N)*100)
n = c(A, B, C, D)
RESPUESTA = c("NINGUNO", "OTRA", "AMBIENTAL", "ECONÓMICO")
VI04 <- data.frame(n,RESPUESTA,total)
#P3 Qu� desventajas recibe de vivir aqu� cerca de la salina?
AMB3 = graficarPlot(VI04,"DESVENTAJAS", "Número de personas", "3.-¿QUÉ DESVENTAJA RECIBE DE VIVIR AQUÍ CERCA DE LA SALINA?" )
TAMB3 = graficarTable(VI04,"DESVENTAJAS", "Número de personas", "3.-¿QUÉ DESVENTAJA RECIBE DE VIVIR AQUÍ CERCA DE LA SALINA?" )

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
AMB4 = graficarPlot(VI04,"PERCEPCIÓN ", "Número de personas", "4.-¿EN QUÉ CONDICIONES CONSIDERA QUE SE ENCUENTRA LA SALINA?" )
TAMB4 = graficarTable(VI04,"PERCEPCIÓN ", "Número de personas", "4.-¿EN QUÉ CONDICIONES CONSIDERA QUE SE ENCUENTRA LA SALINA?" )



N = 98
Uno = Ambiental_df[225:279,]
A = sum(Uno$RESPUESTA == 'OLORES')
B = sum(Uno$RESPUESTA ==  'INUNDACIONES')
C = sum(Uno$RESPUESTA ==  'BASURA')
D = sum(Uno$RESPUESTA ==  'AGUAS NEGRAS')
E = sum(Uno$RESPUESTA == 'FLORA Y FAUNA')
FF = sum(Uno$RESPUESTA == 'OTRO')
total = c(40, 18,16, 4,12,8)
A = as.integer((40/N)*100)
B = as.integer((18/N)*100)
C = as.integer((16/N)*100)
D = as.integer((4/N)*100)

E = as.integer((12/N)*100)
FF = as.integer((8/N)*100)

n = c(A, B, C, D,E,FF)
RESPUESTA = c("OLORES", "INUNDACIONES","BASURA", "AGUAS NEGRAS", "FLORA Y FAUNA", "OTRO")
VI04 <- data.frame(n,RESPUESTA,total)
#P5 Qu� efectos genera la condici�n (sucia o contaminada) de la Salina? 
AMB5 = graficarPlot(VI04,"CONSECUENCIAS", "Número de personas", "5.-¿QUÉ EFECTOS GENERA LA CONDICIÓN (SUCIA O CONTAMINADA) DE LA SALINA?" )
TAMB5 = graficarTable(VI04,"CONSECUENCIAS", "Número de personas", "5.-¿QUÉ EFECTOS GENERA LA CONDICIÓN (SUCIA O CONTAMINADA) DE LA SALINA?" )




N=55
Uno = Ambiental_df[280:335,]
A = sum(Uno$RESPUESTA == 'SIEMPRE')
B = sum(Uno$RESPUESTA ==  'A VECES')
C = sum(Uno$RESPUESTA == 'NUNCA')


total = c(A, B, C)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)

n = c(A, B, C)

RESPUESTA =  c("SIEMPRE", "A VECES","NUNCA")
VI04 <- data.frame(n,RESPUESTA,total)
#P6.-¿HAN LLEVADO A CABO ALGUNA ACTIVIDAD DE LMPIEZA, SANEAMIENTO O CONSERVACIÓN DE LA SALINA?
AMB6 = graficarPlot(VI04,"RESPUESTA", "Número de personas", "6.-¿HAN LLEVADO A CABO ALGUNA ACTIVIDAD DE LMPIEZA, SANEAMIENTO O CONSERVACIÓN DE LA SALINA?")
TAMB6 = graficarTable(VI04,"RESPUESTA", "Número de personas", "6.-¿HAN LLEVADO A CABO ALGUNA ACTIVIDAD DE LMPIEZA, SANEAMIENTO O CONSERVACIÓN DE LA SALINA?")



N = 57
Uno = Ambiental_df[280:335,]
A = sum(Uno$RESPUESTA == 'SI')
B = sum(Uno$RESPUESTA ==  'NO')
C = sum(Uno$RESPUESTA == 'NO SE')


total = c(41,8, 8)
A = as.integer((41/57)*100)
B = as.integer((8/57)*100)
C = as.integer((8/57)*100)

n = c(A,B, C)

RESPUESTA = c("SI", "NO","NO SE")
VI04 <- data.frame(n,RESPUESTA,total)
#P1 Qu´W uso le dan sus vecinos a la salina?
AMB7 = graficarPlot(VI04,"RESPUESTA", "Número de personas", "7.-¿ESTÁN CONECTADOS AL DRENAJE?")
TAMB7 = graficarTable(VI04,"RESPUESTA", "Número de personas", "7.-¿ESTÁN CONECTADOS AL DRENAJE?")

#-----------------------------------------------
#------------------  SOCIAL --------------------
#-----------------------------------------------


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
SOC1 = graficarPlot(VI04,"RESPUESTA", "Número de personas", "1.-¿ENTRE LOS VECINOS, REALIZAN  ALGUNA ACTIVIDAD EN COMUN?" )
TSOC1 = graficarTable(VI04,"RESPUESTA", "Número de personas", "1.-¿ENTRE LOS VECINOS, REALIZAN  ALGUNA ACTIVIDAD EN COMUN?" )


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
SOC2 = graficarPlot(VI04,"PROBLEMAS", "Número de personas", "2.-¿CÓMO ES LA RELACIÓN CON SUS VECINOS?" )
TSOC2 = graficarTable(VI04,"PROBLEMAS", "Número de personas", "2.-¿CÓMO ES LA RELACIÓN CON SUS VECINOS?" )

Uno = Social_df[56:111,]
A = sum(Uno$RESPUESTA == 'SIEMPRE')
B = sum(Uno$RESPUESTA ==  'A VECES')
C = sum(Uno$RESPUESTA == 'NUNCA')
total = c(A, B, C)
A = as.integer((A/N)*100)
B = as.integer((B/N)*100)
C = as.integer((C/N)*100)
D = as.integer((D/N)*100)
n = c(A, B, C)
RESPUESTA = c("SIEMPRE", "A VECES", "NUNCA")
VI04 <- data.frame(n,RESPUESTA,total)
#3.		¿Ha tenido problemas con sus vecinos: pleitos, demandas…?
#	Siempre		A veces		Nunca		Otro	
SOC3 =graficarPlot(VI04,"RESPUESTA ", "Número de personas", "3.-¿HA TENIDO PROBLEMAS CON SUS VECINOS: PLEITOS, DEMANDAS…?" )
TSOC3 =graficarTable(VI04,"RESPUESTA ", "Número de personas", "3.-¿HA TENIDO PROBLEMAS CON SUS VECINOS: PLEITOS, DEMANDAS…?" )


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
SOC4 =graficarPlot(VI04,"RESPUESTA ", "Número de personas", "4.-¿CON QUE FRECUENCIA SE HACEN FAVORES ENTRE VECINOS?" )
TSOC4 =graficarTable(VI04,"RESPUESTA ", "Número de personas", "4.-¿CON QUE FRECUENCIA SE HACEN FAVORES ENTRE VECINOS?" )


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
SOC5 = graficarPlot(VI04 ,"RESPUESTA ", "Número de personas", "5.-¿EN ALGÚN PROBLEMA QUE SE LE PRESENTE SUS VECINOS LE AYUDAN?" )
TSOC5 = graficarTable(VI04 ,"RESPUESTA ", "Número de personas", "5.-¿EN ALGÚN PROBLEMA QUE SE LE PRESENTE SUS VECINOS LE AYUDAN?" )


Uno = Social_df[225:279,]
A = sum(Uno$RESPUESTA == 'COMERCIAL')
B = sum(Uno$RESPUESTA ==  'RELIGIOSA')
C = 6
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
SOC6 = graficarPlot(VI04 ,"RESPUESTA ", "Número de personas", "6.-¿PERTENECE A ALGUNA ORGANIZACIÓN? " )
TSOC6 = graficarTable(VI04 ,"RESPUESTA ", "Número de personas", "6.-¿PERTENECE A ALGUNA ORGANIZACIÓN? " )


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
EC1 = graficarPlot(VI04 ,"RESPUESTA ", "Número de personas", "1.-¿CUÁNTAS PERSONAS DE ESTA FAMILIA TRABAJAN? " )
TEC1 = graficarTable(VI04 ,"RESPUESTA ", "Número de personas", "1.-¿CUÁNTAS PERSONAS DE ESTA FAMILIA TRABAJAN? " )

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
EC2 = graficarPlot(VI04 ,"RESPUESTA ", "Número de personas", "2.-¿EN QUÉ TRABAJAN? " )
TEC2 = graficarTable(VI04 ,"RESPUESTA ", "Número de personas", "2.-¿EN QUÉ TRABAJAN? " )

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
EC3 = graficarPlot(VI04 ,"RESPUESTA ", "Número de personas", "3.-¿REALIZAN ALGUNA ACTIVIDAD PRODUCTIVA POR SU CUENTA?" )
TEC3 = graficarTable(VI04 ,"RESPUESTA ", "Número de personas", "3.-¿REALIZAN ALGUNA ACTIVIDAD PRODUCTIVA POR SU CUENTA?" )

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
EC4 = graficarPlot(VI04 ,"RESPUESTA ", "Número de personas", "4.-¿INTERCAMBIAN PRODUCTOS CON SUS VECINOS? " )
TEC4 = graficarTable(VI04 ,"RESPUESTA ", "Número de personas", "4.-¿INTERCAMBIAN PRODUCTOS CON SUS VECINOS? " )

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
EC5 = graficarPlot(VI04 ,"RESPUESTA ", "Número de personas", "5.-¿ SE AYUDAN ENTRE VECINOS PARA ALGÚN TRABAJO QUE BENEFICIE LA ECONOMÍA FAMILIAR DENTRO DE SUS CASAS? " )
TEC5 = graficarTable(VI04 ,"RESPUESTA ", "Número de personas", "5.-¿ SE AYUDAN ENTRE VECINOS PARA ALGÚN TRABAJO QUE BENEFICIE LA ECONOMÍA FAMILIAR DENTRO DE SUS CASAS? " )

