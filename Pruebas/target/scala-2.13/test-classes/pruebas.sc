import Canicas._


/*Pruebas para canicas posibles por cada frasco 1.1.1*/
canicasPosiblesFrasco(2,5) // Ejemplo del taller
canicasPosiblesFrasco(1,0) // Frasco que no pueda almacenar ni una canica
canicasPosiblesFrasco(1,1) // Frasco que solo pueda almacenar una canica
canicasPosiblesFrasco(1,30) // Frasco que puede almacenar 30 canicas
canicasPosiblesFrasco(1000,2) // Frasco que solo puede almacenar 2 canicas

/*Pruebas para canicas posibles por frasco 1.1.2*/
canicasPorFrasco(3,5) // Ejemplo del taller
canicasPorFrasco(0,0) // No hay frascos ni canicas a distribuir
canicasPorFrasco(0,5) // No hay frascos pero hay canicas a distribuir
canicasPorFrasco(1,0) // Hay un frasco pero cero canicas a distribuir
canicasPorFrasco(1,1) // Hay un frasco y una canica a distribuir
canicasPorFrasco(1,2) // Hay un frasco y dos canicas a distribuir

/*Pruebas para combinaciones de canicas por frasco 1.1.3*/
mezclarLCanicas(canicasPorFrasco(3,5)) // Ejemplo del taller
mezclarLCanicas(canicasPorFrasco(0,0)) // No hay frascos ni canicas a distribuir
mezclarLCanicas(canicasPorFrasco(0,5)) // No hay frascos pero hay canicas a distribuir
mezclarLCanicas(canicasPorFrasco(1,0)) // Hay un frasco pero cero canicas a distribuir
mezclarLCanicas(canicasPorFrasco(2,0)) // Hay dos frascos pero cero canicas a distribuir
mezclarLCanicas(canicasPorFrasco(2,1)) // Hay dos frascos y una canica a distribuir
mezclarLCanicas(canicasPorFrasco(2,2)) // Hay dos frascos y dos canicas a distribuir

/*Pruebas Calculando las distribuciones requeridad 1.1.4*/
distribucion(10,3,5) // Ejemplo del taller
distribucion(0,0,0) // No hay canicas ni frascos ni un maximo de canicas por frasco
distribucion(0,0,5) // No hay canicas ni frascos pero si un maximo de canicas por frasco
distribucion(0,3,0) // No hay canicas pero si frascos y un maximo de canicas por frasco
distribucion(0,3,5) // Hay 0 canicas a distribuir en 3 frascos con un maximo de 5 canicas por frasco
distribucion(2,2,1) // Hay dos canicas a distribuir en 2 frascos con un maximo de 1 canica por frasco
distribucion(5,5,0) // Los frascos tienen capacidad para 0 canicas

/*Pruebas de calculo de posibles agrupaciones crecientes 1.2*/
agrupaciones(6) // Ejemplo del taller
agrupaciones(0) // Ejemplo del taller
agrupaciones(1) // Ejemplo del taller
agrupaciones(2) // Ejemplo del taller
agrupaciones(5) // Ejemplo del taller
