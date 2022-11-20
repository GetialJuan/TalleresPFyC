import Derivacion._

//funciones de prueba
val cte = (x:Double) => 2.0 //->' 0
val f = (x:Double) => (x*x) //->' 2x
val g = (x:Double) => (x*x*x) //-> 3x*x
val h = (x:Double) => f(x) / g(x) //->' -1/x*x
val j = (x:Double) => g(x) / f(x) //->' 1

//derivada
derivada(cte)(3)
derivada(f)(3)
derivada(g)(3)
derivada(h)(3)
derivada(j)(3)

//derivadaSuma
derivadaSuma(cte, f)(2)
derivadaSuma(f, g)(2)
derivadaSuma(g, f)(2)
derivadaSuma(h, j)(2)
derivadaSuma(j, h)(2)

//derivadaResta
derivadaResta(cte,f)(4)
derivadaResta(f,g)(4)
derivadaResta(g,f)(4)
derivadaResta(h,j)(4)
derivadaResta(j,h)(4)

//derivadaMult
derivadaMult(cte,f)(1)
derivadaMult(f,g)(1)
derivadaMult(g,f)(1)
derivadaMult(h,j)(1)
derivadaMult(j,h)(1)

//derivadaDiv
derivadaDiv(cte,f)(2)
derivadaDiv(f,g)(2)
derivadaDiv(g,f)(2)
derivadaDiv(h,j)(2)
derivadaDiv(j,h)(2)