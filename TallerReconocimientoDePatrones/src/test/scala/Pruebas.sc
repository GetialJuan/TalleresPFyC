import Newton._

//Expresiones
val expr00 = Numero(5)
val expr0 = Atomo('x')
val expr1 = Suma(Atomo('x'), Numero(2))
val expr2 = Prod(Atomo('x'), Atomo('x'))
val expr3 = Prod(expr1, Expo(expr2, Numero(5)))
val expr4 = Logaritmo(Atomo('x'))
val expr5 = Prod(Div(expr1, expr2), Resta(expr3, expr4))
val expr6 = Expo(Atomo('x'), Numero(3))

//Funcion mostrar

mostrar(expr00)
mostrar(expr1)
mostrar(expr3)
mostrar(expr4)
mostrar(expr5)

//Funcion derivar

mostrar(derivar(expr00, Atomo('x')))
mostrar(derivar(expr6, Atomo('x')))
mostrar(derivar(expr2, Atomo('x')))
mostrar(derivar(expr2, Atomo('y')))
mostrar(derivar(Suma(Atomo('k'), Prod(Numero(3), Atomo('x'))), Atomo('x')))

//Funcion evaluar

evaluar(Suma(expr1, expr2), Atomo('x'), 5)
evaluar(Prod(expr1, expr2), Atomo('x'), 5)
evaluar(Resta(expr1, expr2), Atomo('x'), 5)
evaluar(Div(expr1, expr2), Atomo('x'), 5)
evaluar(Expo(expr1, expr2), Atomo('x'), 5)
evaluar(Logaritmo(expr1), Atomo('x'), 5)

//Funcion limpiar
limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3), Atomo('x'))), Atomo('x')))
mostrar(limpiar(derivar(Suma(Atomo('k'), Prod(Numero(3), Atomo('x'))), Atomo('x'))))
limpiar(derivar(expr2, Atomo('y')))
mostrar(limpiar(derivar(expr2, Atomo('y'))))
limpiar(derivar(expr2, Atomo('x')))
mostrar(limpiar(derivar(expr2, Atomo('x'))))
limpiar(derivar(expr6, Atomo('x')))
mostrar(limpiar(derivar(expr6, Atomo('x'))))
limpiar(Suma(Prod(Resta(Numero(0), Atomo('x')), Numero(5)), Logaritmo(Suma(Numero(0), Atomo('x')))))
mostrar(limpiar(Suma(Prod(Resta(Numero(0), Atomo('x')), Numero(5)), Logaritmo(Suma(Numero(0), Atomo('x'))))))

//Funcion raizNewton

val e3 = Suma(Resta(Prod(Atomo('x'), Atomo('x')), Numero(4)), Prod(Numero(3), Atomo('x')))
raizNewton(Resta(expr2, Numero(2)), Atomo('x'), 2, buenaAprox)
raizNewton(Resta(expr2, Numero(4)), Atomo('x'), 2, buenaAprox)
raizNewton(e3, Atomo('x'), 2, buenaAprox)
raizNewton(expr1, Atomo('x'), 2, buenaAprox)
raizNewton(expr2, Atomo('x'), 2, buenaAprox)

