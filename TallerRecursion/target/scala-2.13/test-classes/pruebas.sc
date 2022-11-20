import Recursion. {mcdTFA, mcdEBez, fibonacciA, fibonacciI}

//Pruebas mcdTFA
mcdTFA(List(3), List(0), List(2)) //mcd(8,1) = 1
mcdTFA(List(1,2,1,1), List(2,1,2,1), List(2,3,5,7)) //mcd(630,2100) = 210
mcdTFA(List(1,1,0,1,0), List(1,0,2,0,1), List(2,3,5,7,11)) //mcd(42,550) = 2
mcdTFA(List(2,0,0,0,1), List(2,1,0,0,0), List(2,3,5,7,11)) //mcd(44,12) = 4
mcdTFA(List(1,0,1,0,0,0,0,0,1), List(2,0,1,1,0,0,0,0,0), List(2,3,5,7,11,13,17,19,23)) //mcd(230,140) = 10

//Pruebas mcdEBez
mcdEBez(5,0)
mcdEBez(44,12)
mcdEBez(230,140)
mcdEBez(963,657)
mcdEBez(30263,20657)

//Pruebas fibonacciA
fibonacciA(0)
fibonacciA(1)
fibonacciA(10)
fibonacciA(30)
fibonacciA(45)

//Pruebas fibonacciI
fibonacciI(0)
fibonacciI(1)
fibonacciI(10)
fibonacciI(30)
fibonacciI(45)

