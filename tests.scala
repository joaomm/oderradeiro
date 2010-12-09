import pfc._

implicit def doubleToPol(d: Double): Pol = Pol(d, 0)

def assertEquals(actual: Any, expected: Any) = {
	try{
		assert(actual == expected)
		println(" OK!")
	}
	catch {
		case ex: AssertionError => {
			println(" Falha!")
			println("   expected: [" + expected + "]")
			println("   actual: [" + actual + "]")
			println()
		}
	}
}

object Test {
	def apply(name: String)(body: => Unit){
		print("test: " + name)
		body
	}

}

Test("Pol e Term toString") {
	assertEquals(Pol(1,2).toString, Term(1,2).toString)
}

Test("Term com exp = 0") {
	assertEquals(Term(1, 0).toString, "1.0")
}

Test("Criando Pol com so o coef")  {
	assertEquals(Pol(1).toString, Term(1, 0).toString)	
}

//Equals
Test("Equals") {
	assertEquals(Term(1,0), Term(1,0))
	assertEquals(Pol(1,0), Pol(1,0))
	assertEquals(Pol(Term(1,1), Term(1,0)),  Pol(Term(1,1), Term(1,0)))
	
	assert(Pol(1,0) != Pol(2,0))
	assert(Pol(Term(1,1), Term(2,0)) != Pol(Term(1,1), Term(1,0)) )
}

//SOMA
Test("Soma de Polinomios grau 0") {
	assertEquals(Pol(1, 0) + Pol(2, 0),  Pol(3,0))
}

Test("Soma de Polinomios de grau 5") {
	assertEquals(Pol(1, 5) + Pol(3, 5),  Pol(4, 5))	
}

Test("Soma de Pols graus diferentes") {
	assertEquals(Pol(1,0) + Pol(2,1), Pol(Term(2,1), Term(1,0)))
}

Test("Soma de Pols graus diferentes invertidos") {
	assertEquals(Pol(2,1) + Pol(1,0), Pol(Term(2,1), Term(1,0)))	
}

Test("Soma de Pols Diferentes") {
	assertEquals(Pol(1,0) + Pol(2,1), Pol(Term(2,1), Term(1,0)))
}

Test("Soma de Pols com varios termos") {
	val p1 = Pol(3, 1)
	val p2 = Pol(Term(5, 5), Term(6, 0))
	val esperado = Pol(Term(5, 5), Term(3, 1), Term(6, 0))
	assertEquals(p1 + p2, esperado)	
}

Test("Soma de Pols resultando em termo.coef == 0") {
	val p1 = Pol(-1, 0) + Pol(1, 1)
	val p2 = Pol(1, 0)
	assertEquals(p1 + p2, Pol(1, 1))
}

//-Pol
Test("Polinomio negado com um termo") {
	val actual = -(Pol(1, 0) + Pol(3, 2))
	val expected = Pol(Term(-3, 2), Term(-1, 0))
	assertEquals(actual, expected)
}

//Subtração
Test("Subtracao") {
	val actual = (Pol(2, 0) + Pol(3, 2)) - (Pol(2, 2) + Pol(2, 1))
	val expected = (Pol(Term(1, 2), Term(-2, 1), Term(2, 0)))
	assertEquals(actual, expected)
}

//Derivacao
Test("Derivando um termo facil") {
	assertEquals(Term(2, 1).deriv, Term(2, 0))
}

Test("Derivando um polinomio com termo facil") {
	assertEquals(Pol(2, 1).deriv, Pol(2, 0))
	assertEquals(Pol(2, 1)!, Pol(2, 0))
}

Test("Derivando um polinomio com termo que vai sumir") {
	assertEquals(Pol(Term(2, 1), Term(1, 0)).deriv, Pol(2, 0))
}

//Multiplicacao
Test("Multiplicando dois terms") {
	assertEquals(Term(2,0) * Term(3,1), Term(6,1))
}

Test("Multiplicando dois polinomios simples") {
	assertEquals(Pol(2,0) * Pol(3,1), Pol(6,1))
}

Test("Multiplicando um polinomio simples e um complexo") {
	assertEquals(Pol(Term(3,2), Term(1, 1)) * Pol(2,0), Pol(Term(6,2), Term(2,1)))
}

Test("Multiplicando um polinomio simples e um complexo invertido") {
	assertEquals(Pol(2,0) * Pol(Term(3,2), Term(1, 1)), Pol(Term(6,2), Term(2,1)))
}

//Potenciacao
Test("Potencia de Polinomios") {
	val actual = Pol(Term(2,2), Term(2,0)) ^ 3
	val expected = Pol(Term(8,6), Term(24,4), Term(24, 2), Term(8,0))
	assertEquals(actual, expected)
}

//Apply
Test("Apply de Double") {
	val p = Pol(Term(3,3), Term(-1, 2), Term(5, 1), Term(-2, 0))
	assertEquals(p(3), 85)
}

Test("Apply de Polinomios") {
	val p = Pol(Term(1, 2), Term(1,1))
	val y = Pol(Term(1,1), Term(-1,0))
	assertEquals(p(y), Pol(Term(1,2), Term(-1,1)))
}

//Double
Test("Pol + Double") {
	assertEquals((Pol(1, 0) + 2) + 0, Pol(3, 0))
	assertEquals(Pol(1, 2) + 2, Pol(1, 2) + Pol(2, 0))
}

Test("Pol - Double") {
	assertEquals((Pol(1, 0) - 2) + 0, Pol(-1, 0))
}

Test("Pol * Double") {
	assertEquals(Pol(Term(2,2), Term(1, 0)) * 2, Pol(Term(4,2), Term(2,0)))
}

Test("Pol / Double") {
	assertEquals(Pol(Term(4,2), Term(2, 0)) / 2, Pol(Term(2,2), Term(1,0)))
}


//Implicit doubleToPol
Test("doubleToPol"){
	assertEquals(2 + Pol(Term(4,2), Term(2, 0)), 4 - Pol(Term(-4,2), Term(4,0)) + 4)
}

//Degree
Test("Degree") {
	assertEquals(Pol(6, 3).degree, 3)
}

//POLINOMIO NULO
Test("toString do polinomio nulo deve ser 0") {
	assertEquals((Pol(1, 1) - Pol(1, 1)).toString, "0")
}

//Divisão
Test("divisao de dois termos") {
	assertEquals(Term(2,4) / Term(1, 2), Term(2,2))
}

Test("divisao de dois polinomios") {
	val dividendo = Pol(Term(2, 4), Term(4, 3), Term(3, 2), Term(1, 1), Term(1,0))
	val divisor = Pol(Term(1, 2), Term(-1, 0))
	val quociente = Pol(Term(2,2), Term(4,1), Term(5,0))
	val resto = Pol(Term(5,1), Term(6,0))
	assertEquals(dividendo / divisor, (quociente, resto))
}

Test("divisao de dois polinomios 2") {
	val dividendo = Pol(Term(-6, 2), Term(4, 0))
	val divisor = Pol(Term(1, 1), Term(1, 0))
	val quociente = Pol(Term(-6,1), Term(6,0))
	val resto = Pol(Term(-2,0))
	assertEquals(dividendo / divisor, (quociente, resto))
}

//Hashcode
Test("deve ter um hashcode") {
	val pol1 = Pol(Term(1, 2), Term(1,1))
	val pol2 = Pol(1, 2) + Pol(1, 1)
	assertEquals(pol2, pol1)
	assertEquals(pol2.hashCode, pol1.hashCode)
}