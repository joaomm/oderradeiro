import scala.math.pow

case class Term(coef:Double, exp:Int) {
	require(coef != 0 && exp >= 0)
	
	def unary_- : Term = Term(-coef, exp)
	
	def +(that: Term) = {
		require(exp == that.exp)
		Term(coef + that.coef, exp)
	}
	
	def *(that: Term) = {
		Term(coef * that.coef, exp + that.exp)
	}
	
	def *(list: List[Term]): List[Term] = {
		list.map(term => term * this)
	}
	
	def *(d: Double) = Term(coef*d, exp)
	
	def deriv = Term(coef * exp, exp - 1)
	def derivavel = exp > 0
	
	def apply(x: Double): Double = coef * pow(x, exp)
	
	def degree: Int = exp

	def canBeSummedWith(that: Term) = 
		(coef + that.coef) != 0
		
	override def toString: String = 
		if(exp > 0) coef + "x^" + exp
		else coef.toString
	
}

class Pol private (private val terms: List[Term]){
	
	private def this(coef: Double, exp: Int) = this(List(Term(coef, exp)))
	
	def + (that: Pol): Pol = Pol(Pol.add(this.terms, that.terms))	
	def -(that: Pol): Pol = this + (-that)
	
	def *(that: Pol): Pol = {
		val listaPols = terms.map(term => Pol(term * that.terms))
		listaPols.reduceLeft(_ + _)
	}

	def unary_- : Pol = Pol(terms.map(term => -term))
	def unary_+ : Pol = this
	
	def *(d: Double): Pol = Pol(terms.map(term => term*d))
	
	def degree = terms.head.degree
	def ^(n: Int): Pol = List.fill(n)(this).reduceLeft(_ * _)
	def deriv = Pol(terms.filter(term => term.derivavel).map(term => term.deriv))
	
	def apply(n: Double): Double = terms.map(term => term(n)).reduceLeft(_ + _)
	def apply(p: Pol): Pol = terms.map(term => (p ^ term.exp) * term.coef).reduceLeft(_ + _)
	
	override def toString : String = terms.map(term => term.toString).mkString(" + ")
	
	override def equals(other: Any): Boolean =
		other match {
			case that: Pol => terms == that.terms
			case _ => false
		} 	
}

object Pol {
	def apply(coef: Double, exp: Int) : Pol = new Pol(coef, exp)
	def apply(coef: Double): Pol = new Pol(coef, 0)
	def apply(terms: List[Term]): Pol = new Pol(terms)
	def apply(terms: Term*): Pol = new Pol(terms.toList)
	
	def add(terms1: List[Term], terms2: List[Term]): List[Term] = {
		add(terms1, terms2, Nil)
	}
	
	def add(terms1: List[Term], terms2: List[Term], result: List[Term]): List[Term] = {
		if(terms1.isEmpty)
			return result.reverse ::: terms2
		if(terms2.isEmpty)
			return result.reverse ::: terms1
			
		val term1 = terms1.head
		val term2 = terms2.head
		
		if(term1.exp > term2.exp)
			return add(terms1.tail, terms2, term1 :: result)
			
		if(term1.exp < term2.exp)
			return add(terms1, terms2.tail, term2 :: result)
		
		if(term1 canBeSummedWith term2)
			return add(terms1.tail, terms2.tail, (term1 + term2) :: result)
			
		return add(terms1.tail, terms2.tail, result)
	}
}

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

assert(Pol(1, 2).toString == Term(1,2).toString, "Pol e Term toString")
assert(Term(1, 0).toString == "1.0", "Term com exp = 0")
assert(Pol(1).toString == Term(1, 0).toString, "Criando Pol com so o coef")

//Equals
assert(Term(1,0) == Term(1,0))
assert(Pol(1,0) == Pol(1,0))
assert(Pol(1,0) != Pol(2,0))
assert(Pol(Term(1,1), Term(1,0)) == Pol(Term(1,1), Term(1,0)) )
assert(Pol(Term(1,1), Term(2,0)) != Pol(Term(1,1), Term(1,0)) )

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
Test("Pol * Double") {
	assertEquals(Pol(Term(2,2), Term(1, 0)) * 2, Pol(Term(4,2), Term(2,0)))
}


//Degree
Test("Degree") {
	assertEquals(Pol(6, 3).degree, 3)
}