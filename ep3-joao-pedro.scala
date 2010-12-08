package pfc
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
	
	def /(that: Term) = {
		Term( coef / that.coef, exp - that.exp)
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
	
	def /(that: Pol): Tuple2[Pol, Pol] = {
		Pol.devide(this, that, Pol(List()))
	}

	def unary_- : Pol = Pol(terms.map(term => -term))
	def unary_+ : Pol = this
	
	def +(d: Double): Pol = if(d == 0) this else this + Pol(d, 0)
	def -(d: Double): Pol = if(d == 0) this else this - Pol(d, 0)
	def *(d: Double): Pol = Pol(terms.map(term => term*d))
	def /(d: Double): Pol = this * Pol(1/d, 0)
	
	def degree = terms.head.degree
	def ^(n: Int): Pol = List.fill(n)(this).reduceLeft(_ * _)
	def deriv = Pol(terms.filter(term => term.derivavel).map(term => term.deriv))
	def ! = deriv
	
	def apply(n: Double): Double = terms.map(term => term(n)).reduceLeft(_ + _)
	def apply(p: Pol): Pol = terms.map(term => (p ^ term.exp) * term.coef).reduceLeft(_ + _)
	
	override def toString : String = 
		if(terms.isEmpty) "0"
		else terms.map(term => term.toString).mkString(" + ")
	
	override def equals(other: Any): Boolean =
		other match {
			case that: Pol => terms == that.terms
			case _ => false
		} 	
}

object Pol {

	implicit def doubleToPol(d: Double): Pol = Pol(d, 0)
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
	
	def devide(dividendo: Pol, divisor: Pol, resultado: Pol): Tuple2[Pol, Pol] = {
		if(dividendo.degree < divisor.degree)
			return (resultado, dividendo)
		
		val quocienteTerm: Term = (dividendo.terms.head / divisor.terms.head)
		val restoParcial = dividendo - Pol(quocienteTerm * divisor.terms) 
		
		devide(restoParcial, divisor, resultado + Pol(quocienteTerm))		
	}
}
