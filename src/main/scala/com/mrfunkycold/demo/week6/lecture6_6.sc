val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)

val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

capitalOfCountry("US")
//capitalOfCountry("Andorra") //produces an error :(

capitalOfCountry.get("US")

capitalOfCountry.get("South Africa")


def showCapital(country: String) = capitalOfCountry.get(country) match {
  case Some(capital) => capital
  case None => "Missing capital"
}

showCapital("US")

val fruit = List("apple", "pear", "orange", "pineapple")

fruit.sorted
fruit.sortWith(_.length < _.length)

fruit.groupBy(x => x.head)

//------------------------------------------------------------



//Polynomials
// x^3 - 2x + 5

//Map of exponents to coefficients
Map(0 -> 5, 1 -> -2, 3 -> 1)

//Design a class Polynom that represents polynomials as map

class Poly(terms0: Map[Int, Double]) {
  //Removing the need to create a map
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  val terms = terms0 withDefaultValue 0.0

 // def + (other: Poly) = new Poly(terms ++ (other.terms map adjust))

  //exercise
  def + (other: Poly) =
    new Poly((other.terms foldLeft terms)(addTerm))

  private def addTerm(terms: Map[Int, Double], term: (Int, Double)):Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))
  }


  private def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term

    exp -> (terms(exp) + coeff)

    //No longer needed since we have a default value
    /*terms get exp match {
      case Some(c) => exp -> (c + coeff)
      case None => exp -> coeff
    } */
  }
  override def toString: String = (
      for{
       (exp,coef) <- terms.toList.sorted.reverse
      }
      yield if(exp == 0) coef.toString
            else coef + "x^"+ exp
  ).mkString(" + ")
}
val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
p1 + p2






