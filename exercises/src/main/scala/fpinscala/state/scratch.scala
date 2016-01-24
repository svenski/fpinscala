
object bla {
  val rr = RNG.Simple(3)

  val numberOfRolls = 3000
  val expected = numberOfRolls.toDouble / 6
  val outcomes = RNG.sequence(List.fill(numberOfRolls)(RNG.rollDie))(rr)._1 groupBy(identity) mapValues( _.size.toDouble)
  val chiSquared = outcomes.mapValues( actual => (math.pow(expected - actual,2))/expected).values.sum

}
