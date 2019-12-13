package poker

sealed trait Value extends Ordered[Value] {
  def of(suit: Suit): Card = Card(this, suit)

  def ranking: Int = this match {
    case Ace => 14
    case Two => 2
    case Three => 3
    case Four => 4
    case Five => 5
    case Six => 6
    case Seven => 7
    case Eight => 8
    case Nine => 9
    case Ten => 10
    case Jack => 11
    case Queen => 12
    case King => 13
  }
  override def compare(that: Value): Int = this.ranking - that.ranking
}

case object Ace extends Value
case object Two extends Value
case object Three extends Value
case object Four extends Value
case object Five extends Value
case object Six extends Value
case object Seven extends Value
case object Eight extends Value
case object Nine extends Value
case object Ten extends Value
case object Jack extends Value
case object Queen extends Value
case object King extends Value
