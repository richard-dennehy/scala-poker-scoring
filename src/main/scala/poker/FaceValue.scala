package poker

sealed trait FaceValue extends Ordered[FaceValue] {
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
  override def compare(that: FaceValue): Int = this.ranking - that.ranking
}

case object Ace extends FaceValue
case object Two extends FaceValue
case object Three extends FaceValue
case object Four extends FaceValue
case object Five extends FaceValue
case object Six extends FaceValue
case object Seven extends FaceValue
case object Eight extends FaceValue
case object Nine extends FaceValue
case object Ten extends FaceValue
case object Jack extends FaceValue
case object Queen extends FaceValue
case object King extends FaceValue
