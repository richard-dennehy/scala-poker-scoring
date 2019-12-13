package poker

case class Card(value: FaceValue, suit: Suit) extends Ordered[Card] {
  override def toString: String = s"$value of $suit"

  override def compare(that: Card): Int = this.value.compare(that.value)
}

object Card {
  implicit val ordering: Ordering[Card] = Ordering.by(_.value.ranking)
}
