object Suit extends Enumeration {
  type Suit = Value
  val D, C, S, H = Value
}
import Suit._

object Rank extends Enumeration {
  type Rank = Value
  val `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, J, Q, K, A = Value
}
import Rank._

object Winner extends Enumeration {
  type Winner = Value
  val PLAYER, HOUSE = Value
}
import Winner._

case class Card (suit: Suit, rank: Rank) extends Ordered[Card] {
  def compare(that: Card): Int = (this.suit, this.rank) compare (that.suit, that.rank)
}

case class Deck(cards: Seq[Card]) {
  def shuffled = Deck(cards.reverse)
}

object Deck {
  def apply() = new Deck(defaultCards)
  def defaultCards = for {
    suit <- Suit.values.toList
    value <- Rank.values.toList
  } yield Card(suit, value)
}

/////

case class Hand(cards: Seq[Card] = Seq.empty) {
  def total: Int = cards.map(cardValue).sum

  def bust: Boolean = total > 21

  def blackjack: Boolean = total == 21

  private def cardValue(c: Card) = c.rank match {
    case r if r <= `10` => r.id + 2
    case J | Q | K => 10
    case A => 11
  }
}

case class Blackjack(deck: Deck, player: Hand = Hand(), dealer: Hand = Hand()) {
  def draw: Blackjack =
    if (winner.isEmpty && !playerStopped) drawCards(p = 1)
    else if (winner.isEmpty && !dealerStopped) drawCards(d = 1)
    else this

  def winner: Option[Winner] = {
    if (player.blackjack || dealer.bust)
      Some(PLAYER)
    else if (dealer.blackjack || player.bust || playerStopped && dealerStopped)
      Some(HOUSE)
    else
      None
  }

  private def playerStopped = player.total >= 17

  private def dealerStopped = dealer.total > player.total

  private def drawCards(p: Int = 0, d: Int = 0): Blackjack = Blackjack(
    Deck(deck.cards.drop(p + d)),
    Hand(player.cards ++ deck.cards.take(p)),
    Hand(dealer.cards ++ deck.cards.slice(p, p + d)))
}

object Blackjack {
  def apply(deck: Deck) = new Blackjack(deck).drawCards(2, 2)
}
