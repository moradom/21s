import Suit._
import Rank._
import Winner._

import scala.util.Random

class BlackjackSpec extends BaseSpec {

  "Deck" should {
    "contain 52 cards" in new Context {
      originalDeck.cards.length shouldBe 52
    }

    "have 13 cards of each suit" in new Context {
      val result = originalDeck.cards.groupBy(_.suit)
      result.mapValues(_.length) shouldBe Map(D -> 13, C -> 13, S -> 13, H -> 13)
    }

    "have 4 cards of each rank per ordered suit" in new Context {
      val result = originalDeck.cards.groupBy(_.rank)
      result.values.map(_.map(_.suit)) should contain only Seq(D, C, S, H)
    }

    "have cards ordered by rank within a suit" in new Context {
      val result = originalDeck.cards.groupBy(_.suit)
      result.values.map(_.map(_.rank)) should contain only Seq(`2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, J, Q, K, A)
    }

    "be able to shuffle itself" in new Context {
      pending // TODO: how to properly test randomness?
      val result = originalDeck.shuffled
      result.cards should not be sorted
      result.cards.sorted shouldBe originalDeck.cards
    }
  }

  "Blackjack" when {
    "playing the game" should {
      "draw 2 cards for player and dealer" in new Context {
        val game = Blackjack(randomDeck)

        game.player.cards shouldBe Seq(randomDeck.cards(0), randomDeck.cards(1))
        game.dealer.cards shouldBe Seq(randomDeck.cards(2), randomDeck.cards(3))
        game.deck.cards.length shouldBe 52 - 4
      }

      "draw player cards until at least 17" in new Context {
        val deck = makeDeck(`10`, `2`, K, K, `2`, `3`, Q)
        val game = Blackjack(deck)

        val result = draw(game, 3)

        result.player.cards shouldBe cards(deck, 0, 1, 4, 5)
      }

      "draw dealer cards until the score is higher than player" in new Context {
        val deck = makeDeck(`10`, `7`, K, `5`, `2`, `3`, Q)
        val game = Blackjack(deck)

        val result = draw(game, 3)

        result.dealer.cards shouldBe cards(deck, 2, 3, 4, 5)
      }
    }

    "finding a winner" should {
      "immediately win the player if only they start with 21" in new Context {
        val deck = makeDeck(`10`, A, K, K, `2`, `3`, Q)
        val game = Blackjack(deck)

        val result = draw(game, 3)

        result.player.cards shouldBe cards(deck, 0, 1)
        result.winner shouldBe Some(PLAYER)
      }

      "immediately win the dealer if only they start with 21" in new Context {
        val deck = makeDeck(`10`, K, A, K, `2`, `3`, Q)
        val game = Blackjack(deck)

        val result = draw(game, 3)

        result.dealer.cards shouldBe cards(deck, 2, 3)
        result.winner shouldBe Some(HOUSE)
      }

      "immediately win the player if both with 21" in new Context {
        val deck = makeDeck(`10`, A, A, K, `2`, `3`, Q)
        val game = Blackjack(deck)

        val result = draw(game, 3)

        result.player.cards shouldBe cards(deck, 0, 1)
        result.dealer.cards shouldBe cards(deck, 2, 3)
        result.winner shouldBe Some(PLAYER)
      }

      "give victory to the higher score without going bust" in new Context {
        val deck = makeDeck(`10`, `5`, `5`, K, `3`, `4`, Q)
        val game = Blackjack(deck)

        val result = draw(game, 3)

        result.player.cards shouldBe cards(deck, 0, 1, 4)
        result.dealer.cards shouldBe cards(deck, 2, 3, 5)
        result.winner shouldBe Some(HOUSE)
      }

      "give victory to player if dealer goes bust" in new Context {
        val deck = makeDeck(`10`, `5`, `5`, K, `3`, `7`, Q)
        val game = Blackjack(deck)

        val result = draw(game, 3)

        result.player.cards shouldBe cards(deck, 0, 1, 4)
        result.dealer.cards shouldBe cards(deck, 2, 3, 5)
        result.winner shouldBe Some(PLAYER)
      }

      "give victory to dealer if player goes bust" in new Context {
        val deck = makeDeck(`10`, `5`, `5`, K, `7`, `4`, Q)
        val game = Blackjack(deck)

        val result = draw(game, 3)

        result.player.cards shouldBe cards(deck, 0, 1, 4)
        result.dealer.cards shouldBe cards(deck, 2, 3)
        result.winner shouldBe Some(HOUSE)
      }
    }
  }

  "Hand" when {
    "calculating its total value" should {
      "return 0 for no cards" in new Context {
        Hand().total shouldBe 0
      }

      "consider number ranks as their value" in new Context {
        val numberRanks = Rank.values.toSeq.sorted.take(9)
        val listValues = numberRanks.map(_.id + 2)
        val listHands = makeDeck(numberRanks: _*).cards.map(c => Hand(Seq(c)))

        listHands.map(_.total) shouldBe listValues
      }

      "count figure cards as 10" in new Context {
        val figureRanks = Seq(J, Q, K)
        val listValues = figureRanks.map(_ => 10)
        val listHands = makeDeck(figureRanks: _*).cards.map(c => Hand(Seq(c)))

        listHands.map(_.total) shouldBe listValues
      }

      "count the ace as 11" in new Context {
        Hand(makeDeck(A).cards).total shouldBe 11
      }

      "sum the values of all the cards" in new Context {
        Hand(randomDeck.cards).total shouldBe (54 + 41) * 4
      }
    }
  }

  trait Context {
    val originalDeck = Deck()
    //val randomDeck = originalDeck.shuffled
    val randomDeck = Deck(Random.shuffle(originalDeck.cards))

    def makeDeck(ranks: Rank*) = {
      def suit = Random.shuffle(Suit.values.toSeq).head
      Deck(ranks.map(r => Card(suit, r)))
    }

    def cards(deck: Deck, idxs: Int*) =
      idxs.map(i => deck.cards(i))

    def draw(g: Blackjack, n: Int): Blackjack =
      (1 to n).foldLeft(g)((a: Blackjack, _) => a.draw)
  }
}

