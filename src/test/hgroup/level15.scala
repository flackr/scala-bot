package tests.hgroup.level15

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{hasInfs, hasStatus, Player, preClue, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class DoubleBluffs extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("gives and resolves a normal double bluff"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "g3", "p3", "r3"),
			Vector("y1", "b3", "g4", "p4"),
			Vector("b4", "r4", "y4", "g5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Donald"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
			hasStatus(g, Cathy, 1, CardStatus.Bluffed)
			hasInfs(g, None, Donald, 2, Vector("r2", "r3", "r4", "r5"))
		.pipe(takeTurn("Bob plays b1", "p2"))
		.tap: g =>
			hasStatus(g, Cathy, 1, CardStatus.Bluffed)
			hasInfs(g, None, Donald, 2, Vector("r3", "r4", "r5"))
		.pipe(takeTurn("Cathy plays y1", "b2"))

		hasInfs(game, None, Donald, 2, Vector("r4", "r5"))

	test("plays into a double bluff as first blind-player"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y1", "b3", "g4", "p4"),
			Vector("b4", "r4", "y4", "g5"),
			Vector("g3", "p3", "r3", "b5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues red to Cathy"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice plays b1 (slot 1)"))
		.tap: g =>
			hasStatus(g, Alice, 2, CardStatus.None)
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
			hasInfs(g, None, Cathy, 2, Vector("r3", "r4", "r5"))
		.pipe(takeTurn("Bob plays y1", "b2"))

		hasInfs(game, None, Cathy, 2, Vector("r4", "r5"))

	test("plays into a double bluff as second blind-player"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "r4", "y4", "g5"),
			Vector("g3", "p3", "r3", "b5"),
			Vector("b1", "b3", "g4", "p4")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues red to Bob"))
		.tap: g =>
			hasStatus(g, Donald, 1, CardStatus.Bluffed)
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Donald plays b1", "p2"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice plays y1 (slot 1)"))

		hasInfs(game, None, Bob, 2, Vector("r4", "r5"))

	test("receives a double bluff and narrows inferences step by step"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "r3", "b5"),
			Vector("b1", "b3", "g4", "p4"),
			Vector("y1", "b4", "y4", "g5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 2)"))
		.tap: g =>
			hasInfs(g, None, Alice, 2, Vector("r2", "r3", "r4", "r5"))
		.pipe(takeTurn("Cathy plays b1", "p2"))
		.tap: g =>
			hasInfs(g, None, Alice, 2, Vector("r3", "r4", "r5"))
		.pipe(takeTurn("Donald plays y1", "b2"))

		hasInfs(game, None, Alice, 2, Vector("r4", "r5"))

	test("falls back to single bluff when second player does not blind-play"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "r4", "b5"),
			Vector("b1", "b3", "g4", "p4"),
			Vector("y1", "b4", "y4", "g3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			clueTokens = 7,
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 2)"))
		.pipe(takeTurn("Cathy plays b1", "p2"))
		.tap: g =>
			hasInfs(g, None, Alice, 2, Vector("r3", "r4", "r5"))
		.pipe(takeTurn("Donald discards g3", "b2"))

		hasInfs(game, None, Alice, 2, Vector("r3"))
		hasStatus(game, Donald, 2, CardStatus.None)

	test("gives and resolves a half-finesse double bluff"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "g3", "p3", "r3"),
			Vector("y1", "b3", "g4", "p4"),
			Vector("b4", "r4", "y4", "g5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Donald"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
			hasStatus(g, Cathy, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Bob plays r2", "p2"))
		.tap: g =>
			hasStatus(g, Cathy, 1, CardStatus.Bluffed)
			hasInfs(g, None, Donald, 2, Vector("r3", "r4", "r5"))
		.pipe(takeTurn("Cathy plays y1", "b2"))

		hasInfs(game, None, Donald, 2, Vector("r4", "r5"))

	test("plays into a half-finesse double bluff as second blind-player"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b4", "r4", "y4", "g5"),
			Vector("g3", "p3", "b3", "b5"),
			Vector("r2", "b3", "g4", "p4")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues red to Bob"))
		.tap: g =>
			// Alice cannot tell whether her slot 1 is r3 (Double Finesse) or a Double Bluff card until she plays.
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
			hasInfs(g, None, Alice, 1, Vector("r3", "y1", "g1", "b1", "p1"))
		.pipe(takeTurn("Donald plays r2", "p2"))
		.tap: g =>
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice plays y1 (slot 1)"))

		hasInfs(game, None, Bob, 2, Vector("r4", "r5"))

	test("resolves double self-finesse when second player passes after connecting play"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y1", "b3", "g4", "p4"),
			Vector("b4", "r4", "y4", "g5"),
			Vector("g3", "p3", "b2", "b5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			clueTokens = 7,
			starting = Donald
		)
		.pipe(takeTurn("Donald clues red to Cathy"))
		.pipe(takeTurn("Alice plays r2 (slot 1)"))
		.pipe(takeTurn("Bob discards p4", "p2"))

		// Since Bob didn't play y1, Alice must have r3 on slot 2 as a Double Self-Finesse.
		hasStatus(game, Alice, 2, CardStatus.Finessed)
		hasInfs(game, None, Alice, 2, Vector("r3"))

	test("disallows same-colour double bluff when second blind-play connects"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "g3", "p3", "b3"),
			Vector("r2", "b3", "g4", "p4"),
			Vector("b4", "r4", "y4", "g5")
		))
		.pipe(takeTurn("Alice clues red to Donald"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("allows 3-player double bluff to Cathy when no single bluff target exists"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g3", "p3", "r3", "b3"),
			Vector("y1", "r4", "g4", "p4", "y3")
		))
		.pipe(takeTurn("Alice clues 4 to Cathy"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
			hasStatus(g, Cathy, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Bob plays b1", "p2"))
		.pipe(takeTurn("Cathy plays y1", "b2"))

		hasInfs(game, None, Cathy, 3, Vector("r4", "y4", "g4", "b4", "p4"))

	test("disallows 3-player double bluff to Cathy when a single bluff target exists"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g3", "p3", "r3", "b3"),
			Vector("y1", "g4", "b4", "p4", "y3")
		),
			playStacks = Some(Vector(2, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues 4 to Cathy"))

		// Cathy will assume a single bluff for r4 and stop after Bob plays.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("disallows 3-player colour double bluff"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g3", "p3", "r3", "b3"),
			Vector("y1", "r4", "g4", "p4", "y3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("uses single bluff rather than double bluff on a critical colour bluff target"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "g3", "p3", "r3"),
			Vector("y1", "b3", "g4", "p4"),
			Vector("b4", "r4", "y4", "g5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			discarded = Vector("r4")
		)
		.pipe(takeTurn("Alice clues red to Donald"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
			hasStatus(g, Cathy, 1, CardStatus.None)
		.pipe(takeTurn("Bob plays b1", "p2"))

		// Single critical colour bluff allows r3 (2-away) and critical r4/r5, without bluffing Cathy.
		hasInfs(game, None, Donald, 2, Vector("r3", "r4", "r5"))

class HardDoubleBluffs extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("gives and resolves a hard double bluff"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "g3", "p3", "b3"),
			Vector("r2", "b3", "g4", "p4"),
			Vector("b5", "r4", "y3", "g5")
		))
		.pipe(takeTurn("Alice clues 4 to Donald"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
			hasStatus(g, Cathy, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Bob plays r1", "p2"))
		.tap: g =>
			hasStatus(g, Cathy, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Cathy plays r2", "b2"))

		hasInfs(game, None, Donald, 2, Vector("r4", "y4", "g4", "b4", "p4"))

	test("plays into a hard double bluff as first blind-player"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "b3", "g4", "p4"),
			Vector("b5", "r4", "y3", "g5"),
			Vector("g3", "p3", "b3", "y5")
		),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues 4 to Cathy"))
		.tap: g =>
			// Only r1 makes Bob's r2 playable for the Hard Double Bluff.
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
			hasInfs(g, None, Alice, 1, Vector("r1"))
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice plays r1 (slot 1)"))
		.pipe(takeTurn("Bob plays r2", "b2"))

		hasInfs(game, None, Cathy, 2, Vector("r4", "y4", "g4", "b4", "p4"))

	test("plays into a hard double bluff as second blind-player"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b5", "r4", "y3", "g5"),
			Vector("g3", "p3", "b3", "y5"),
			Vector("r1", "b3", "g4", "p4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 4 to Bob"))
		.tap: g =>
			hasStatus(g, Donald, 1, CardStatus.Bluffed)
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
			hasInfs(g, None, Alice, 1, Vector("r2", "y1", "g1", "b1", "p1"))
		.pipe(takeTurn("Donald plays r1", "p2"))
		.tap: g =>
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice plays r2 (slot 1)"))

		hasInfs(game, None, Bob, 2, Vector("r4", "y4", "g4", "b4", "p4"))

	test("receives a hard double bluff without promising the connecting 3"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "b3", "y5"),
			Vector("r1", "b3", "g4", "p4"),
			Vector("r2", "b5", "y3", "g5")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 4 to Alice (slot 2)"))
		.pipe(takeTurn("Cathy plays r1", "p2"))
		.pipe(takeTurn("Donald plays r2", "b2"))

		hasInfs(game, None, Alice, 2, Vector("r4", "y4", "g4", "b4", "p4"))
		hasStatus(game, Alice, 1, CardStatus.None)

	test("hard double bluff preempts triple finesse on rank 4 clue"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r1", "g3", "p3", "b3"),
			Vector("r2", "b3", "g4", "p4"),
			Vector("r3", "y3", "g3", "p3"),
			Vector("b5", "r4", "y3", "g5")
		))
		.pipe(takeTurn("Alice clues 4 to Emily"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
			hasStatus(g, Cathy, 1, CardStatus.Bluffed)
			hasStatus(g, Donald, 1, CardStatus.None)
		.pipe(takeTurn("Bob plays r1", "p2"))
		.pipe(takeTurn("Cathy plays r2", "b2"))

		hasStatus(game, Donald, 1, CardStatus.None)
		hasInfs(game, None, Emily, 2, Vector("r4", "y4", "g4", "b4", "p4"))

class PestilentDoubleBluffs extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("gives and resolves pestilent double bluff example 1"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y1", "g3", "p3", "b3"),
			Vector("b1", "b3", "g4", "p4"),
			Vector("b5", "r2", "y3", "g5")
		))
		.pipe(takeTurn("Alice clues 2 to Donald"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
			hasStatus(g, Cathy, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Bob plays y1", "p2"))
		.tap: g =>
			hasStatus(g, Cathy, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Cathy plays b1", "g2"))

		hasInfs(game, None, Donald, 2, Vector("r2", "g2", "p2"))

	test("plays into pestilent double bluff as second blind-player to heal desynchronization"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b5", "r2", "y3", "g5"),
			Vector("g3", "p3", "b3", "y4"),
			Vector("y1", "b3", "g4", "p4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 2 to Bob"))
		.pipe(takeTurn("Donald plays y1", "p2"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice plays b1 (slot 1)"))

		hasInfs(game, None, Bob, 2, Vector("r2", "g2", "p2"))

	test("receives pestilent double bluff example 1"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "b3", "y4"),
			Vector("g1", "b3", "g4", "p4"),
			Vector("b1", "b5", "y3", "g5")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 2 to Alice (slot 2)"))
		.pipe(takeTurn("Cathy plays g1", "p2"))
		.tap: g =>
			// Cathy playing rules out b2 (Cathy wouldn't have played before Donald's b1).
			hasInfs(g, None, Alice, 2, Vector("r2", "y2", "g2", "p2"))
		.pipe(takeTurn("Donald plays b1", "r3"))

		// Donald also blind-playing rules out g2 as well.
		hasInfs(game, None, Alice, 2, Vector("r2", "y2", "p2"))

	test("gives and resolves pestilent double bluff example 2"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r2", "g4", "p4", "b4"),
			Vector("p1", "y4", "g4", "r4"),
			Vector("y5", "g5", "p5", "b3")
		),
			playStacks = Some(Vector(1, 0, 0, 1, 0))
		)
		.pipe(takeTurn("Alice clues 3 to Donald"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
			hasStatus(g, Cathy, 1, CardStatus.Bluffed)
		.pipe(takeTurn("Bob plays r2", "p2"))
		.pipe(takeTurn("Cathy plays p1", "g2"))

		// r3 is ruled out because Cathy played; p3 is ruled out because Bob didn't wait for Cathy's p1.
		hasInfs(game, None, Donald, 4, Vector("y3", "g3", "b3"))

	test("receives pestilent double bluff example 2"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y5", "g5", "p5", "b4"),
			Vector("r2", "g4", "p4", "b4"),
			Vector("p1", "y4", "g4", "r4")
		),
			playStacks = Some(Vector(1, 0, 0, 1, 0)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 4)"))
		.pipe(takeTurn("Cathy plays r2", "p2"))
		.pipe(takeTurn("Donald plays p1", "g2"))

		hasInfs(game, None, Alice, 4, Vector("y3", "g3", "b3"))

	test("resolves pestilent double bluff example 3 when giver holds both connecting copies unclued"):
		// Cathy is giver holding both r3s unclued; Donald is 1st blind-player with [3, 3, r2, x]; Alice is 2nd blind-player.
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y4", "g3", "b4", "p4"),
			Vector("r3", "b4", "r3", "b2"),
			Vector("p3", "p3", "r2", "g4")
		),
			playStacks = Some(Vector(1, 1, 0, 0, 1)),
			init = preClue[HGroup](Donald, 1, Seq("3")) andThen preClue[HGroup](Donald, 2, Seq("3")),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 3 to Bob"))
		.pipe(takeTurn("Donald plays r2", "y2"))
		.tap: g =>
			// Even though Alice sees both r3s in Cathy's hand, Cathy does not know she holds them, so Alice must still blind-play.
			hasStatus(g, Alice, 1, CardStatus.Bluffed)
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Play(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice plays p2 (slot 1)"))

		hasInfs(game, None, Bob, 2, Vector("y3", "g3", "b3"))

	test("receives pestilent double bluff example 3 as target"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r3", "b4", "r3", "b2"),
			Vector("p3", "p3", "r2", "g4"),
			Vector("p2", "y4", "g4", "r4")
		),
			playStacks = Some(Vector(1, 1, 0, 0, 1)),
			init = preClue[HGroup](Cathy, 1, Seq("3")) andThen preClue[HGroup](Cathy, 2, Seq("3")),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues 3 to Alice (slot 2)"))
		.pipe(takeTurn("Cathy plays r2", "y2"))
		.pipe(takeTurn("Donald plays p2", "g2"))

		hasInfs(game, None, Alice, 2, Vector("y3", "g3", "b3"))

	test("disallows pestilent double bluff to second player in 3-player game"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y1", "g3", "p3", "b3", "r4"),
			Vector("b1", "r2", "g4", "p4", "y3")
		))
		.pipe(takeTurn("Alice clues 2 to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("uses single 3-bluff rather than pestilent double bluff when first blind-play already disconnects"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g1", "g4", "p4", "b4"),
			Vector("b1", "y4", "g4", "r4"),
			Vector("y4", "r3", "p4", "b5")
		))
		.pipe(takeTurn("Alice clues 3 to Donald"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
			hasStatus(g, Cathy, 1, CardStatus.None)
		.pipe(takeTurn("Bob plays g1", "p2"))

		hasStatus(game, Cathy, 1, CardStatus.None)
		hasInfs(game, None, Donald, 2, Vector("r3", "y3", "g3", "p3"))

class PrinciplesAndRestrictions extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("resolves after two blind-plays and never performs a triple bluff"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "g3", "p3", "r3"),
			Vector("y1", "b3", "g4", "p4"),
			Vector("g1", "y3", "g3", "p3"),
			Vector("b4", "r5", "y4", "g5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Emily"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Bob, 1, CardStatus.Bluffed)
			hasStatus(g, Cathy, 1, CardStatus.Bluffed)
			hasStatus(g, Donald, 1, CardStatus.None)
		.pipe(takeTurn("Bob plays b1", "p2"))
		.pipe(takeTurn("Cathy plays y1", "b2"))

		// Donald (3rd player) must not blind-play.
		hasStatus(game, Donald, 1, CardStatus.None)
		hasInfs(game, None, Emily, 2, Vector("r4", "r5"))

	test("disallows double bluff when second player has an unplayable card even if third player is playable"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "g3", "p3", "r3"),
			Vector("y4", "b3", "g4", "p4"),
			Vector("g1", "y3", "g3", "p3"),
			Vector("b4", "r5", "y4", "g5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Emily"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("disallows double bluffs in 2-player games"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "r4", "g4", "p4", "y3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Bob"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("first blind-player never defers a potential double bluff"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("y1", "g1", "g4", "p4"),
			Vector("g2", "r4", "y4", "g5"),
			Vector("g3", "p3", "r3", "b5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues red to Cathy"))

		// Even though Alice sees a finesse available (Bob's g1 -> Cathy's g2 -> Donald's g3), she must not defer a Double Bluff.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(0)))

	test("second blind-player never defers a potential double bluff"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g1", "r4", "y4", "g5"),
			Vector("g2", "p3", "r3", "b5"),
			Vector("b1", "b3", "g4", "p4")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues red to Bob"))
		.pipe(takeTurn("Donald plays b1", "p2"))

		// Even though Alice sees a finesse available (Bob's g1 -> Cathy's g2), she must not defer the 2nd blind-play.
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Play(game.state.hands(Alice.ordinal)(0)))

	test("bobs truth principle overrides occams razor (assumes reverse finesse over double bluff)"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r1", "g3", "p3", "b3", "y4"),
			Vector("g4", "b4", "p4", "y3", "r5")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues 4 to Alice (slot 1)"))
		.tap: g =>
			// Alice (in Bob's seat) sees r1 on Bob's Finesse Position, so she assumes Reverse Finesse (r1 -> r2 -> r3 -> r4)
			// rather than Double Bluff, and waits for Bob instead of blind-playing slot 2 immediately.
			hasStatus(g, Bob, 1, CardStatus.Finessed)
			hasStatus(g, Alice, 2, CardStatus.Finessed)
			hasStatus(g, Alice, 3, CardStatus.Finessed)
			assertNotEquals(g.takeAction.unsafeRunSync(), PerformAction.Play(g.state.hands(Alice.ordinal)(1)))
		.pipe(takeTurn("Alice clues 5 to Cathy"))
		.pipe(takeTurn("Bob plays r1", "p2"))

		hasInfs(game, None, Alice, 1, Vector("r4"))

	test("giver does not give a double bluff that violates bobs truth principle"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "b1", "p3", "b3", "y4"),
			Vector("r1", "b4", "p4", "y3", "r5")
		))
		.pipe(takeTurn("Alice clues 4 to Bob"))

		// Bob will see Cathy's r1 and assume a Reverse Finesse on r4 instead of blind-playing b1.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("disallows double bluff when second blind-player is already finessed"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "g2", "p3", "r3"),
			Vector("g1", "y1", "g4", "p4"),
			Vector("b4", "r4", "y4", "g5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues green to Bob"))
		.tap: g =>
			hasStatus(g, Cathy, 1, CardStatus.Finessed)
		.pipe(takeTurn("Alice clues red to Donald"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("disallows double bluff when first blind-player is already finessed"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "g3", "p3", "r3"),
			Vector("g1", "b2", "g4", "p4"),
			Vector("b4", "r4", "y4", "g5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			starting = Donald
		)
		.pipe(takeTurn("Donald clues blue to Cathy"))
		.tap: g =>
			hasStatus(g, Bob, 1, CardStatus.Finessed)
		.pipe(takeTurn("Alice clues red to Donald"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("disallows double bluff when first player has a prompt instead of blind-playing"):
		val game = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "r2", "p3", "g4"),
			Vector("y1", "b3", "g4", "p4"),
			Vector("b4", "r4", "y4", "g5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			init = preClue[HGroup](Bob, 2, Seq("red"))
		)
		.pipe(takeTurn("Alice clues red to Donald"))

		// Bob will prompt his clued r2 rather than blind-playing b1, leaving r3 missing.
		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("does not allow double bluffs below level 15"):
		val game = setup(HGroup.atLevel(14), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "g3", "p3", "r3"),
			Vector("g1", "b3", "g4", "p4"),
			Vector("b4", "r4", "y4", "g5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Donald"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))
