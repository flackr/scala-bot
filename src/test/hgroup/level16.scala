package tests.hgroup.level16

import cats.effect.unsafe.implicits.global

import scala_bot.basics._
import scala_bot.test.{fullyKnown, hasInfs, hasStatus, Player, preClue, setup, takeTurn}, Player._
import scala_bot.hgroup.HGroup

import scala_bot.utils.{pipe, tap}
import scala_bot.logger.{Logger, LogLevel}

class FiveColorEjection extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("gives and resolves a 5 color ejection (guide example)"):
		val game = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "b1", "y4", "p3", "b4"),
			Vector("r5", "g4", "y3", "p4", "b3")
		))
		.pipe(takeTurn("Alice clues red to Cathy"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Bob, 1, CardStatus.None)
			hasStatus(g, Bob, 2, CardStatus.Bluffed)
			hasInfs(g, None, Cathy, 1, Vector("r1", "r5"))
		.pipe(takeTurn("Bob plays b1", "p2"))

		hasInfs(game, None, Cathy, 1, Vector("r5"))

	test("blind-plays 2FP into a 5 color ejection"):
		val game = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r5", "g4", "y3", "p4", "b3"),
			Vector("g3", "y4", "p3", "b4", "g4")
		),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues red to Bob"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Alice, 1, CardStatus.None)
			hasStatus(g, Alice, 2, CardStatus.Bluffed)
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Play(g.state.hands(Alice.ordinal)(1)))
		.pipe(takeTurn("Alice plays b1 (slot 2)"))

		hasInfs(game, None, Bob, 1, Vector("r5"))

	test("receives a 5 color ejection and disambiguates 2FP play, 1FP play, and discard"):
		val afterClue = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "y4", "p3", "b4", "g4"),
			Vector("y1", "b1", "p4", "b3", "y3")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 1)"))
		.tap: g =>
			hasInfs(g, None, Alice, 1, Vector("r1", "r2", "r3", "r5"))

		// Case 1: Cathy blind-plays 2FP -> 5 Color Ejection (r5)
		val after2fp = afterClue.pipe(takeTurn("Cathy plays b1", "g2"))
		hasInfs(after2fp, None, Alice, 1, Vector("r5"))

		// Case 2: Cathy blind-plays 1FP -> normal 1FP Bluff (r2) or Self Double Bluff (r3)
		val after1fp = afterClue.pipe(takeTurn("Cathy plays y1", "g2"))
		hasInfs(after1fp, None, Alice, 1, Vector("r2", "r3"))

		// Case 3: Cathy discards -> direct play (r1)
		val afterDc = afterClue.pipe(takeTurn("Cathy discards y3", "g2"))
		hasInfs(afterDc, None, Alice, 1, Vector("r1"))

	test("takes precedence over a 2-blind-play normal finesse on Bob"):
		// Even when Bob holds r3 in slot 1, cluing red to Cathy's r5 (with r2 on stack) must be 5CE on Bob's 2FP.
		val game = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "b1", "r4", "p3", "b4"),
			Vector("r5", "g4", "y3", "p4", "b3")
		),
			playStacks = Some(Vector(2, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Bob, 1, CardStatus.None)
			hasStatus(g, Bob, 2, CardStatus.Bluffed)
		.pipe(takeTurn("Bob plays b1", "p2"))

		hasInfs(game, None, Cathy, 1, Vector("r5"))

	test("marks mistake when Bob holds r3, r4 in slots 1, 2 (2FP is unplayable)"):
		val game = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "r4", "y4", "p3", "b4"),
			Vector("r5", "g4", "y3", "p4", "b3")
		),
			playStacks = Some(Vector(2, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("takes precedence over level 15 color double bluff on unclued 5"):
		val game = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("b1", "g3", "p3", "r3"),
			Vector("y1", "b3", "g4", "p4"),
			Vector("b4", "r4", "y4", "g5")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Donald"))
		.pipe(takeTurn("Bob plays b1", "p2"))
		.pipe(takeTurn("Cathy plays y1", "b2"))

		// At Level 16, r5 would have used 5CE on Bob's 2FP rather than Double Bluff across Bob + Cathy.
		hasInfs(game, None, Donald, 2, Vector("r4"))

	test("does not trigger when Bob only needs 1 blind-play"):
		// Subcase A: r3 is already on stack, so Bob only needs r4 (1 blind-play).
		val oneAway = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "b1", "y4", "p3", "b4"),
			Vector("r5", "g4", "y3", "p4", "b3")
		),
			playStacks = Some(Vector(3, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		assertEquals(oneAway.lastMove, Some(ClueInterp.Play))
		hasStatus(oneAway, Bob, 1, CardStatus.Finessed)
		hasStatus(oneAway, Bob, 2, CardStatus.None)

		// Subcase B: 4-player split finesse where Bob holds r3 (1 blind-play) and Cathy holds r4 (1 blind-play).
		val splitFinesse = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("r3", "b1", "y4", "p3"),
			Vector("r4", "g4", "y3", "p4"),
			Vector("r5", "b3", "g3", "b4")
		),
			playStacks = Some(Vector(2, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Donald"))

		assertEquals(splitFinesse.lastMove, Some(ClueInterp.Play))
		hasStatus(splitFinesse, Bob, 1, CardStatus.Finessed)
		hasStatus(splitFinesse, Bob, 2, CardStatus.None)
		hasStatus(splitFinesse, Cathy, 1, CardStatus.Finessed)

	test("prompts in Bob's hand do not count toward the 2 blind-plays"):
		// 1 prompt (r3) + 1 blind-play (r4) on Bob -> NOT 5CE.
		val promptPlusOne = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r4", "b1", "r3", "p3", "b4"),
			Vector("r5", "g4", "y3", "p4", "b3")
		),
			playStacks = Some(Vector(2, 0, 0, 0, 0)),
			init = preClue(Bob, 3, Seq("red"))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		assertEquals(promptPlusOne.lastMove, Some(ClueInterp.Play))
		hasStatus(promptPlusOne, Bob, 1, CardStatus.Finessed)
		hasStatus(promptPlusOne, Bob, 2, CardStatus.None)

		// 1 prompt (r2) + 2 blind-plays (r3, r4) needed from Bob -> DOES trigger 5CE on 2FP!
		val promptPlusTwo = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "b1", "r2", "p3", "b4"),
			Vector("r5", "g4", "y3", "p4", "b3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			init = preClue(Bob, 3, Seq("red"))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		assertEquals(promptPlusTwo.lastMove, Some(ClueInterp.Play))
		hasStatus(promptPlusTwo, Bob, 1, CardStatus.None)
		hasStatus(promptPlusTwo, Bob, 2, CardStatus.Bluffed)

	test("re-clued 5 signals full finesse instead of 5CE, while chop-moved unclued 5 still triggers 5CE"):
		val reclued = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r3", "r4", "y4", "p3", "b4"),
			Vector("r5", "g4", "y3", "p4", "b3")
		),
			playStacks = Some(Vector(2, 0, 0, 0, 0)),
			init = preClue(Cathy, 1, Seq("5"))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		assertEquals(reclued.lastMove, Some(ClueInterp.Play))
		hasStatus(reclued, Bob, 1, CardStatus.Finessed)
		hasStatus(reclued, Bob, 2, CardStatus.Finessed)

		val chopMoved5 = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "b1", "y4", "p3", "b4"),
			Vector("g4", "y3", "p4", "b3", "r5")
		),
			init = g => g.withMeta(g.state.hands(Cathy.ordinal)(4))(_.copy(status = CardStatus.ChopMoved))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		assertEquals(chopMoved5.lastMove, Some(ClueInterp.Play))
		hasStatus(chopMoved5, Bob, 2, CardStatus.Bluffed)

	test("skips clued and chop-moved cards in Bob's hand when locating 2FP"):
		val game = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y3", "p3", "g3", "b1", "b4"),
			Vector("r5", "g4", "y4", "p4", "b3")
		),
			init = g =>
				val g1 = preClue(Bob, 1, Seq("yellow"))(g)
				g1.withMeta(g1.state.hands(Bob.ordinal)(1))(_.copy(status = CardStatus.ChopMoved))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Play))
		hasStatus(game, Bob, 3, CardStatus.None)
		hasStatus(game, Bob, 4, CardStatus.Bluffed)


class UnknownTrashDischarge extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("disallows 1FP trash bluff when empathy has unplayable useful cards (guide illegal-trash-bluff)"):
		val game = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g3", "p4", "y4", "b4"),
			Vector("g4", "r1", "y3", "p3", "b3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("gives and resolves 1-for-1 UTD without chop-moving right of focus (guide unknown-trash-discharge)"):
		val game = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "b1", "y4", "b4"),
			Vector("g4", "r1", "y3", "p4", "b3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Bob, 1, CardStatus.None)
			hasStatus(g, Bob, 3, CardStatus.Bluffed)
			assert(g.meta(g.state.hands(Cathy.ordinal)(1)).trash)
			hasStatus(g, Cathy, 3, CardStatus.None)
			hasStatus(g, Cathy, 4, CardStatus.None)
			hasStatus(g, Cathy, 5, CardStatus.None)
		.pipe(takeTurn("Bob plays b1", "p2"))

		hasInfs(game, None, Cathy, 2, Vector("r1"))
		assert(game.meta(game.state.hands(Cathy.ordinal)(1)).trash)

	test("blind-plays 3FP into an unknown trash discharge"):
		val game = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "r1", "y3", "p4", "b3"),
			Vector("g3", "p3", "y4", "b4", "g4")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			starting = Cathy
		)
		.pipe(takeTurn("Cathy clues red to Bob"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Alice, 3, CardStatus.Bluffed)
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Play(g.state.hands(Alice.ordinal)(2)))
		.pipe(takeTurn("Alice plays b1 (slot 3)"))

		hasInfs(game, None, Bob, 2, Vector("r1"))
		assert(game.meta(game.state.hands(Bob.ordinal)(1)).trash)

	test("receives 1-for-1 UTD, marks focus trash without chop-moving, and discards focus"):
		val game = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "y4", "b4", "g4"),
			Vector("g2", "p2", "b1", "y3", "b3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slot 2)"))
		.pipe(takeTurn("Cathy plays b1", "p4"))

		hasInfs(game, None, Alice, 2, Vector("r1"))
		assert(game.meta(game.state.hands(Alice.ordinal)(1)).trash)
		hasStatus(game, Alice, 3, CardStatus.None)
		hasStatus(game, Alice, 4, CardStatus.None)
		hasStatus(game, Alice, 5, CardStatus.None)
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(1)))

	test("uses 1FP trash bluff instead of 3FP UTD when all useful possibilities are playable"):
		// r3 on stack with r5 known in Bob's hand: only useful red card is r4 (2 copies left), which is immediately playable -> Level 14 1FP Trash Bluff applies.
		val r3Stack = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("b1", "g3", "p4", "y4", "r5"),
			Vector("g4", "r2", "y3", "p3", "b3")
		),
			playStacks = Some(Vector(3, 0, 0, 0, 0)),
			init = fullyKnown[HGroup](Bob, 5, "r5")
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		assertEquals(r3Stack.lastMove, Some(ClueInterp.Play))
		hasStatus(r3Stack, Bob, 1, CardStatus.Bluffed)
		hasStatus(r3Stack, Bob, 3, CardStatus.None)
		hasStatus(r3Stack, Cathy, 3, CardStatus.ChopMoved)
		hasStatus(r3Stack, Cathy, 4, CardStatus.ChopMoved)
		hasStatus(r3Stack, Cathy, 5, CardStatus.ChopMoved)

		// Single-card rank 1 clue: all useful 1s are playable -> 1FP Trash Bluff, so unplayable 1FP is a Mistake even if 3FP is playable!
		val bad1TrashBluff = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "b1", "y4", "b4"),
			Vector("g4", "r1", "y3", "p4", "b3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues 1 to Cathy"))

		assertEquals(bad1TrashBluff.lastMove, Some(ClueInterp.Mistake))

	test("handles 2-for-1 color UTD touching trash focus + useful non-focus (guide unknown-trash-discharge-two-for-one)"):
		// Observing as giver: symmetrically could be 2-for-1 UTD (r1) or 2-card UDD (r2/r3/r4), so focus is CalledToDiscard
		val obs = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "b1", "y4", "b4"),
			Vector("r1", "r5", "y3", "p4", "b3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			assertEquals(g.waiting.length, 1)
			hasStatus(g, Bob, 3, CardStatus.Bluffed)
			hasStatus(g, Cathy, 1, CardStatus.CalledToDiscard)
			assert(g.common.orderTrash(g, g.state.hands(Cathy.ordinal)(0)))
			assert(!g.common.orderTrash(g, g.state.hands(Cathy.ordinal)(1)))
			hasInfs(g, None, Cathy, 2, Vector("r2", "r3", "r4", "r5"))
			hasStatus(g, Cathy, 3, CardStatus.None)
			hasStatus(g, Cathy, 4, CardStatus.None)
			hasStatus(g, Cathy, 5, CardStatus.None)
		.pipe(takeTurn("Bob plays b1", "p2"))
		.pipe(takeTurn("Cathy discards r1", "g2"))

		// Slot 2 (formerly slot 2, after drawing into slot 1) remains a good red card with no Sarcastic link to r1
		assert(!obs.common.orderTrash(obs, obs.state.hands(Cathy.ordinal)(1)))
		hasInfs(obs, None, Cathy, 2, Vector("r2", "r3", "r4", "r5"))

		// Receiving as Alice
		val recv = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "y4", "b4", "g4"),
			Vector("g2", "p2", "b1", "y3", "b3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slots 1,2)"))
		.pipe(takeTurn("Cathy plays b1", "p4"))
		.tap: g =>
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Discard(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice discards r1 (slot 1)"))

		assert(!recv.common.orderTrash(recv, recv.state.hands(Alice.ordinal)(1)))
		hasInfs(recv, None, Alice, 2, Vector("r2", "r3", "r4", "r5"))
		hasStatus(recv, Alice, 3, CardStatus.None)
		hasStatus(recv, Alice, 4, CardStatus.None)
		hasStatus(recv, Alice, 5, CardStatus.None)

	test("handles 2-for-1 rank 1 UTD overriding level 14 trash bluff (guide unknown-trash-discharge-two-for-one-2)"):
		// Observing as giver: g1 on stack, Cathy has [r1, g1, ...] in starting hand (slot 2 g1 is rightmost starting-hand 1 = focus!)
		val obs = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("y1", "p3", "b1", "y4", "b4"),
			Vector("r1", "g1", "y3", "p4", "b3")
		),
			playStacks = Some(Vector(0, 0, 1, 0, 0)),
			clueTokens = 7
		)
		.pipe(takeTurn("Alice clues 1 to Cathy"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			hasStatus(g, Bob, 1, CardStatus.None)
			hasStatus(g, Bob, 3, CardStatus.Bluffed)
			hasStatus(g, Cathy, 2, CardStatus.CalledToDiscard)
			assert(!g.common.orderTrash(g, g.state.hands(Cathy.ordinal)(0)))
			hasInfs(g, None, Cathy, 1, Vector("r1", "y1", "b1", "p1"))
			hasStatus(g, Cathy, 3, CardStatus.None)
		.pipe(takeTurn("Bob plays b1", "p2"))
		.pipe(takeTurn("Cathy discards g1 (slot 2)", "p2"))

		// Cathy's remaining 1 (now in slot 2 after drawing into slot 1) is known playable [r1, y1, p1]!
		hasInfs(obs, None, Cathy, 2, Vector("r1", "y1", "p1"))

		// Receiving as Alice with a fresh 1 in slot 1 (drawn after turn 1) and starting-hand 1 in slot 3!
		val recv = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "y4", "b4", "g4"),
			Vector("y1", "p2", "b1", "y3", "b3")
		),
			playStacks = Some(Vector(0, 0, 1, 0, 0)),
			clueTokens = 7
		)
		.pipe(takeTurn("Alice discards y2 (slot 5)"))
		.pipe(takeTurn("Bob clues 1 to Alice (slots 1,3)"))
		.pipe(takeTurn("Cathy plays b1", "p4"))
		.pipe(takeTurn("Alice discards g1 (slot 1)"))

		assert(!recv.common.orderTrash(recv, recv.state.hands(Alice.ordinal)(2)))
		hasInfs(recv, None, Alice, 3, Vector("r1", "y1", "p1"))
		hasStatus(recv, Alice, 4, CardStatus.None)
		hasStatus(recv, Alice, 5, CardStatus.None)


class UnknownDupeDischarge extends munit.FunSuite:
	override def beforeAll() = Logger.setLevel(LogLevel.Off)

	test("gives and resolves 3-card color UDD with sarcastic link after discard (guide unknown-dupe-discharge)"):
		// Observing as giver
		val obs = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "b1", "y4", "b4"),
			Vector("r2", "r2", "r4", "p3", "b3")
		))
		.pipe(takeTurn("Alice clues red to Cathy"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			assertEquals(g.waiting.length, 1)
			hasStatus(g, Bob, 3, CardStatus.Bluffed)
			hasStatus(g, Cathy, 1, CardStatus.CalledToDiscard)
		.pipe(takeTurn("Bob plays b1", "p2"))
		.pipe(takeTurn("Cathy discards r2 (slot 1)", "g2"))
		.pipe(takeTurn("Alice clues 4 to Cathy"))

		// After Cathy discarded slot 1 and drew g2 into slot 1, former slots 2,3 are now slots 2,3!
		hasInfs(obs, None, Cathy, 3, Vector("r4"))
		hasInfs(obs, None, Cathy, 2, Vector("r2"))

		// Receiving as Alice (at 7 clues so playable r1 is also a UDD candidate; at 8 clues r1 is excluded!)
		val recv8 = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "y4", "b4", "g4"),
			Vector("g2", "p2", "b1", "y3", "b3")
		),
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slots 1,2,3)"))
		.pipe(takeTurn("Cathy plays b1", "p4"))

		hasInfs(recv8, None, Alice, 1, Vector("r2", "r3", "r4"))

		val recv = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "y4", "b4", "g4"),
			Vector("g2", "p2", "b1", "y3", "b3")
		),
			clueTokens = 7,
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slots 1,2,3)"))
		.pipe(takeTurn("Cathy plays b1", "p4"))
		.tap: g =>
			hasStatus(g, Alice, 1, CardStatus.CalledToDiscard)
			hasInfs(g, None, Alice, 1, Vector("r1", "r2", "r3", "r4"))
			assertEquals(g.takeAction.unsafeRunSync(), PerformAction.Discard(g.state.hands(Alice.ordinal)(0)))
		.pipe(takeTurn("Alice discards r2 (slot 1)"))
		.pipe(takeTurn("Bob clues 4 to Alice (slot 3)"))

		hasInfs(recv, None, Alice, 3, Vector("r4"))
		hasInfs(recv, None, Alice, 2, Vector("r2"))

	test("handles rank-initiated UDD and UDD duplicating an already-clued card in target's hand"):
		val rankUdd = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "b1", "y4", "b4"),
			Vector("r2", "r2", "g4", "p4", "b3")
		))
		.pipe(takeTurn("Alice clues 2 to Cathy"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			assertEquals(g.waiting.length, 1)
			hasStatus(g, Bob, 3, CardStatus.Bluffed)
			hasStatus(g, Cathy, 1, CardStatus.CalledToDiscard)
		.pipe(takeTurn("Bob plays b1", "p2"))
		.pipe(takeTurn("Cathy discards r2 (slot 1)", "g2"))

		hasInfs(rankUdd, None, Cathy, 2, Vector("r2"))

		val existingCluedUdd = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "b1", "y4", "b4"),
			Vector("r2", "g4", "r2", "p4", "b3")
		),
			init = preClue(Cathy, 3, Seq("red"))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))
		.tap: g =>
			assertEquals(g.lastMove, Some(ClueInterp.Play))
			assertEquals(g.waiting.length, 1)
			hasStatus(g, Bob, 3, CardStatus.Bluffed)
			hasStatus(g, Cathy, 1, CardStatus.CalledToDiscard)
		.pipe(takeTurn("Bob plays b1", "p2"))
		.pipe(takeTurn("Cathy discards r2 (slot 1)", "g2"))

		hasInfs(existingCluedUdd, None, Cathy, 3, Vector("r2"))

	test("marks mistake when duplicating a touched card in another player's hand"):
		val game = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "b1", "y4"),
			Vector("r2", "g4", "p4", "b3"),
			Vector("r2", "y3", "b4", "g5")
		),
			init = preClue(Donald, 1, Seq("red", "2"))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		assertEquals(game.lastMove, Some(ClueInterp.Mistake))

	test("does not trigger UDD on playable duplicates at 8 clues or in endgame"):
		// At 8 clues, touching two playable r1s in Cathy's hand is NOT a 3FP UDD.
		val eightClues = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "b1", "y4", "b4"),
			Vector("r1", "r1", "g4", "p4", "b3")
		),
			clueTokens = 8,
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		hasStatus(eightClues, Bob, 3, CardStatus.None)

		// In endgame (pace < numPlayers), touching two r4s in Cathy's hand is NOT a 3FP UDD.
		val endgame = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g5", "p5", "b4", "y5", "b5"),
			Vector("r4", "r4", "r5", "p4", "b4")
		),
			playStacks = Some(Vector(3, 4, 4, 3, 4)),
			discarded = Vector("r1", "r1", "y1", "y1", "g1", "g1", "b1", "b1", "p1", "p1", "r2", "y2", "g2"),
			clueTokens = 7,
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		assert(endgame.inEndgame)
		hasStatus(endgame, Bob, 3, CardStatus.None)

	test("discards focused chop card first ahead of left clued trash in UDD"):
		val game = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "y4", "b4", "g4"),
			Vector("g4", "p2", "b1", "y3", "b3")
		),
			playStacks = Some(Vector(0, 0, 1, 0, 0)),
			clueTokens = 1,
			starting = Bob,
			init = g =>
				val g1 = preClue(Alice, 2, Seq("red"))(g).copy(inEarlyGame = false)
				fullyKnown(Alice, 3, "g1")(g1)
		)
		.pipe(takeTurn("Bob clues red to Alice (slots 2,5)"))
		.pipe(takeTurn("Cathy plays b1", "p4"))

		hasStatus(game, Alice, 5, CardStatus.CalledToDiscard)
		// Alice must discard the discharged focus (slot 5) ahead of known clued trash g1 (slot 3)!
		assertEquals(game.takeAction.unsafeRunSync(), PerformAction.Discard(game.state.hands(Alice.ordinal)(4)))

	test("handles 2-card UDD TOCM when non-focused partner is discarded instead of focus"):
		// Observing Cathy discard slot 2 instead of slot 1 -> chop-moves Alice's chop (slot 5) and keeps Cathy's slot 1 as r2!
		val obs = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "b1", "y4", "b4"),
			Vector("r2", "r2", "g4", "p4", "b3")
		),
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Alice clues red to Cathy"))
		.pipe(takeTurn("Bob plays b1", "p2"))
		.pipe(takeTurn("Cathy discards r2 (slot 2)", "g2"))

		hasStatus(obs, Alice, 5, CardStatus.ChopMoved)
		hasStatus(obs, Cathy, 2, CardStatus.None)
		hasInfs(obs, None, Cathy, 2, Vector("r2"))

		// Performing 2-card UDD TOCM as Alice to save Bob's r5 on chop!
		val perf = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "y4", "b4", "r5"),
			Vector("g2", "p2", "b1", "y3", "b3")
		),
			clueTokens = 1,
			starting = Bob,
			init = _.copy(inEarlyGame = false)
		)
		.pipe(takeTurn("Bob clues red to Alice (slots 1,2)"))
		.pipe(takeTurn("Cathy plays b1", "p4"))

		// Alice has 0 clue tokens and Bob has critical r5 on chop -> Alice discards slot 2 (partner) instead of slot 1 (focus) to TOCM Bob!
		assertEquals(perf.takeAction.unsafeRunSync(), PerformAction.Discard(perf.state.hands(Alice.ordinal)(1)))
		val afterPerf = perf.pipe(takeTurn("Alice discards r2 (slot 2)"))
		hasStatus(afterPerf, Bob, 5, CardStatus.ChopMoved)
		hasStatus(afterPerf, Alice, 2, CardStatus.None)
		hasInfs(afterPerf, None, Alice, 2, Vector("r2"))

	test("falls back to direct play when 3FP does not blind-play on potential UTD/UDD"):
		val game = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "y4", "b4", "g4"),
			Vector("g2", "p2", "b1", "y3", "b3")
		),
			playStacks = Some(Vector(1, 0, 0, 0, 0)),
			clueTokens = 7,
			starting = Bob
		)
		.pipe(takeTurn("Bob clues red to Alice (slots 1,2)"))
		.pipe(takeTurn("Cathy discards b3", "p4"))

		hasStatus(game, Alice, 1, CardStatus.None)
		assert(!game.meta(game.state.hands(Alice.ordinal)(0)).trash)
		hasInfs(game, None, Alice, 1, Vector("r2"))

	test("does not trigger 5CE, UTD, or UDD when cluing Bob directly or at level 15"):
		// Cluing Bob directly (bob == target) cannot eject or discharge Bob's own hand.
		val clueBob5 = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r5", "b1", "y4", "p3", "b4"),
			Vector("g3", "g4", "y3", "p4", "b3")
		))
		.pipe(takeTurn("Alice clues red to Bob"))

		assertEquals(clueBob5.lastMove, Some(ClueInterp.Mistake))

		val clueBobUdd = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("r2", "r2", "b1", "p3", "b4"),
			Vector("g3", "g4", "y3", "p4", "b3")
		))
		.pipe(takeTurn("Alice clues red to Bob"))

		assertEquals(clueBobUdd.lastMove, Some(ClueInterp.Mistake))

		// At Level 15, 5CE does not exist.
		val lvl15 = setup(HGroup.atLevel(15), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "b1", "y4", "p3", "b4"),
			Vector("r5", "g4", "y3", "p4", "b3")
		))
		.pipe(takeTurn("Alice clues red to Cathy"))

		assertEquals(lvl15.lastMove, Some(ClueInterp.Mistake))

	test("does not trigger UDD on playable duplicates under DDA"):
		val ddaGame = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g3", "p3", "b1", "y4", "b4"),
			Vector("r1", "r1", "g4", "p4", "b3")
		),
			discarded = Vector("y3"),
			clueTokens = 7,
			init = _.copy(inEarlyGame = false, dda = Some(Identity(1, 3)))
		)
		.pipe(takeTurn("Alice clues red to Cathy"))

		hasStatus(ddaGame, Bob, 3, CardStatus.None)

	test("bot can choose 5CE, UTD, and UDD clues via takeAction"):
		// Give 5CE: Cathy holds r5 and Bob's 2FP is playable b1 (2-for-1 beats 1-for-1 direct clue on b1!)
		val give5ce = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "b1", "y4", "p3", "b4"),
			Vector("r5", "p4", "g3", "y3", "b3")
		),
			clueTokens = 8,
			init = _.copy(inEarlyGame = false)
		)
		assertEquals(give5ce.takeAction.unsafeRunSync(), PerformAction.Colour(Cathy.ordinal, 0))

		// Give 2-for-1 UTD: direct clues to Bob's b2 either bad-touch trash b1 (blue) or focus unplayable chop y2 (2)
		val giveUtd = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "p4", "b2", "b1", "y2"),
			Vector("r1", "r5", "g3", "p3", "y4")
		),
			playStacks = Some(Vector(1, 0, 0, 1, 0)),
			clueTokens = 7,
			init = _.copy(inEarlyGame = false)
		)
		assertEquals(giveUtd.takeAction.unsafeRunSync(), PerformAction.Colour(Cathy.ordinal, 0))

		// Give UDD: Cathy holds two r3s and Bob's 3FP is playable b2 (while blue/2 to Bob bad-touches b1 or focuses chop y2)
		val giveUdd = setup(HGroup.atLevel(16), Vector(
			Vector("xx", "xx", "xx", "xx", "xx"),
			Vector("g4", "p4", "b2", "b1", "y2"),
			Vector("r3", "r3", "g3", "y4", "b4")
		),
			playStacks = Some(Vector(0, 0, 0, 1, 0)),
			clueTokens = 7,
			init = _.copy(inEarlyGame = false)
		)
		val uddAction = giveUdd.takeAction.unsafeRunSync()
		assert(
			uddAction == PerformAction.Colour(Cathy.ordinal, 0) ||
			uddAction == PerformAction.Rank(Cathy.ordinal, 3),
			s"Expected UDD clue (red or 3 to Cathy), got $uddAction"
		)
