package scala_bot.hgroup

import scala_bot.basics._
import scala_bot.lib.FastBitSet
import scala_bot.utils._

import scala_bot.logger.{Log, Logger, LogLevel}

case class ConnectContext(
	looksDirect: Boolean,
	thinksStall: FastBitSet,
	connected: FastBitSet,
	ignore: FastBitSet = FastBitSet.empty
)

case class ConnectOpts(
	knownOnly: FastBitSet = FastBitSet.empty,
	assumeTruth: Boolean = false,
	bluff: Boolean = false,
	findOwn: Option[Int] = None,
	nonTargetFinessed: Boolean = false,
	noLayer: Boolean = false,
	preferOwn: Boolean = false,
	insertingInto: Option[Seq[Int]] = None,
	isTrashPush: Boolean = false
):
	inline def noDefer: Boolean = isTrashPush

def findKnownConn(ctx: ClueContext, id: Identity, finalId: Identity, baseIgnore: FastBitSet, opts: ConnectOpts): Option[Connection] =
	val ClueContext(prev, game, action, _) = ctx
	val (common, state) = (game.common, game.state)
	val giver = action.giver
	val findOwn = opts.findOwn.isDefined
	val ignore =
		if !opts.noDefer then baseIgnore
		else baseIgnore.union:
			state.heldOrders.filter: o =>
				prev.state.hands(state.holderOf(o)).exists(o2 => o2 != o && prev.isBlindPlaying(o2))

	// Try looking for a [r1, r2] card that can be disproved if someone else doesn't play
	val ownPlay = if !opts.preferOwn then None else
		var wcExists = false

		state.ourHand.find: o =>
			!ignore.contains(o) &&
			common.thoughts(o).possible.contains(id) &&
			common.thoughts(o).inferred.forall: i =>
				state.isPlayable(i) ||
				game.waiting.exists: wc =>
					val conn = wc.connections.find(!_.hidden).get
					val valid =
						i.prev.exists(conn.ids.contains) &&
						conn.reacting != state.ourPlayerIndex &&
						game.xmeta(conn.order).fStatus.contains(FStatus.PossiblyOn(state.ourPlayerIndex))

					if valid then
						wcExists = true

					valid
			&& wcExists
		.map: o =>
			KnownConn(state.ourPlayerIndex, o, id)

	if ownPlay.isDefined then
		return ownPlay

	val globallyKnown = state.heldOrders.findSome: o =>
		// Check whether the giver knows their card isn't a crit that they are currently cluing
		lazy val asymmetricKnown =
			state.holderOf(o) == giver &&
			state.isCritical(finalId) &&
			common.thoughts(o).inferred.forall: i =>
				i == id || i == finalId

		val validKnown =
			!ignore.contains(o) &&
			game.state.deck(o).matches(id, assume = !game.common.thoughts(o).reset) &&
			(common.thoughts(o).matches(id, infer = true) || asymmetricKnown) &&
			!(!prev.state.deck(o).clued && common.thoughts(o).possible.length > 1 && common.thoughts(ctx.focusResult.focus).inferred.contains(id)) &&
			!game.meta(o).hidden &&
			!game.xmeta(o).fStatus.contains(FStatus.PossiblyOn(giver)) &&
			(game.assumePlays || !game.xmeta(o).fStatus.contains(FStatus.PossiblyOn(state.ourPlayerIndex))) &&
			!common.isLinked(state, o)

		Option.when(validKnown)(KnownConn(state.holderOf(o), o, id))

	if globallyKnown.isDefined then
		return globallyKnown

	val promised = state.heldOrders.findSome: o =>
		if ignore.contains(o) || common.thoughts(o).inferred.difference(state.playableSet).nonEmpty then
			None
		else
			val link = common.links.find: link =>
				link.matchesP:
					case Link.Promised(orders, i, _) => i == id && orders.contains(o)
					case Link.Sarcastic(orders, i)   => i == id && orders.contains(o)

			link.map: l =>
				PlayableConn(state.holderOf(o), o, id, linked = l.getOrders.toList)

	if promised.isDefined then
		return promised

	// Log.info(s"finding known ${state.logId(id)} ${ignore.fmt} ${common.linkedOrders(state)} $findOwn")

	def validPlayable(playerIndex: Int, order: Int) =
		!ignore.contains(order) &&
		common.thoughts(order).inferred.contains(id) &&
		!game.xmeta(order).fStatus.contains(FStatus.PossiblyOn(giver)) &&
		(game.assumePlays || !game.xmeta(order).fStatus.contains(FStatus.PossiblyOn(state.ourPlayerIndex))) && {
			common.orderKp(game, order, excludeTrash = true) || {
				// Mimic common.orderPlayable, except we can also try playing previously-known unrelated plays
				!game.meta(order).trash &&
				!common.isLinked(state, order, unpromisedOnly = true) && {
					val poss = common.thoughts(order).possibilities.difference(state.trashSet)

					poss.nonEmpty && poss.forall: i =>
						state.isPlayable(i) ||
						(state.playStacks(i.suitIndex) + 1 until i.rank).forall: rank =>
							val id = Identity(i.suitIndex, rank)
							common.hypoPlays.exists(state.deck(_).matches(id))
				}
			}
		} &&
		!(	// Don't connect on our unknown playables for an id matching the focus:
			// giver would be bad touching
			playerIndex == state.ourPlayerIndex &&
			state.deck(ctx.focusResult.focus).id().contains(id)
		)

	// Visible and going to be played (excludes giver)
	val playable = state.hands.zipWithIndex.findSome: (hand, playerIndex) =>
		if playerIndex == giver then None else
			val playables = hand.filter(validPlayable(playerIndex, _))
			val playableOrder =
				playables.filter: p =>
					state.deck(p).matches(id, assume = game.allowFindOwn && findOwn) && game.isTouched(p)
				.minByOption: p =>
					// Prefer playing the option with the most info first
					val thought = game.common.thoughts(p)
					thought.infoLock.getOrElse(thought.possibilities).length

			playableOrder.map(PlayableConn(playerIndex, _, id, linked = playables.toList))

	if playable.isDefined then
		return playable

	val playLinked = state.heldOrders.findSome: o =>
		val playerIndex = state.holderOf(o)
		val play =
			!ignore.contains(o) &&
			playerIndex != giver &&
			state.deck(o).clued &&
			common.playLinks.exists(_.target == o) &&
			common.thoughts(o).inferred.contains(id) &&
			state.deck(o).matches(id, assume = game.allowFindOwn && findOwn)

		Option.when(play):
			PlayableConn(playerIndex, o, id, linked = List(o))

	if playLinked.isDefined then
		return playLinked

	val assumeKnown = state.hands(giver).findSome: o =>
		// Maybe someone clued 2 to us and the giver knows their 1s must connect
		val playLink = common.playLinks.find(pl => pl.orders.contains(o) && state.ourHand.contains(pl.target))

		// Maybe someone clued a colour to us (in an omni variant) and the giver knows their 1 must connect
		val waitingConn = game.waiting.find: wc =>
			val giverConn = wc.connections.indexWhere(c => c.order == o && c.ids.length == 1 && c.ids.contains(id))

			giverConn != -1 &&
			wc.giver != giver &&
			(wc.connections.drop(giverConn + 1).exists(_.reacting == state.ourPlayerIndex) || wc.receiver == state.ourPlayerIndex)

		val play =
			!ignore.contains(o) &&
			state.deck(o).clued &&
			(playLink.isDefined || waitingConn.isDefined) &&
			common.thoughts(o).inferred.contains(id) &&
			state.deck(o).matches(id)

		Option.when(play):
			PlayableConn(giver, o, id, linked = playLink.map(_.orders).getOrElse(Nil))

	assumeKnown

def findUnknownConnecting(ctx: ClueContext, reacting: Int, id: Identity, connected: FastBitSet, ignore: FastBitSet, opts: ConnectOpts): Option[Connection] =
	val ClueContext(prev, game, action, _) = ctx
	val (state, level) = (game.state, game.level)
	val ClueAction(giver, receiver, _, _) = action

	// val flags = List(
	// 	opts.findOwn.map(own => s"own ($own)"),
	// 	Option.when(opts.noLayer)("no layer"),
	// 	opts.insertingInto.map(i => s"inserting into $i"),
	// 	Option.when(opts.bluff)("bluff")
	// ).flatten.mkString(", ")
	// Log.info(s"finding unknown connecting for ${state.logId(id)} (${state.names(reacting)}), ${connected.fmt}, flags: [$flags]")

	def validOrder(order: Int) =
		!opts.noDefer || !prev.state.hands(reacting).exists(o => o != order && prev.isBlindPlaying(o))

	if opts.bluff then
		val clued = prev.common.findClued(prev, reacting, id, ignore.union(connected))
		val matched = clued.find(o => validOrder(o) && state.deck(o).matches(id, assume = opts.findOwn.isDefined))

		return matched.map: order =>
			PlayableConn(reacting, order, id, linked = clued.toList)

	def tryPrompt(order: Int) =
		if !validOrder(order) then
			None
		else
			opts.findOwn.map(game.players(_).thoughts(order).id()).getOrElse(state.deck(order).id()) match
				case None =>
					Option.when(opts.findOwn.exists(game.players(_).thoughts(order).possible.contains(id)))
						(PromptConn(reacting, order, id))

				case Some(promptId) =>
					if promptId == id then
						Some(PromptConn(reacting, order, id))

					else if !opts.noDefer && level >= Level.IntermediateFinesses && state.isPlayable(promptId) then
						if game.common.orderPlayable(game, order, excludeTrash = true) then
							Some(PlayableConn(reacting, order, promptId, hidden = true))
						else
							Some(PromptConn(reacting, order, promptId, hidden = true))

					else
						// Log.warn(s"wrong prompt on ${state.logId(order)} $order, stacks ${state.playStacks}")
						None

	// Need to use 'game' to exclude non-omni cards after a colour clue
	// But we also can't use newly clued cards
	val prompt = game.common.findPrompt(game, reacting, id, connected, ignore.union(state.hands(reacting).filter(!prev.state.deck(_).clued)))
	// Need to use 'game' to exclude newly clued cards
	val potentialFinesse = game.findFinesseId(
		reacting,
		id,
		connected,
		ignore,
		overrideLayer = opts.insertingInto.exists(_.exists(prev.state.hands(reacting).contains))
	)

	if prompt.exists(!rainbowMismatch(prev, game, action, id, _)) then
		return tryPrompt(prompt.get)

	// Try prompting a wrongly-ranked pink card
	val tryPinkPrompt = state.variant.pinkish &&
		potentialFinesse.forall: f =>
			if level < Level.Bluffs then
				!game.common.thoughts(f).possible.intersect(state.playableSet).exists: i =>
					state.variant.suits(i.suitIndex).suitType.pinkish
			else
				game.common.thoughts(f).possible.intersect(state.playableSet).isEmpty

	if tryPinkPrompt then
		val pinkPrompt = prev.common.findPrompt(prev, reacting, id, connected, ignore, forcePink = true)
		val pinkPromptConn = pinkPrompt.filter(!prompt.contains(_)).flatMap(tryPrompt)

		if pinkPromptConn.isDefined then
			return pinkPromptConn

	val cluedDupe = state.hands.zipWithIndex.exists: (hand, i) =>
		i != giver && hand.exists(o => prev.state.deck(o).clued && state.deck(o).matches(id))

	if cluedDupe then
		// Log.warn(s"disallowed finesse, ${state.logId(id)} already clued")
		return None

	// Try to insert into an earlier finesse
	val cantInsert = game.level < Level.IntermediateFinesses ||
		!opts.findOwn.contains(reacting) ||
		state.isInverted(id) ||
		potentialFinesse.exists(game.common.thinksInverted(state, _))

	val insertingInto = if cantInsert then None else
		val prevHand = state.hands(reacting).dropWhile: f =>
			game.meta(f).status != CardStatus.Finessed ||
			game.xmeta(f).finesseIds.isEmpty ||
			game.future(f).length == 1

		if prevHand.isEmpty then None else
			// Checks whether all inserting orders can be placed
			def insertable(hypo: HGroup, insertOrders: Seq[Int]): Boolean =
				insertOrders.foldLeftOpt(connected): (newConnected, o) =>
					val finesseIds = hypo.xmeta(o).finesseIds.get
					val nextFinesse = hypo.findFinesse(reacting, newConnected, ignore)

					// Log.info(s"checking that $o (${finesseIds.fmt(state)}) can be shifted to $nextFinesse (${nextFinesse.map(hypo.common.strPoss(hypo.state, _))})")

					// At least one id is possible on the next card on finesse position
					val possible = nextFinesse.exists(o => hypo.future(o).intersect(hypo.common.thoughts(o).possible).intersect(finesseIds).nonEmpty)

					Either.cond(possible, newConnected.incl(nextFinesse.get), FastBitSet.empty)
				.nonEmpty

			def canInsert(prevHand: Vector[Int], insertOrders: Seq[Int], firstInsert: Boolean): Boolean =
				prevHand.nonEmpty && insertOrders.nonEmpty && {
					val replacement = prevHand.head
					val possibleIds = if firstInsert then game.xmeta(replacement).finesseIds.get else game.common.thoughts(replacement).possible

					// Log.info(s"trying to insert ${state.logId(id)} at $replacement with prev hand $prevHand, shifting $insertOrders ${possibleIds.fmt(state)}")

					!ignore.contains(replacement) &&
					validOrder(replacement) &&
					possibleIds.contains(id) &&
					insertable(game.withState(s => s.copy(hands = s.hands.updated(reacting, prevHand.tail))), insertOrders)
				}

			opts.insertingInto match
				case None =>
					val insertOrders = prevHand.filter(o => game.meta(o).status == CardStatus.Finessed && !game.meta(o).hidden)
					Option.when(canInsert(prevHand, insertOrders, firstInsert = true)):
						(prevHand.head, insertOrders)

				case Some(insertOrders) =>
					val prevHand2 = state.hands(reacting).dropWhile(_ > insertOrders.head)
					Option.when(canInsert(prevHand2, insertOrders, firstInsert = false)):
						(prevHand2.head, insertOrders)

	val knownLayeredIds = potentialFinesse.flatMap: f =>
		val future = game.future(f)
		val playableIds = future.intersect(game.state.playableSet)

		Option.when(!future.contains(id) && playableIds.nonEmpty)(playableIds)

	// We previously inserted, but we can't insert any more: leave a spot for the replacements
	val rawFinesse =
		if opts.insertingInto.isDefined && insertingInto.isEmpty then
			val insertOrders = opts.insertingInto.get
			val prevHand = state.hands(reacting).dropWhile(_ > insertOrders.head)
			val hypo = prev.withState(s => s.copy(hands = s.hands.updated(reacting, prevHand)))

			val newConnected = (0 until insertOrders.length).foldLeftOpt(connected): (newConnected, _) =>
				hypo.findFinesse(reacting, newConnected, ignore) match
					case None => Left(FastBitSet.empty)
					case Some(f) => Right(newConnected.incl(f))

			if newConnected.isEmpty then None else
				game.findFinesseId(reacting, id, newConnected, ignore, overrideLayer = opts.insertingInto.exists(_.exists(prev.state.hands(reacting).contains)))
		else
			potentialFinesse
	val finesse = rawFinesse.filter(validOrder)

	finesse.flatMap(state.deck(_).id()) match
		case _ if finesse.exists(o => game.future(o).length < game.common.thoughts(o).possible.length) =>
			val inverted = state.isInverted(id)
			val possibleIds = if inverted then state.trashSet.union(state.playableSet.filter(state.isInverted)) else state.playableSet
			val futureIds = game.future(finesse.get).intersect(possibleIds)

			if futureIds.isEmpty then
				// Log.warn(s"future knowledge of ${finesse.get} is [${game.future(finesse.get).fmt(state)}], but all unplayable!")
				None
			else
				val fKind =
					if !opts.assumeTruth && futureIds.exists(validBluff(game, action, _, reacting, connected)) then
						FinesseKind.Bluff
					else if futureIds.isExactly(id) then
						FinesseKind.True
					else
						FinesseKind.Hidden

				Option.when(!opts.noDefer || fKind != FinesseKind.Hidden):
					FinesseConn(reacting, finesse.get, futureIds.toList, fKind, inverted = inverted)

		case None if !opts.noDefer && finesse.isDefined && knownLayeredIds.isDefined =>
			Some(FinesseConn(reacting, finesse.get, knownLayeredIds.get.toList, FinesseKind.Hidden))

		case _ if insertingInto.isDefined =>
			val (order, inserts) = insertingInto.get
			Some(PlayableConn(reacting, order, id, insertingInto = Some(inserts)))

		case None if opts.findOwn.isDefined && finesse.isDefined =>
			if prev.meta(finesse.get).status == CardStatus.Finessed && !prev.common.thoughts(finesse.get).reset && game.common.thoughts(finesse.get).reset then
				// Attempted to replace this finessed card with a new finessed card: this player cannot be asked to play any cards
				None
			else
				val thought = game.players(opts.findOwn.get).thoughts(finesse.get)
				val finesseIds = game.xmeta(finesse.get).finesseIds

				if finesseIds.exists(!_.contains(id)) then
					Option.when(!opts.noDefer):
						FinesseConn(reacting, finesse.get, thought.inferred.toList, FinesseKind.Hidden)
				else
					val bluffableIds =
						if opts.assumeTruth then IdentitySet.empty
						else thought.inferred.filter: i =>
							state.isPlayable(i) &&
							validBluff(game, action, i, reacting, connected)

					val trueFinesse = thought.infoLock.getOrElse(thought.possible).contains(id) &&
						thought.matches(id, assume = true) &&
						(!opts.noDefer || thought.inferred.contains(id))

					Option.when(trueFinesse || bluffableIds.nonEmpty):
						val certain = state.hands(giver).exists: o =>
							val card = state.deck(o)
							card.matches(id) && card.clued

						val fKind =
							if !opts.assumeTruth && bluffableIds.nonEmpty then
								// if !thought.possible.contains(id) then FinesseKind.Bluff else FinesseKind.PossiblyBluff
								FinesseKind.Bluff
							else if certain then
								FinesseKind.Certain
							else if !thought.inferred.contains(id) then
								FinesseKind.Hidden
							else
								FinesseKind.True

						val ids = if trueFinesse then
							List(thought.id(infer = true).getOrElse(id))
						else
							game.common.thoughts(finesse.get).inferred.filter: i =>
								state.isPlayable(i) &&
								validBluff(game, action, i, reacting, connected)
							.union(id)
							.toList

						FinesseConn(reacting, finesse.get, ids, fKind)

		case None => None

		case Some(finesseId) if finesseId == id =>
			if level == 1 && !inBetween(state.numPlayers, reacting, giver, receiver) then
				Log.warn(s"found non-forward finesse ${state.logId(id)} in ${state.names(reacting)}'s hand at lv 1!")
				None
			else
				val possiblyBluff = !opts.assumeTruth &&
					validBluff(game, action, finesseId, reacting, connected)

				Some(FinesseConn(reacting, finesse.get, List(id), fKind = if possiblyBluff then FinesseKind.Bluff else FinesseKind.True))

		case Some(finesseId) =>
			if Vector(CardStatus.GentlemansDiscard, CardStatus.Sarcastic).contains(game.meta(finesse.get).status) && state.isPlayable(finesseId) && !state.isInverted(finesseId) && !state.isInverted(id) then
				Option.when(!opts.noDefer)(PlayableConn(reacting, finesse.get, finesseId, hidden = true))
			else
				val possiblyBluff = !opts.assumeTruth &&
					validBluff(game, action, finesseId, reacting, connected)

				if opts.findOwn.exists(i => i != state.ourPlayerIndex && game.players(i).thoughts(finesse.get).possible.contains(id)) then
					Some(FinesseConn(reacting, finesse.get, List(id), fKind = if possiblyBluff then FinesseKind.Bluff else FinesseKind.True))

				else if !opts.noLayer && !opts.noDefer && level >= Level.IntermediateFinesses && state.isPlayable(finesseId) && !state.isInverted(finesseId) && !state.isInverted(id) then
					if game.meta(finesse.get).status == CardStatus.Finessed && game.isDefinite(finesse.get) then
						Some(PlayableConn(reacting, finesse.get, finesseId, hidden = true))
					else
						// TODO: Check in resolver that we don't give uncertain finesses or likely dupes
						// Giver cannot give a layered finesse when they have a card matching the desired id
						val uncertainFinesse = !possiblyBluff &&
							state.hands(giver).exists(o => state.deck(o).clued && game.players(giver).thoughts(o).inferred.contains(id))

						val orangeFib = state.isInverted(id) && !state.isInverted(finesseId)

						if uncertainFinesse then
							Log.warn(s"disallowed hidden finesse on ${state.names(reacting)}, ${state.logId(id)} could be duplicated in giver's hand")
							None
						else if orangeFib then
							Log.warn("disallowed bluff when the focus is an orange card!")
							None
						else
							Some(FinesseConn(reacting, finesse.get, List(finesseId), fKind = if possiblyBluff then FinesseKind.Bluff else FinesseKind.Hidden))

				else
					None

def findSingleConn(ctx: ClueContext, reacting: Int, id: Identity, connCtx: ConnectContext, opts: ConnectOpts, connections: List[Connection] = Nil): Option[List[Connection]] =
	val ClueContext(prev, game, action, _) = ctx
	val state = game.state
	val ClueAction(giver, receiver, _, _) = action

	lazy val allVisible =
		val remaining = state.cardCount(id.toOrd) - state.baseCount(id.toOrd)

		// Everyone between giver and reacting must be able to see them,
		// otherwise they will try to react
		val visible = playersUntil(state.numPlayers, state.nextPlayerIndex(reacting), giver)
			.summing(state.hands(_).count(state.deck(_).matches(id)))

		remaining == visible

	val skip = reacting == giver ||
		opts.knownOnly.contains(reacting) ||
		(reacting == receiver && connCtx.looksDirect && !allVisible) ||
		(connCtx.thinksStall.contains(reacting) && {	// might not think stall if a clue exists in our hand
			giver == state.ourPlayerIndex ||
			state.ourHand.forall: o =>
				state.deck(o).clued || game.me.orderKt(game, o)
		})

	// TODO: When resolving, disallow prompting/finessing a player that may need to prove sth to us
	if skip then
		// Log.info(s"skipping ${state.names(reacting)}, giver $giver receiver $receiver $connCtx ${opts.knownOnly.fmt}")
		None
	else
		findUnknownConnecting(ctx, reacting, id, connCtx.connected, connCtx.ignore, opts) match
			case None => None

			// Try again
			case Some(conn) if conn.hidden =>
				val selfClandestine = reacting == receiver &&
					conn.ids.head.next.exists(game.common.thoughts(ctx.focusResult.focus).possible.contains) &&
					// Someone else finessing will prove this is a clandestine self.
					!opts.nonTargetFinessed

				if selfClandestine then
					// Log.warn("illegal clandestine self-finesse!")
					None
				else
					val hypo = state.deck(conn.order).id().orElse(Option.when(conn.ids.length == 1)(conn.ids.head))
						.fold(game)(id => game.withState(_.withPlay(id)).elim(skipHypoStacks = true))

					val newConnCtx = connCtx.copy(connected = connCtx.connected.incl(conn.order))
					findSingleConn(ctx.copy(game = hypo), reacting, id, newConnCtx, opts, conn +: connections)

			case Some(conn) =>
				Some((conn +: connections).reverse)

def findConnecting(ctx: ClueContext, id: Identity, finalId: Identity, connCtx: ConnectContext, opts: ConnectOpts): Option[List[Connection]] =
	val ClueContext(prev, game, action, _) = ctx
	val state = game.state
	val ClueAction(giver, receiver, _, _) = action

	// Log.highlight(Console.GREEN, s"find connecting ${state.logId(id)}")

	if state.baseCount(id.toOrd) == state.cardCount(id.toOrd) then
		Log.info(s"all ${state.logId(id)} in trash!")
		return None

	val known = findKnownConn(ctx, id, finalId, connCtx.ignore.union(connCtx.connected), opts)

	if known.isDefined then
		return Some(known.toList)

	val mustPassback = game.common.thoughts(ctx.focusResult.focus).inferred.exists: i =>
		i.suitIndex != id.suitIndex &&
		state.playStacks(i.suitIndex) <= state.playStacks(id.suitIndex)

	val connPlayerOrder =
		if mustPassback then
			List(
				List(receiver),	// first, try without layered
				(0 until state.numPlayers)
					.map(i => (giver - i - 1 + state.numPlayers) % state.numPlayers)
					.filterNot(_ == receiver),
				List(receiver),	// allow self-layered if no one will fix a self-clandestine
			).flatten
		else
			(1 until state.numPlayers).map(i => (giver - i + state.numPlayers) % state.numPlayers)

	connPlayerOrder.zipWithIndex.findSome: (reacting, i) =>
		findSingleConn(ctx, reacting, id, connCtx, opts.copy(noLayer = opts.noLayer || (mustPassback && i == 0)))

def connect(ctx: ClueContext, id: Identity, looksDirect: Boolean, thinksStall: FastBitSet, assumeTruth: Boolean = false, ignoreKnown: FastBitSet = FastBitSet.empty, findOwn: Option[Int] = None, preferOwn: Boolean = false, forTrashFinesse: Boolean = false): List[FocusPossibility] =
	val ClueContext(prev, game, action, _) = ctx
	val ClueAction(giver, receiver, _, clue) = action
	val state = game.state
	val FocusResult(focus, _, positional) = ctx.focusResult
	val isTrashPush = positional.contains(Positional.TrashPush)

	// Log.highlight(Console.MAGENTA, s"trying to connect ${state.logId(id)}${if (looksDirect) " (looks direct)" else ""} $findOwn")

	val targetRank = if forTrashFinesse then id.rank + 1 else id.rank

	def loop(hypo: HGroup, nextRank: Int, connections: List[Connection], connCtx: ConnectContext, opts: ConnectOpts): List[(Either[List[Connection], List[Connection]], Boolean)] =
		if nextRank >= targetRank then
			return List((Right(connections), opts.bluff))

		val nextId = Identity(id.suitIndex, nextRank)
		val newCtx = ctx.copy(game = hypo)

		val (stepConnCtx, stepOpts) =
			if !opts.isTrashPush then
				(connCtx, opts)
			else
				val lastPlayer = connections.lastOption.map(_.reacting).getOrElse(giver)
				val outOfOrder = (0 until state.numPlayers).filterNot: p =>
					inBetween(state.numPlayers, p, state.nextPlayerIndex(lastPlayer), receiver)
				(
					connCtx.copy(ignore = connCtx.ignore.union(outOfOrder.flatMap(state.hands(_)))),
					opts.copy(knownOnly = opts.knownOnly.union(outOfOrder))
				)

		def seekOwn(playerIndex: Int) =
			val ownOpts = stepOpts.copy(findOwn = Some(playerIndex))
			val known = findKnownConn(newCtx, nextId, id, stepConnCtx.ignore.union(stepConnCtx.connected), ownOpts)

			// See if we need to correct based on future information
			known match
				case Some(c @ PlayableConn(reacting, order, id, linked, hidden, _)) =>
					if linked.forall(!game.future(_).contains(id)) then
						val playableIds = game.future(order).intersect(state.playableSet)

						if playableIds.isEmpty then None else
							Log.highlight(Console.CYAN, s"playable conn ($order) is known to not match in the future, finding again")
							val unknown = findSingleConn(newCtx, playerIndex, nextId, stepConnCtx.copy(connected = stepConnCtx.connected.incl(order)), ownOpts)
							unknown.map(c.copy(id = playableIds.head) +: _)
					else
						Some(List(c))
				case Some(conn) => Some(List(conn))
				case None =>
					findSingleConn(newCtx, playerIndex, nextId, stepConnCtx, ownOpts)

		val possibleConns =
			if opts.preferOwn then
				List(seekOwn(state.ourPlayerIndex), findConnecting(newCtx, nextId, id, stepConnCtx, stepOpts)).flatten
			else
				findConnecting(newCtx, nextId, id, stepConnCtx, stepOpts)
				.orElse(findOwn.flatMap(seekOwn))
				.toList

		if possibleConns.isEmpty then
			// Log.info(s"failed connection to ${state.logId(id)}: ${state.logConns(connections, nextId)} $findOwn")
			return List((Left(connections), opts.bluff))

		else
			possibleConns.flatMap: conns =>
				// Log.info(s"found conns ${state.logConns(conns)} $findOwn")

				val newGame = conns.foldLeft(hypo): (acc, conn) =>
					acc.state.deck(conn.order).id()
						.orElse(Option.when(conn.ids.length == 1)(conn.ids.head))
						.fold(acc): i =>
							val playAction = PlayAction(state.holderOf(conn.order), conn.order, Some(i))
							val level = Logger.level

							Logger.setLevel(LogLevel.Error.min(level))

							// Resolve wcs after playing the card
							val res = acc.onPlay(playAction)
								.pipe(refreshWCs(acc, _, playAction, hypo = Some(giver)))

							Logger.setLevel(level)
							res

				val newConnCtx = connCtx.copy(
					looksDirect = connCtx.looksDirect && {
						(clue.kind == ClueKind.Colour && nextId.next.exists(game.common.thoughts(focus).possible.contains)) ||
						positional.isDefined ||
						!conns.existsM:
							case f: FinesseConn => f.reacting != receiver && !f.hidden
							case f: PromptConn  => f.reacting != receiver && !f.hidden
					},
					connected = connCtx.connected.union(conns.map(_.order))
				)

				val newOpts = opts.copy(
					bluff = opts.bluff || conns.exists(_.isPossiblyBluff),
					nonTargetFinessed = opts.nonTargetFinessed || conns.exists(c => c.isInstanceOf[FinesseConn] && c.reacting != receiver),
					insertingInto = opts.insertingInto.orElse(conns.collectFirst { case c: PlayableConn if c.insertingInto.isDefined => c.insertingInto.get })
				)

				if !forTrashFinesse && connections.isEmpty && ignoreKnown.isEmpty && conns.exists(_.isPossiblyBluff) && isIntermediateBluffTarget(prev, game, action, id, focus) then
					List((Right(conns), true))
				else
					loop(newGame, nextRank + 1, connections ++ conns, newConnCtx, newOpts)

	val ignore =
		// Assume mud clues aren't selfish
		if ctx.focusResult.positional.contains(Positional.Mud) then
			ignoreKnown.union(state.hands(giver))
		else
			ignoreKnown

	val unconfirmedTrash = state.heldOrders.filter(prev.provingTrash(_, giver, findOwn))
	val initialConnCtx = ConnectContext(looksDirect = looksDirect, thinksStall = thinksStall, connected = FastBitSet.single(focus), ignore = ignore.union(unconfirmedTrash))
	val initialOpts = ConnectOpts(
		assumeTruth = assumeTruth || isTrashPush,
		preferOwn = preferOwn,
		isTrashPush = isTrashPush
	)

	val normalRes = loop(game, state.playStacks(id.suitIndex) + 1, Nil, initialConnCtx, initialOpts).flatMap:
		case (Left(conns), bluffed) =>
			val truthConns =
				if game.level >= Level.Bluffs && !assumeTruth && bluffed then
					Log.highlight(Console.MAGENTA, "bluff connection failed, retrying true finesse")
					connect(ctx, id, looksDirect, thinksStall, assumeTruth = true, findOwn = findOwn, ignoreKnown = ignore, preferOwn = preferOwn, forTrashFinesse = forTrashFinesse)
				else
					List.empty

			if truthConns.nonEmpty then
				truthConns
			else if conns.existsM { case _: KnownConn => true } then
				val newIgnore = ignore.incl(conns.collectFirst { case c: KnownConn => c.order }.get)
				connect(ctx, id, looksDirect, thinksStall, ignoreKnown = newIgnore, findOwn = findOwn, assumeTruth = assumeTruth, preferOwn = preferOwn, forTrashFinesse = forTrashFinesse)
			else
				List.empty

		case (Right(conns), _) if forTrashFinesse =>
			findTrashFinesses(ctx, conns, ambiguous = preferOwn)

		case (Right(conns), _) =>
			val symmetric = !state.deck(focus).matches(id, assume = receiver == state.ourPlayerIndex) ||
				!game.players(receiver).thoughts(focus).possible.contains(id) ||
				// Newly touching a visible dupe
				(!prev.isTouched(focus) && visibleFind(state, prev.players(giver), id, infer = true, excludeOrder = focus).exists(prev.isTouched)) ||
				symmetricConns(game, conns) ||
				conns.exists(c => prev.provingTrash(c.order, giver))

			val finalizedConns = finalizeConns(conns, givenByUs = giver == state.ourPlayerIndex)

			Log.info(s"found connections: ${state.logConns(finalizedConns, id)}${if symmetric then " (symmetric)" else ""}${if preferOwn then " (ambiguous)" else ""}")
			Some(FocusPossibility(IdentitySet.single(id), finalizedConns, ClueInterp.Play, symmetric, ambiguous = preferOwn))

	val bob = state.nextPlayerIndex(giver)
	val eligible5CE =
		game.level >= Level.Ejections &&
		state.numPlayers >= 3 &&
		bob != receiver &&
		clue.kind == ClueKind.Colour &&
		id.rank == 5 &&
		!state.isBasicTrash(id) &&
		!state.isInverted(id) &&
		state.playStacks(id.suitIndex) <= 2 &&
		!prev.state.deck(focus).clued &&
		!prev.isTouched(focus) &&
		positional.isEmpty &&
		!forTrashFinesse &&
		ignoreKnown.isEmpty

	val baseNormalRes =
		if !eligible5CE then
			normalRes
		else
			normalRes.filterNot: fp =>
				fp.connections.count(c => c.reacting == bob && c.isInstanceOf[FinesseConn]) >= 2

	if eligible5CE && !initialOpts.assumeTruth && !preferOwn && baseNormalRes.isEmpty then
		return connect5ColorEjection(ctx, id, looksDirect, thinksStall, initialConnCtx.ignore, findOwn)

	val canTryDoubleBluff =
		game.level >= Level.DoubleBluffs &&
		state.numPlayers >= 3 &&
		!initialOpts.assumeTruth &&
		!preferOwn &&
		!forTrashFinesse &&
		ignoreKnown.isEmpty

	if !canTryDoubleBluff then
		baseNormalRes
	else if baseNormalRes.isEmpty then
		connectDoubleBluff(ctx, id, looksDirect, thinksStall, initialConnCtx.ignore, findOwn)
	else
		val cathy = state.nextPlayerIndex(bob)

		def isUnseen(order: Int) =
			state.deck(order).id().isEmpty && game.future(order).length > 1

		def isPreemptedTriple(fp: FocusPossibility) =
			val finesses = fp.connections.collect { case f: FinesseConn => f }
			finesses.length >= 3 &&
			finesses(0).reacting == bob &&
			finesses(1).reacting == cathy

		def isUnseenMultiSelfOnBob(fp: FocusPossibility) =
			findOwn.contains(bob) &&
			fp.connections.headOption.exists(_.reacting == bob) &&
			fp.connections.exists(c => c.reacting == bob && isUnseen(c.order)) &&
			fp.connections.count(_.isInstanceOf[FinesseConn]) >= 2

		def isUnseenSelfOnCathy(fp: FocusPossibility) =
			findOwn.contains(cathy) &&
			fp.connections.exists(c => c.reacting == cathy && isUnseen(c.order))

		if baseNormalRes.exists(fp => isPreemptedTriple(fp) || isUnseenMultiSelfOnBob(fp) || isUnseenSelfOnCathy(fp)) then
			val dbRes = connectDoubleBluff(ctx, id, looksDirect, thinksStall, initialConnCtx.ignore, findOwn)
			if dbRes.isEmpty then
				baseNormalRes
			else
				val dbOrders = dbRes.head.connections.map(_.order)
				val (sameOrders, otherNormal) = baseNormalRes.filterNot(isPreemptedTriple).partition(_.connections.map(_.order) == dbOrders)
				val mergedDb = if sameOrders.isEmpty then dbRes else dbRes.map: db =>
					val mergedConns = db.connections.zipWithIndex.map: (conn, idx) =>
						conn match
							case f: FinesseConn =>
								val extraIds = sameOrders.flatMap(_.connections(idx).ids)
								f.copy(ids = (f.ids ++ extraIds).distinct)
							case other => other
					db.copy(connections = mergedConns)
				mergedDb ++ otherNormal
		else
			baseNormalRes

def connect5ColorEjection(ctx: ClueContext, id: Identity, looksDirect: Boolean, thinksStall: FastBitSet, ignore: FastBitSet, findOwn: Option[Int]): List[FocusPossibility] =
	val ClueContext(prev, game, action, _) = ctx
	val ClueAction(giver, receiver, _, _) = action
	val state = game.state
	val FocusResult(focus, _, _) = ctx.focusResult

	val bob = state.nextPlayerIndex(giver)

	def thinksClueIsStall(p: Int) =
		thinksStall.contains(p) && (
			giver == state.ourPlayerIndex ||
			state.ourHand.forall(o => state.deck(o).clued || game.me.orderKt(game, o))
		)

	def resetFinesse(o: Int) =
		prev.meta(o).status == CardStatus.Finessed &&
		!prev.common.thoughts(o).reset &&
		game.common.thoughts(o).reset

	def knownId(o: Int): Option[Identity] =
		state.deck(o).id().orElse:
			Option.when(game.future(o).length == 1)(game.future(o).head)

	if thinksClueIsStall(bob) || bluffInterferes(game, giver, bob) then
		return Nil

	def advanceHypo(hypo: HGroup, playerIdx: Int, order: Int, playId: Identity): HGroup =
		val playAction = PlayAction(playerIdx, order, Some(playId))
		val level = Logger.level
		Logger.setLevel(LogLevel.Error.min(level))
		val res = hypo.onPlay(playAction)
			.pipe(refreshWCs(hypo, _, playAction, hypo = Some(giver)))
		Logger.setLevel(level)
		res

	val (_, _, bobBlindPlays) = ((state.playStacks(id.suitIndex) + 1) to 4).foldLeft((game, FastBitSet.single(focus), 0)):
		case ((hypo, usedOrders, blindCount), r) =>
			val reqId = Identity(id.suitIndex, r)
			val stepCtx = ctx.copy(game = hypo)

			val knownOpt = findKnownConn(stepCtx, reqId, id, ignore.union(usedOrders), ConnectOpts(findOwn = findOwn))
			val bobPromptOpt =
				if knownOpt.isDefined then None
				else hypo.common.findPrompt(hypo, bob, reqId, usedOrders, ignore.union(state.hands(bob).filter(!prev.state.deck(_).clued)))
					.filter(!rainbowMismatch(prev, hypo, action, reqId, _))

			val otherConnOpt =
				if knownOpt.isDefined || bobPromptOpt.isDefined then None
				else
					val connCtx = ConnectContext(looksDirect = looksDirect, thinksStall = thinksStall, connected = usedOrders, ignore = ignore)
					(1 until state.numPlayers).map(i => (giver - i + state.numPlayers) % state.numPlayers)
						.filter(_ != bob)
						.findSome(p => findSingleConn(stepCtx, p, reqId, connCtx, ConnectOpts(assumeTruth = true)))

			knownOpt match
				case Some(conn) =>
					val playId = hypo.state.deck(conn.order).id().getOrElse(reqId)
					(advanceHypo(hypo, conn.reacting, conn.order, playId), usedOrders.incl(conn.order), blindCount)
				case None =>
					bobPromptOpt match
						case Some(pOrder) =>
							val playId = hypo.state.deck(pOrder).id().filter(hypo.state.isPlayable).getOrElse(reqId)
							(advanceHypo(hypo, bob, pOrder, playId), usedOrders.incl(pOrder), blindCount)
						case None =>
							otherConnOpt match
								case Some(conns) =>
									val nextHypo = conns.foldLeft(hypo): (acc, c) =>
										val playId = acc.state.deck(c.order).id().orElse(Option.when(c.ids.length == 1)(c.ids.head)).getOrElse(reqId)
										advanceHypo(acc, c.reacting, c.order, playId)
									(nextHypo, usedOrders.union(conns.map(_.order)), blindCount)
								case None =>
									val nextHypo = hypo.withState(_.withPlay(reqId)).elim(skipHypoStacks = true)
									(nextHypo, usedOrders, blindCount + 1)

	if bobBlindPlays < 2 then
		return Nil

	game.findNthFinesse(bob, 2, connected = FastBitSet.single(focus), ignore = ignore).filterNot(resetFinesse) match
		case None => Nil
		case Some(fp2) =>
			val bIdsOpt: Option[List[Identity]] = knownId(fp2) match
				case Some(b) =>
					Option.when(state.isPlayable(b) && !state.isInverted(b))(List(b))
				case None if findOwn.contains(bob) =>
					val playable = game.players(bob).thoughts(fp2).inferred.filter(i => state.isPlayable(i) && !state.isInverted(i))
					Option.when(playable.nonEmpty):
						val commonPlayable = game.common.thoughts(fp2).inferred.filter(i => state.isPlayable(i) && !state.isInverted(i))
						commonPlayable.when(_.isEmpty)(_ => playable).toList
				case _ => None

			bIdsOpt.toList.map: bIds =>
				val conns = List(FinesseConn(bob, fp2, bIds, FinesseKind.Bluff))
				val symmetric = !state.deck(focus).matches(id, assume = receiver == state.ourPlayerIndex) ||
					!game.players(receiver).thoughts(focus).possible.contains(id) ||
					(!prev.isTouched(focus) && visibleFind(state, prev.players(giver), id, infer = true, excludeOrder = focus).exists(prev.isTouched)) ||
					symmetricConns(game, conns) ||
					conns.exists(c => prev.provingTrash(c.order, giver))

				Log.info(s"found 5 color ejection: ${state.logConns(conns, id)}${if symmetric then " (symmetric)" else ""}")
				FocusPossibility(IdentitySet.single(id), conns, ClueInterp.Play, symmetric = symmetric)

def connectDoubleBluff(ctx: ClueContext, id: Identity, looksDirect: Boolean, thinksStall: FastBitSet, ignore: FastBitSet, findOwn: Option[Int]): List[FocusPossibility] =
	val ClueContext(prev, game, action, _) = ctx
	val ClueAction(giver, receiver, _, clue) = action
	val state = game.state
	val FocusResult(focus, _, positional) = ctx.focusResult

	val bob = state.nextPlayerIndex(giver)
	val cathy = state.nextPlayerIndex(bob)

	def thinksClueIsStall(p: Int) =
		thinksStall.contains(p) && (
			giver == state.ourPlayerIndex ||
			state.ourHand.forall(o => state.deck(o).clued || game.me.orderKt(game, o))
		)

	def resetFinesse(o: Int) =
		prev.meta(o).status == CardStatus.Finessed &&
		!prev.common.thoughts(o).reset &&
		game.common.thoughts(o).reset

	def knownId(o: Int): Option[Identity] =
		state.deck(o).id().orElse:
			Option.when(game.future(o).length == 1)(game.future(o).head)

	def hasPromptFor(p: Int, reqId: Identity, connected: FastBitSet) =
		p != receiver &&
		game.common.findPrompt(game, p, reqId, connected, ignore.union(state.hands(p).filter(!prev.state.deck(_).clued)))
			.exists(!rainbowMismatch(prev, game, action, reqId, _))

	val isSingleBluffRank =
		id.rank <= state.playStacks(id.suitIndex) + 2 ||
		isIntermediateBluffTarget(prev, game, action, id, focus)

	val disallowed =
		game.level < Level.DoubleBluffs ||
		state.numPlayers < 3 ||
		state.playStacks(id.suitIndex) + 1 >= id.rank ||
		state.isInverted(id) ||
		positional.contains(Positional.TrashPush) ||
		(game.level >= Level.Ejections && clue.kind == ClueKind.Colour && id.rank == 5 && !prev.state.deck(focus).clued && !prev.isTouched(focus)) ||
		(clue.kind == ClueKind.Colour && (bob == receiver || cathy == receiver)) ||
		(bob == receiver && looksDirect) ||
		(cathy == receiver && isSingleBluffRank) ||
		thinksClueIsStall(bob) ||
		thinksClueIsStall(cathy) ||
		bluffInterferes(game, giver, bob) ||
		bluffInterferes(game, giver, cathy)

	if disallowed then
		return Nil

	val connected = FastBitSet.single(focus)
	val nextId = Identity(id.suitIndex, state.playStacks(id.suitIndex) + 1)
	val checkConnCtx = ConnectContext(looksDirect = looksDirect, thinksStall = thinksStall, connected = connected, ignore = ignore)

	// By Bob's Truth Principle, Bob won't blind-play if nextId is already known, visible in another player's hand, or promptable in Bob's hand.
	val bobWaitsOrPrompts =
		hasPromptFor(bob, nextId, connected) ||
		findKnownConn(ctx, nextId, id, ignore.union(connected), ConnectOpts(findOwn = findOwn)).isDefined ||
		(0 until state.numPlayers).exists: p =>
			p != giver && p != bob &&
			findSingleConn(ctx, p, nextId, checkConnCtx, ConnectOpts(assumeTruth = true)).isDefined

	if bobWaitsOrPrompts then
		return Nil

	val bobFinesse = game.findFinesse(bob, connected = connected, ignore = ignore).filterNot(resetFinesse)
	val cathyFinesse = game.findFinesse(cathy, connected = connected, ignore = ignore).filterNot(resetFinesse)

	(bobFinesse, cathyFinesse) match
		case (Some(bobOrder), Some(cathyOrder)) if bobOrder != cathyOrder =>
			def validPair(b1: Identity, b2: Identity): Boolean =
				state.isPlayable(b1) &&
				!state.isInverted(b1) &&
				!(b1 == nextId && id.rank > nextId.rank + 1 && hasPromptFor(cathy, Identity(id.suitIndex, nextId.rank + 1), connected.incl(bobOrder))) && {
					val s1 = state.withPlay(b1)
					s1.isPlayable(b2) &&
					!state.isInverted(b2) &&
					!s1.withPlay(b2).isBasicTrash(id) &&
					doubleBluffDisconnect(game, clue, b2, focus) &&
					// Must not already resolve after Bob's blind-play alone (unless Bob's play failed single-bluff disconnect, i.e. PDB)
					!(isSingleBluffRank && (b1.next.contains(id) || bluffDisconnect(game, clue, b1, focus)))
				}

			val pairOpt: Option[(List[Identity], List[Identity])] = (knownId(bobOrder), knownId(cathyOrder)) match
				case (Some(b1), Some(b2)) =>
					Option.when(validPair(b1, b2)):
						(List(b1), List(b2))

				case (None, Some(b2)) if findOwn.contains(bob) =>
					val bobThought = game.players(bob).thoughts(bobOrder)
					val b1s = bobThought.inferred.filter(validPair(_, b2))
					Option.when(b1s.nonEmpty):
						val commonB1s = game.common.thoughts(bobOrder).inferred.filter(validPair(_, b2))
						(commonB1s.when(_.isEmpty)(_ => b1s).toList, List(b2))

				case (Some(b1), None) if findOwn.contains(cathy) =>
					val cathyThought = game.players(cathy).thoughts(cathyOrder)
					val b2s = cathyThought.inferred.filter(validPair(b1, _))
					Option.when(b2s.nonEmpty):
						val commonB2s = game.common.thoughts(cathyOrder).inferred.filter(validPair(b1, _))
						(List(b1), commonB2s.when(_.isEmpty)(_ => b2s).toList)

				case _ => None

			pairOpt.toList.map: (b1Ids, b2Ids) =>
				val conns = List(
					FinesseConn(bob, bobOrder, b1Ids, FinesseKind.Bluff),
					FinesseConn(cathy, cathyOrder, b2Ids, FinesseKind.Bluff)
				)
				val symmetric = !state.deck(focus).matches(id, assume = receiver == state.ourPlayerIndex) ||
					!game.players(receiver).thoughts(focus).possible.contains(id) ||
					(!prev.isTouched(focus) && visibleFind(state, prev.players(giver), id, infer = true, excludeOrder = focus).exists(prev.isTouched)) ||
					symmetricConns(game, conns) ||
					conns.exists(c => prev.provingTrash(c.order, giver))

				Log.info(s"found double bluff connections: ${state.logConns(conns, id)}${if symmetric then " (symmetric)" else ""}")
				FocusPossibility(IdentitySet.single(id), conns, ClueInterp.Play, symmetric)

		case _ => Nil

def symmetricConns(game: HGroup, conns: Seq[Connection]): Boolean =
	val state = game.state
	def matchesIds(order: Int, ids: List[Identity]) =
		state.deck(order).id() match
			case Some(i) => ids.contains(i)
			case None => ids.exists(game.future(order).contains)

	conns.exists:
		case conn: PlayableConn =>
			if conn.linked.isEmpty then
				!matchesIds(conn.order, conn.ids)
			else
				conn.linked.forall(!matchesIds(_, conn.ids))
		case conn: FinesseConn if conn.isBluff =>
			// A bluff doesn't have to match the actual id
			false
		case conn =>
			!matchesIds(conn.order, conn.ids)

def findTrashFinesses(ctx: ClueContext, connections: List[Connection], ambiguous: Boolean = false): List[FocusPossibility] =
	val ClueContext(prev, game, action, _) = ctx
	val (common, state) = (game.common, game.state)
	val ClueAction(giver, receiver, _, _) = action
	val focus = ctx.focusResult.focus
	val focusThoughts = common.thoughts(focus)

	if game.level < Level.TrashMoves || isTrashClue(prev, game, action, focus) || !connections.exists(_.isInstanceOf[FinesseConn]) then
		return Nil

	// The first unknown play must come from another player.
	if connections.find(c => c.isInstanceOf[FinesseConn] || c.isInstanceOf[PromptConn]).exists(_.reacting == receiver) then
		return Nil

	var lastPlayer = giver
	var ci = 0
	while ci < connections.length &&
		((ci > 0 && connections(ci - 1).hidden && connections(ci).reacting == lastPlayer) ||
			inBetween(state.numPlayers, connections(ci).reacting, state.nextPlayerIndex(lastPlayer), receiver)) &&
		!prev.state.hands(connections(ci).reacting).exists(prev.isBlindPlaying)
	do
		lastPlayer = connections(ci).reacting
		ci += 1

	// If there are plays before the target, check if the play would prove it trash.
	val playablesRemaining =
		val base = focusThoughts.possible.filter(!state.isBasicTrash(_))
		if ci == 0 then
			base
		else if connections.head.isBluff then
			// If we have a bluff connection, we can only eliminate immediate playables or connecting matching ranks.
			base.filter: id =>
				!state.isPlayable(id) &&
				!connections(ci - 1).ids.forall(cid => cid.suitIndex == id.suitIndex && cid.rank >= id.rank)
		else
			// Otherwise, we can rule out anything that doesn't follow the connection.
			base.filter: id =>
				!connections(ci - 1).ids.forall(cid => cid.suitIndex != id.suitIndex || cid.rank >= id.rank)

	val noConnectingPlay = playablesRemaining.isEmpty
	val tfConnections = finalizeConns(connections, givenByUs = giver == state.ourPlayerIndex)

	def trashWithReverse(revConns: List[Connection]) =
		val revFinesses = revConns.collect { case f: FinesseConn => f }
		focusThoughts.possible.filter: id =>
			state.isBasicTrash(id) ||
			revFinesses.exists(_.ids.contains(id))

	val validReverse = !tfConnections.exists(_.isPossiblyBluff) && {
		val revTrash = trashWithReverse(tfConnections)

		// 2. The target knows their card must either be trash or match a reverse finesse play, or
		val cardMatchesLastPlay = focusThoughts.possible.forall(revTrash.contains)

		// 3. There is only one playable card and we might have it in our hand.
		lazy val playableIds = focusThoughts.inferred.filter: id =>
			!revTrash.contains(id) &&
			!tfConnections.exists(conn => conn.ids.length == 1 && conn.ids.head == id)

		lazy val maybeInOurHand = playableIds.length == 1 &&
			state.isCritical(playableIds.head) &&
			giver != state.ourPlayerIndex &&
			receiver != state.ourPlayerIndex &&
			state.ourHand.exists(o => game.me.thoughts(o).possible.contains(playableIds.head))

		cardMatchesLastPlay || maybeInOurHand
	}

	// Can only reverse trash finesse before end game.
	if !noConnectingPlay && (game.inEndgame || !validReverse) then
		return Nil

	val possibleTrash = trashWithReverse(if !game.inEndgame && validReverse then tfConnections.drop(ci) else Nil)
	if possibleTrash.isEmpty then
		return Nil

	val symmetric = !state.deck(focus).id().forall(possibleTrash.contains) || symmetricConns(game, tfConnections)

	Log.info(s"found possible trash finesse: ${state.logConns(tfConnections)} -> [${possibleTrash.fmt(state)}]${if symmetric then " (symmetric)" else ""}${if ambiguous then " (ambiguous)" else ""}")
	List(FocusPossibility(possibleTrash, tfConnections, ClueInterp.Play, symmetric = symmetric, ambiguous = ambiguous, trashFinesse = true, needsBlindPlay = !validReverse))

def connectTrashFinesses(ctx: ClueContext, looksDirect: Boolean, thinksStall: FastBitSet, savePoss: Seq[FocusPossibility] = Nil, findOwn: Option[Int] = None, preferOwn: Boolean = false): List[FocusPossibility] =
	val ClueContext(prev, game, action, _) = ctx
	val (common, state) = (game.common, game.state)
	val ClueAction(giver, receiver, _, clue) = action
	val FocusResult(focus, _, positional) = ctx.focusResult

	val hasUsefulNewlyCluedNonFocus =
		game.level >= Level.Ejections &&
		receiver != state.ourPlayerIndex &&
		action.orders.exists(o => o != focus && !prev.state.deck(o).clued && state.deck(o).id().exists(!state.isBasicTrash(_)))

	if game.level < Level.TrashMoves ||
		positional.contains(Positional.TrashPush) ||
		isTrashClue(prev, game, action, focus) ||
		hasUsefulNewlyCluedNonFocus ||
		(savePoss.nonEmpty && !common.thoughts(focus).possible.exists(state.isBasicTrash))
	then
		return Nil

	val targetIds = (0 until state.variant.suits.length).toList.flatMap: suitIndex =>
		val rank = if clue.kind == ClueKind.Rank then clue.value else state.playStacks(suitIndex) + 1
		Option.when(
			rank <= state.maxRanks(suitIndex) &&
			state.playStacks(suitIndex) < rank &&
			state.variant.idTouched(Identity(suitIndex, rank), clue) &&
			!savePoss.exists(_.ids.contains(Identity(suitIndex, rank)))
		):
			Identity(suitIndex, rank)

	if targetIds.isEmpty then
		return Nil

	if findOwn.isEmpty && !preferOwn then
		val bob = state.nextPlayerIndex(giver)
		val bobMatchesTarget = game.findFinesse(bob, FastBitSet.single(focus), state.heldOrders.filter(prev.provingTrash(_, giver)))
			.flatMap(state.deck(_).id())
			.exists(targetIds.contains)
		var foundBluff = false

		targetIds.flatMap: id =>
			val fps = connect(ctx, id, looksDirect, thinksStall, assumeTruth = bobMatchesTarget || foundBluff, forTrashFinesse = true)
			if fps.exists(_.isBluff) then foundBluff = true
			fps
		.filterNot: fp =>
			foundBluff && fp.connections.headOption.exists(c => c.reacting == bob && c.hidden)
	else if giver != state.ourPlayerIndex && receiver != state.ourPlayerIndex && state.deck(focus).id().exists(state.isBasicTrash) && common.thoughts(focus).possible.exists(!state.isBasicTrash(_)) then
		game.findFinesse(state.ourPlayerIndex) match
			case None => Nil
			case Some(ownFinesse) =>
				val possibleBluff = !preferOwn &&
					state.nextPlayerIndex(giver) == state.ourPlayerIndex &&
					!prev.isBlindPlaying(ownFinesse) &&
					!common.thoughts(focus).possible.exists(id => !state.isBasicTrash(id) && !state.isPlayable(id))

				if possibleBluff then
					val bluffableIds = common.thoughts(ownFinesse).possible.filter(state.isPlayable)
					val trashIds = common.thoughts(focus).possible.filter(state.isBasicTrash)
					if bluffableIds.nonEmpty then
						val conn = FinesseConn(state.ourPlayerIndex, ownFinesse, bluffableIds.toList, FinesseKind.Bluff)
						List(FocusPossibility(trashIds, List(conn), ClueInterp.Play, trashFinesse = true, needsBlindPlay = true))
					else
						Nil
				else
					val unconfirmedTrash = state.heldOrders.filter(prev.provingTrash(_, giver, findOwn)).incl(focus)
					val finesseIds = game.me.thoughts(ownFinesse).inferred.filter: id =>
						targetIds.contains(id) &&
						state.isPlayable(id) &&
						!state.isInverted(id) &&
						findKnownConn(ctx, id, id, unconfirmedTrash, ConnectOpts()).isEmpty
					if finesseIds.nonEmpty then
						findTrashFinesses(ctx, List(FinesseConn(state.ourPlayerIndex, ownFinesse, finesseIds.toList, FinesseKind.True)), ambiguous = preferOwn)
					else
						Nil
	else
		Nil

/** Corrects 'possibly bluff' connections to true finesses if necessary. */
def finalizeConns(conns: List[Connection], givenByUs: Boolean = false) =
	if conns.isEmpty then conns else
		val newHead = conns.head match
			case f: FinesseConn if f.possiblyBluff =>
				if conns.count(_.isInstanceOf[FinesseConn]) > 1 then
					Log.info("rewriting to true finesse")
					f.copy(fKind = FinesseKind.True)
				else if givenByUs then
					f.copy(fKind = FinesseKind.Bluff)
				else
					f

			case x => x

		newHead +: conns.tail

/** Searches for Level 16 Unknown Trash Discharge (UTD) and Unknown Dupe Discharge (UDD) connections on Bob's 3rd finesse position.
  * @return `(actualDischarge, fps)` where `actualDischarge` is true when `receiver != state.ourPlayerIndex` and the clue visibly requires a 3FP discharge.
  */
def connectDischarge(ctx: ClueContext, rawTrashFocusPoss: Seq[FocusPossibility], thinksStall: FastBitSet, findOwn: Option[Int] = None): (Boolean, List[FocusPossibility]) =
	val ClueContext(prev, game, action, _) = ctx
	val (common, state) = (game.common, game.state)
	val ClueAction(giver, receiver, orders, _) = action
	val FocusResult(focus, _, positional) = ctx.focusResult

	val bob = state.nextPlayerIndex(giver)

	def thinksClueIsStall(p: Int) =
		thinksStall.contains(p) && (
			giver == state.ourPlayerIndex ||
			state.ourHand.forall(o => state.deck(o).clued || game.me.orderKt(game, o))
		)

	val baseEligible =
		game.level >= Level.Ejections &&
		state.numPlayers >= 3 &&
		bob != receiver &&
		!prev.state.deck(focus).clued &&
		positional.isEmpty &&
		!isTrashClue(prev, game, action, focus)

	if !baseEligible then
		return (false, Nil)

	val focusThought = common.thoughts(focus)

	val hasUsefulNewlyCluedNonFocus =
		if receiver != state.ourPlayerIndex then
			orders.exists(o => o != focus && !prev.state.deck(o).clued && state.deck(o).id().exists(!state.isBasicTrash(_)))
		else
			orders.exists(o => o != focus && !prev.state.deck(o).clued && common.thoughts(o).possible.exists(!state.isBasicTrash(_)))

	val hasUnplayableUsefulInEmpathy =
		focusThought.possible.exists(i => !state.isBasicTrash(i) && !state.isPlayable(i)) &&
		!rawTrashFocusPoss.exists(fp => !fp.isBluff && (!fp.symmetric || receiver == state.ourPlayerIndex))

	val uddAllowed =
		!game.inEndgame &&
		thinksStall.isEmpty

	val actualDischarge = receiver != state.ourPlayerIndex && !thinksClueIsStall(bob) && state.deck(focus).id().exists: focusId =>
		!state.isInverted(focusId) && (
			(state.isBasicTrash(focusId) && (hasUsefulNewlyCluedNonFocus || hasUnplayableUsefulInEmpathy)) || (
				!state.isBasicTrash(focusId) &&
				uddAllowed &&
				!(state.isPlayable(focusId) && (prev.state.clueTokens == 8 || prev.dda.isDefined)) &&
				state.hands(receiver).exists(o => o != focus && game.isTouched(o) && state.deck(o).matches(focusId))
			)
		)

	if (receiver != state.ourPlayerIndex && !actualDischarge) || thinksClueIsStall(bob) || bluffInterferes(game, giver, bob) then
		return (actualDischarge, Nil)

	def resetFinesse(o: Int) =
		prev.meta(o).status == CardStatus.Finessed &&
		!prev.common.thoughts(o).reset &&
		game.common.thoughts(o).reset

	def knownId(o: Int): Option[Identity] =
		state.deck(o).id().orElse:
			Option.when(game.future(o).length == 1)(game.future(o).head)

	val unconfirmedTrash = state.heldOrders.filter(prev.provingTrash(_, giver, findOwn))

	game.findNthFinesse(bob, 3, connected = FastBitSet.single(focus), ignore = unconfirmedTrash).filterNot(resetFinesse) match
		case None => (actualDischarge, Nil)
		case Some(fp3) =>
			val bIdsOpt: Option[List[Identity]] = knownId(fp3) match
				case Some(b) =>
					Option.when(state.isPlayable(b) && !state.isInverted(b))(List(b))
				case None if findOwn.contains(bob) =>
					val playable = game.players(bob).thoughts(fp3).inferred.filter(i => state.isPlayable(i) && !state.isInverted(i))
					Option.when(playable.nonEmpty):
						val commonPlayable = common.thoughts(fp3).inferred.filter(i => state.isPlayable(i) && !state.isInverted(i))
						commonPlayable.when(_.isEmpty)(_ => playable).toList
				case _ => None

			bIdsOpt match
				case None => (actualDischarge, Nil)
				case Some(bIds) =>
					val utdIds =
						if hasUsefulNewlyCluedNonFocus || hasUnplayableUsefulInEmpathy then
							focusThought.possible.filter(i => state.isBasicTrash(i) && !state.isInverted(i))
						else
							IdentitySet.empty

					val uddIds =
						if !uddAllowed then IdentitySet.empty
						else focusThought.possible.filter: id =>
							!state.isBasicTrash(id) &&
							!state.isCritical(id) &&
							!state.isInverted(id) &&
							!(state.isPlayable(id) && (prev.state.clueTokens == 8 || prev.dda.isDefined)) &&
							state.hands(receiver).exists: o =>
								o != focus &&
								game.isTouched(o) &&
								common.thoughts(o).possible.contains(id)
							&& {
								val knownOutsideTarget = (0 until state.numPlayers).summing: p =>
									if p == receiver then 0
									else state.hands(p).count(o => common.thoughts(o).matches(id))
								state.cardCount(id.toOrd) - state.baseCount(id.toOrd) - knownOutsideTarget >= 2
							}

					val dischargeIds = utdIds.union(uddIds)
					val conns = List(FinesseConn(bob, fp3, bIds, FinesseKind.Bluff))

					if dischargeIds.isEmpty then
						(actualDischarge, Nil)
					else
						val symmetric = !state.deck(focus).id().forall(dischargeIds.contains) ||
							game.players(receiver).thoughts(focus).possible.intersect(dischargeIds).isEmpty ||
							symmetricConns(game, conns) ||
							conns.exists(c => prev.provingTrash(c.order, giver))

						Log.info(s"found discharge connections: ${state.logConns(conns)} -> [${dischargeIds.fmt(state)}]${if symmetric then " (symmetric)" else ""}")
						(actualDischarge, List(FocusPossibility(dischargeIds, conns, ClueInterp.Play, symmetric = symmetric, discharge = true)))
