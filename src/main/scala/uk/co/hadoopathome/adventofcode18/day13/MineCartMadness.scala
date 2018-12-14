package uk.co.hadoopathome.adventofcode18.day13

object MineCartMadness {
    def runCarts(board: List[Track], carts: List[Cart]): Coord = {
        val cartsAtCollision = moveCartsIter(board, carts)
        findCollision(cartsAtCollision)
    }

    def runCartsRemoveCrashed(board: List[Track], carts: List[Cart]): Coord = {
        val lastCart = moveCartsRemoveCrashedIter(board, carts)
        lastCart.head.position
    }

    def moveCartsIter(board: List[Track], carts: List[Cart]): List[Cart] = {
        val sortedCarts = carts.sortBy(c => (c.position._2, c.position._1))
        var movedCarts = List[Cart]()
        var originalCarts = sortedCarts

        for (i <- sortedCarts.indices) {
            val movedCart = moveCart(sortedCarts(i), board)
            movedCarts = movedCarts :+ movedCart
            originalCarts = originalCarts.filter(_ != sortedCarts(i))
            val currentCarts = originalCarts ::: movedCarts

            if (hasCollided(currentCarts)) {
                return currentCarts
            }
        }
        moveCartsIter(board, movedCarts)
    }

    def moveCartsRemoveCrashedIter(board: List[Track], carts: List[Cart]): List[Cart] = {
        val sortedCarts = carts.sortBy(c => (c.position._2, c.position._1))
        var movedCarts = List[Cart]()
        var collidedCarts = List[Cart]()
        var originalCarts = sortedCarts

        for (i <- sortedCarts.indices) {
            val currentCart = sortedCarts(i)
            if (!collidedCarts.contains(currentCart)) {
                val movedCart = moveCart(currentCart, board)
                movedCarts = movedCarts :+ movedCart
                originalCarts = originalCarts.filter(_ != sortedCarts(i))
                val currentCarts = originalCarts ::: movedCarts

                if (hasCollided(currentCarts)) {
                    val collisionCoord = findCollision(currentCarts)
                    collidedCarts = collidedCarts ::: currentCarts.filter(_.position == collisionCoord)
                    movedCarts = movedCarts.filter(_.position != collisionCoord) //it's around here....
                }
            }
        }

        if (movedCarts.size == 1) {
            movedCarts
        } else {
            moveCartsRemoveCrashedIter(board, movedCarts)
        }
    }

    def moveCart(cart: Cart, board: List[Track]): Cart = {
        val newPosition = addCoords(cart.position, cart.direction.movement)
        val newPiece = board.filter(t => t._1 == newPosition).head._2
        val newDirection = setDirection(cart, newPiece)
        val newTurn = if (newPiece == '+') cart.turn.nextTurn else cart.turn
        Cart(newPosition, newDirection, newTurn)
    }

    def addCoords(a: Coord, b: Coord): Coord = (a._1 + b._1, a._2 + b._2)

    def setDirection(cart: Cart, newPiece: Char): Direction = {
        val oldDirection = cart.direction
        val newDirection = newPiece match {
            case x if x == '-' => oldDirection
            case x if x == '|' => oldDirection
            case x if x == '\\' && oldDirection == DOWN => RIGHT
            case x if x == '\\' && oldDirection == LEFT => UP
            case x if x == '\\' && oldDirection == RIGHT => DOWN
            case x if x == '\\' && oldDirection == UP => LEFT
            case x if x == '/' && oldDirection == UP => RIGHT
            case x if x == '/' && oldDirection == LEFT => DOWN
            case x if x == '/' && oldDirection == RIGHT => UP
            case x if x == '/' && oldDirection == DOWN => LEFT
            case x if x == '+' => turnCart(cart.turn, oldDirection)
            case _ => oldDirection
        }
        newDirection
    }

    def turnCart(turn: Turn, direction: Direction): Direction = {
        val positions = LEFT :: UP :: RIGHT :: DOWN :: Nil
        val newDirection = turn match {
            case x if x == TURN_LEFT => positions((positions.indexOf(direction) + 3) % 4)
            case x if x == TURN_STRAIGHT => direction
            case x if x == TURN_RIGHT => positions((positions.indexOf(direction) + 1) % 4)
        }
        newDirection
    }

    def hasCollided(carts: List[Cart]): Boolean = carts.map(_.position).toSet.size != carts.size

    def findCollision(carts: List[Cart]): Coord = {
        val grouped = carts.map(_.position).groupBy(identity).mapValues(_.size)
        grouped.maxBy(_._2)._1
    }
}
