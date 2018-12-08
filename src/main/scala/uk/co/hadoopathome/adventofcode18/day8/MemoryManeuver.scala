package uk.co.hadoopathome.adventofcode18.day8

object MemoryManeuver {
    def sumMetadata(ls: List[Int]): Int = {
        val root = Node(1, 0, List(), List())
        populateTree(ls, root)
        sumMetadataRec(root.children.head, 0)
    }

    def populateTree(ls: List[Int], parent: Node): List[Int] = {
        val numChildren = ls.head
        val numMetadata = ls.tail.head
        val node = Node(numChildren, numMetadata, List(), List())
        parent.children = parent.children :+ node

        var consumedList = ls.drop(2)
        for (_ <- 0 until numChildren) {
            consumedList = populateTree(consumedList, node)
        }
        for (i <- 0 until numMetadata) node.metadata = node.metadata :+ consumedList(i)
        consumedList.drop(numMetadata)
    }

    def sumMetadataRec(node: Node, sumMetadata: Int): Int = {
        var sum = sumMetadata + node.metadata.sum
        for (i <- 0 until node.numChildren) {
            sum = sumMetadataRec(node.children(i), sum)
        }
        sum
    }

    case class Node(numChildren: Int, numMetadata: Int, var children: List[Node], var metadata: List[Int])
}
