package uk.co.hadoopathome.adventofcode18.day2

object DiffFinder {
    def diffFinder(ls: List[List[Char]]): List[Char] = {
        diffFinderRec(ls, ls).getOrElse(Nil)
    }

    def compareWords(word1: List[Char], word2: List[Char]): Option[List[Char]] = {
        val wordLength = word1.length
        val filtered = word1.zip(word2).filter(x => x._1 == x._2)
        if (filtered.length == wordLength-1) {
            Option(filtered.map(x => x._1))
        } else {
            None
        }
    }

    def diffFinderRec(sublist: List[List[Char]], ls: List[List[Char]]): Option[List[Char]] = {
        ls match {
            case x :: y :: _ if compareWords(x, y).isDefined => compareWords(x, y)
            case x :: _ :: xx => diffFinderRec(sublist, x :: xx)
            case _ :: Nil => diffFinderRec(sublist.tail, sublist.tail)
        }
    }
}
