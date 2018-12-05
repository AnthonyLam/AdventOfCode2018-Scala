object Day5 {
    val input = scala.io.Source.fromFile("input5.txt")
    println(input.isTraversableAgain)

    def reactIterator(stuff: Iterator[Char]): Int = {
        var stack = scala.collection.mutable.ArrayStack[Char]()
        stuff.foreach(char => 
            if (!stack.isEmpty && scala.math.abs(stack.head.toInt - char.toInt) == 32) {
                stack.pop()
            }
            else {
               stack.push(char)
            }
        )
        stack.length
    }

    def solutionPartOne = {
        reactIterator(input.reset())
    }

    def solutionPartTwo = {
        ('a' to 'z').map(removeChar => reactIterator(input.reset().filter(_.toLower != removeChar))).min
    }
}