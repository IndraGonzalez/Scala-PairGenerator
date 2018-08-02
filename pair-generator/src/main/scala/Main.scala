object Main extends App {
	val people = List("tm1","tm2","tm3","tm4")
	printPairs(randomizePairs(getAllPairs(people,people,List.empty)))

	def getAllPairs(people: List[String], peopleInmutable: List[String], pairs: List[(String,String)]): List[(String,String)] = {
	  if(people.isEmpty) pairs
	  else {
	    getAllPairs(people.tail,peopleInmutable,findPair(people.head,peopleInmutable,pairs))
	  }
	}

	def findPair(person:String, people: List[String], pairs: List[(String,String)]): List[(String,String)] = {
	  if(people.isEmpty) pairs
	  else {
	    if (!pairs.contains((person,people.head)) && !pairs.contains((people.head,person)) && person != people.head) {
	      val newPairs = pairs :+ (person, people.head)
	      findPair(person, people.tail, newPairs)
	    } else
	      findPair(person, people.tail, pairs)
	  }
	}

	def printPairs(pairs: List[(String,String)]) {
	  println("----------------------------------------------------");
	  println("Number of different pairs: " + pairs.length);
	  for (pair <- pairs)
	    println("Pair: " + pair._1 + " and " + pair._2)
	  println("----------------------------------------------------");
	}

	def randomizePairs(pairs: List[(String,String)]) : List[(String,String)] = {
	  scala.util.Random.shuffle(pairs)
	}
}
