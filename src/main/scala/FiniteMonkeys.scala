import akka.actor.{ ActorRef, ActorSystem, Props, Actor, Inbox }
import scala.concurrent.duration._
import scala.util.Random
import scala.math._

case class Word(word: String)
case class Feedback(word: String, score: Double)
case object Work
case object Retire




class Monkey extends Actor {
  
  Random.setSeed(Random.nextLong)
  
  var feedback: Array[Set[String]] = Array.fill(101)(Set.empty)

  val guessWord = "......................................" // word used to calculate the guessed word length

  var wordLength = 0 // length of the word Monkey is guessing

  def wordParts = (wordLength / 2d).toInt - 1 + Random.nextInt(2)

  // using something like bigrams (not completely bigrams) - it is easier for monkey to type bigram keyboard to construct the word - higher probability of composing a word. Using limited number of bigrams
  val s = for(c <- List("a", "o", "e", "i", "u" ,"y"); c1 <- List("b","c","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","w","x","z")) yield (c+c1)
  val s1 = for(c <- List("a", "o", "e", "i", "u" ,"y"); c1 <- List("b","c","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","w","x","z")) yield (c1+c)
  val chars: Set[String] = (s ::: s1).toSet 

  def wordExistInPastFeedback(word: String) = feedback.exists(_.exists(_ == word))

  def feedbackToStream(s: Set[String], i: Int): Stream[(String, Int)] = {
    if (i != 0)
      (s.toStream zip Stream.fill(s.size)(i)) #::: feedbackToStream(feedback(i - 1), i - 1)
    else
      Stream.empty
  }

  def score(word1: String, word2: String): Double = {
    1 - Levenshtein.distance(word1, word2).toDouble / word2.length
  }

  def randomWord = (for (i <- (0 to wordParts)) yield chars.toVector(Random.nextInt(chars.size - 1))).mkString("")

  def generateWord(): String = {

    val topWords: Stream[(String, Int)] = feedbackToStream(feedback(100), 100)

    def getAnotherWord(topWords: List[(String, Int)]): String = {

      val generatedWord = randomWord

      // compare if the generated word is worst than to 10 known words then generate another one
      val keepGoing = topWords.exists(w => (score(w._1, generatedWord) * 100 < w._2 || wordExistInPastFeedback(generatedWord)))

      // println("keepgoing: " + keepGoing + " generatedWord: "+ generatedWord)

      if (keepGoing)
        getAnotherWord(topWords)
      else
        generatedWord

    }
    println(topWords.take(10).toList)
    getAnotherWord(topWords.take(10).toList)
  }

  def receive = {

    case Feedback(word, score) => {

      if (wordLength == 0) { wordLength = (guessWord.length() / (-score) - 1).toInt } // Smart monkey guessed the length 
      if (score > 0) {

        val scoreInt = (score * 100).toInt

        // learn form feedback
        feedback(scoreInt) = feedback(scoreInt) + word
        
      }
      sender ! Word(generateWord())
    }

    case Retire => {
      println("Monkey worked hard and retyring now... zzzz...zzzz...");
      context.stop(self)
    }

    case Work => {
      if (wordLength == 0)
        sender ! Word(guessWord) // use this word to determine the word length as we know none of the chars will match and able to calculate it from the score in feedback

    }
  }
}




class MonkeySupervisor(word: String) extends Actor {

  var monkeys: List[ActorRef] = (for (i <- (0 until 1)) yield context.actorOf(Props(new Monkey()), "monkey" + i)).toList

  def distance(word: String): Double = {
    1 - Levenshtein.distance(this.word, word).toDouble / this.word.length
  }

  def generatePoem(poet: ActorRef) = {
    println("Monkey supervisor at work")
    monkeys.foreach(_ ! Work)
  }

  def receive = {
    case Work => generatePoem(sender)
    case Word(word) => {
      val d = distance(word)
      if (d > 0.5d) println("Distance: " + d + " word: " + this.word + " word1: " + word.toString);
      if (d > 0.9d) {
        println("Distance: " + d + " word: " + this.word + " word1: " + word.toString);
        monkeys.foreach(_ ! Retire)
        context.stop(self)
      } else
        sender ! Feedback(word, d)
    }
  }
}




class Poet(poem: String) extends Actor {

  var monkeySupervisors: List[ActorRef] = (for (i <- (0 until poem.split(" ").length)) yield context.actorOf(Props(new MonkeySupervisor(poem.split(" ")(i))), "monkeySupervisor" + i)).toList

  def needInspiration() = {
    println("Poet at work")
    monkeySupervisors.foreach(_ ! Work)
  }

  def receive = {

    case Work => needInspiration()
  }
}

object FiniteMonkeys extends App {

  val system = ActorSystem("MonkeyNetwork")

  val shakespeare = system.actorOf(Props(new Poet("To banana or not to banana")), "Shakespeare")

  shakespeare ! Work

}




object Levenshtein {
  def minimum(i1: Int, i2: Int, i3: Int) = min(min(i1, i2), i3)
  def distance(s1: String, s2: String) = {
    val dist = Array.tabulate(s2.length + 1, s1.length + 1) { (j, i) => if (j == 0) i else if (i == 0) j else 0 }

    for (j <- 1 to s2.length; i <- 1 to s1.length)
      dist(j)(i) = if (s2(j - 1) == s1(i - 1)) dist(j - 1)(i - 1)
      else minimum(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)

    dist(s2.length)(s1.length)
  }
}
