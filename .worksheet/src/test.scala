import scala.util.Random


object test {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(337); 

    
 

   
 //      distance("ot", "to")

	//	val a = ("test" zip "best"  zip "blah" zip "belt")
    
    //val list = List(List("abc","efg"), List("hij","klm"), List("nop","qrs"))
		//list.flatten
    //(for(s <- a; c <- a) yield c.toString)
    //a.flatten
    val generatedWord = "inghtehno";System.out.println("""generatedWord  : String = """ + $show(generatedWord ));$skip(168); 
    
    val topWords = List(("enna",50), ("bebani",50), ("ingna",50), ("bathce",33), ("inno",33), ("tena",33), ("ghtba",16), ("veba",16), ("ince",16), ("innotba",16));System.out.println("""topWords  : List[(String, Int)] = """ + $show(topWords ));$skip(261); 
    
    val chars: Set[String] = Set("th", "ou", "ght", "is", "ve", "ry", "in", "te", "re", "st", "ing", "ni", "ce", "to", "ba", "be", "na", "on", "en", "eh", "or", "no", "not");System.out.println("""chars  : Set[String] = """ + $show(chars ));$skip(108);  // (for(c <- ('a' to 'z'); c2 <- ('a' to 'z') if(c != c2)) yield (""+c+c2)).toSet

 
  def randomWord = (for (i <- (0 to 2)) yield chars.toVector(Random.nextInt(chars.size-1))).mkString("");System.out.println("""randomWord: => String""");$skip(110); 
    
	val keepGoing = topWords.exists(w => ((Levenshtein.distance(generatedWord, w._1) * 100).toInt < w._2 ));System.out.println("""keepGoing  : Boolean = """ + $show(keepGoing ));$skip(532); 
    
   def getAnotherWord(topWords: List[(String, Int)]): String = {

      val generatedWord = randomWord

      // compare if the generated word is worst than to 10 known words then generate another one
      val keepGoing = topWords.exists(w => (((1 - Levenshtein.distance(w._1, generatedWord) * 100) / w._1.length.toDouble ).toInt < w._2 ))
      
      println("keepgoing: " + keepGoing + " generatedWord: "+ generatedWord);
      
      if (keepGoing)
        getAnotherWord(topWords)
      else
        generatedWord

    };System.out.println("""getAnotherWord: (topWords: List[(String, Int)])String""");$skip(108); val res$0 = 
    
 //  getAnotherWord(topWords)
    
    1 - Levenshtein.distance("na", "enna") / "enna".length.toDouble;System.out.println("""res0: Double = """ + $show(res$0));$skip(183); 
     
	//	randomWord
	  val s = for(c <- List("a", "o", "e", "i", "u" ,"y"); c1 <- List("b","c","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","w","x","z")) yield (c+c1);System.out.println("""s  : List[String] = """ + $show(s ));$skip(162); 
  val s1 = for(c <- List("a", "o", "e", "i", "u" ,"y"); c1 <- List("b","c","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","w","x","z")) yield (c1+c);System.out.println("""s1  : List[String] = """ + $show(s1 ));$skip(43); 
  val char: Set[String] = (s ::: s1).toSet;System.out.println("""char  : Set[String] = """ + $show(char ))}
}
