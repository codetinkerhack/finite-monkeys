import scala.util.Random


object test {

    
 

   
 //      distance("ot", "to")

	//	val a = ("test" zip "best"  zip "blah" zip "belt")
    
    //val list = List(List("abc","efg"), List("hij","klm"), List("nop","qrs"))
		//list.flatten
    //(for(s <- a; c <- a) yield c.toString)
    //a.flatten
    val generatedWord = "inghtehno"               //> generatedWord  : String = inghtehno
    
    val topWords = List(("enna",50), ("bebani",50), ("ingna",50), ("bathce",33), ("inno",33), ("tena",33), ("ghtba",16), ("veba",16), ("ince",16), ("innotba",16))
                                                  //> topWords  : List[(String, Int)] = List((enna,50), (bebani,50), (ingna,50), (
                                                  //| bathce,33), (inno,33), (tena,33), (ghtba,16), (veba,16), (ince,16), (innotba
                                                  //| ,16))
    
    val chars: Set[String] = Set("th", "ou", "ght", "is", "ve", "ry", "in", "te", "re", "st", "ing", "ni", "ce", "to", "ba", "be", "na", "on", "en", "eh", "or", "no", "not") // (for(c <- ('a' to 'z'); c2 <- ('a' to 'z') if(c != c2)) yield (""+c+c2)).toSet
                                                  //> chars  : Set[String] = Set(st, ba, in, is, ght, re, ing, or, to, th, ce, on,
                                                  //|  ni, ve, en, not, na, ry, te, be, eh, no, ou)

 
  def randomWord = (for (i <- (0 to 2)) yield chars.toVector(Random.nextInt(chars.size-1))).mkString("")
                                                  //> randomWord: => String
    
	val keepGoing = topWords.exists(w => ((Levenshtein.distance(generatedWord, w._1) * 100).toInt < w._2 ))
                                                  //> keepGoing  : Boolean = false
    
   def getAnotherWord(topWords: List[(String, Int)]): String = {

      val generatedWord = randomWord

      // compare if the generated word is worst than to 10 known words then generate another one
      val keepGoing = topWords.exists(w => (((1 - Levenshtein.distance(w._1, generatedWord) * 100) / w._1.length.toDouble ).toInt < w._2 ))
      
      println("keepgoing: " + keepGoing + " generatedWord: "+ generatedWord);
      
      if (keepGoing)
        getAnotherWord(topWords)
      else
        generatedWord

    }                                             //> getAnotherWord: (topWords: List[(String, Int)])String
    
 //  getAnotherWord(topWords)
    
    1 - Levenshtein.distance("na", "enna") / "enna".length.toDouble
                                                  //> res0: Double = 0.5
     
	//	randomWord
	  val s = for(c <- List("a", "o", "e", "i", "u" ,"y"); c1 <- List("b","c","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","w","x","z")) yield (c+c1)
                                                  //> s  : List[String] = List(ab, ac, ad, af, ag, ah, aj, ak, al, am, an, ap, aq
                                                  //| , ar, as, at, av, aw, ax, az, ob, oc, od, of, og, oh, oj, ok, ol, om, on, o
                                                  //| p, oq, or, os, ot, ov, ow, ox, oz, eb, ec, ed, ef, eg, eh, ej, ek, el, em, 
                                                  //| en, ep, eq, er, es, et, ev, ew, ex, ez, ib, ic, id, if, ig, ih, ij, ik, il,
                                                  //|  im, in, ip, iq, ir, is, it, iv, iw, ix, iz, ub, uc, ud, uf, ug, uh, uj, uk
                                                  //| , ul, um, un, up, uq, ur, us, ut, uv, uw, ux, uz, yb, yc, yd, yf, yg, yh, y
                                                  //| j, yk, yl, ym, yn, yp, yq, yr, ys, yt, yv, yw, yx, yz)
  val s1 = for(c <- List("a", "o", "e", "i", "u" ,"y"); c1 <- List("b","c","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","w","x","z")) yield (c1+c)
                                                  //> s1  : List[String] = List(ba, ca, da, fa, ga, ha, ja, ka, la, ma, na, pa, q
                                                  //| a, ra, sa, ta, va, wa, xa, za, bo, co, do, fo, go, ho, jo, ko, lo, mo, no, 
                                                  //| po, qo, ro, so, to, vo, wo, xo, zo, be, ce, de, fe, ge, he, je, ke, le, me,
                                                  //|  ne, pe, qe, re, se, te, ve, we, xe, ze, bi, ci, di, fi, gi, hi, ji, ki, li
                                                  //| , mi, ni, pi, qi, ri, si, ti, vi, wi, xi, zi, bu, cu, du, fu, gu, hu, ju, k
                                                  //| u, lu, mu, nu, pu, qu, ru, su, tu, vu, wu, xu, zu, by, cy, dy, fy, gy, hy, 
                                                  //| jy, ky, ly, my, ny, py, qy, ry, sy, ty, vy, wy, xy, zy)
  val char: Set[String] = (s ::: s1).toSet        //> char  : Set[String] = Set(yz, ze, hy, se, ku, jo, yp, le, yk, ec, ol, iz, d
                                                  //| u, wi, aq, uv, ip, ba, in, ta, ez, ka, ar, pi, ty, is, ho, oh, ev, ug, fe, 
                                                  //| dy, am, xe, av, uz, ib, yh, tu, iw, uk, co, ot, ed, ys, zi, si, li, nu, ky,
                                                  //|  if, pa, ga, yd, up, so, ij, ep, yw, ov, do, zu, eb, yj, lo, ny, re, cu, om
                                                  //| , ke, ul, yt, us, el, ef, it, ew, su, ca, wy, ju, gy, os, qa, vi, ud, la, u
                                                  //| h, ab, xo, as, ap, uw, ej, di, vu, bi, jy, ko, or, ge, pe, aw, od, ic, ut, 
                                                  //| oc, al, et, af, im, de, ma, va, zy, oz, qy, fu, ha, ri, cy, ki, xy, to, ig,
                                                  //|  yx, ix, ah, sy, je, ur, oj, eg, em, um, fy, qe, uc, ru, at, go, id, ce, ra
                                                  //| , ex, bu, ek, on, ni, vy, ac, da, my, ux, wo, qo, gi, po, yn, zo, me, ob, v
                                                  //| e, ag, wu, by, en, il, gu, he, mo, yb, ji, un, na, az, wa, qi, es, ak, fo, 
                                                  //| oq, ry, ax, og, ih, hi, ci, yf, vo, te, ad, an, be, qu, pu, ok, ub, py, lu,
                                                  //|  uf, yq, mi, xi, ja, ir, ox, eh, yl, iq, sa, za, fi, bo, ro, ym, ne, yr, uj
                                                  //| , iv, xu, no, we, mu, l
                                                  //| Output exceeds cutoff limit.
}