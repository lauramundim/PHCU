package ag

import scala.collection.immutable.SortedSet
import scala.math._

abstract class População[Cromossomo,
                         Ind <: Indivíduo[Cromossomo, Ind],
                         P <: População[Cromossomo, Ind, P]
] {
  // FUNÇÃO: mantém uma coleção de indivíduos

  val indivíduos:SortedSet[Ind]

  lazy val apt:Vector[Int] = {
    val v = new Array[Int](indivíduos.size)
    var i = 0
    for (ind <- indivíduos) { 
      v(i) = ind.apt
      i += 1
    }
    v.toVector
  }

  // EFEITOS: retorna a aptidão do melhor indivíduo da população
  lazy val melhorAptidão:Int = apt.head

  // EFEITOS: retorna a aptidão do pior indivíduo da população
  lazy val piorAptidão:Int = apt.last

  // EFEITOS: retorna a aptidão média dos indivíduos da população
  //lazy val médiaAptidão:Double = 
    //BigDecimal(apt.sum.toDouble / apt.size).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
   lazy val médiaAptidão:Double = 
     apt.foldLeft (0.0)(_.toDouble + _) / apt.length
  
  // EFEITOS: retorna o desvio padrão da aptidão dos indivíduos
  //lazy val desvioAptidão:Double = 
  //BigDecimal(sqrt((apt.map(x => x-médiaAptidão).map(y => y*y)).sum.toDouble/apt.size)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
lazy val desvioAptidão:Double = {
     val soma = apt.foldLeft (0.0) { (s,x) => 
           val desv = x - médiaAptidão; 
           s + desv*desv  }
     Math.sqrt( soma / apt.length)
  }
  def replica(inds: SortedSet[Ind]):P
    
  // EFEITOS: retorna o indivíduo com a melhor aptidão da população em this.
  lazy val melhorIndivíduo:Ind = indivíduos.head

}

