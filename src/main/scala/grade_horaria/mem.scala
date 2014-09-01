package grade_horaria

import collection.mutable.{Map => MMap}

import grade_horaria._

import ag.{Indivíduo, População}

class MEM[Ind <: Indivíduo[Cromossomo, Ind],
          P <: População[Cromossomo,Ind,P]](alfa:Double) {

  def empty:TMEM = Map[Int, List[Gene]]()

  def constroi(pop:P):TMEM = {
    val mmem = MMap[Int, List[Gene]]()
    val inds =  pop.indivíduos
    // Seleciona os melhores alfa * |pop| indivíduos da população
    val q = inds.take((alfa * inds.size).toInt)
      q foreach { ind => // para cada indivíduo 
        ind.cromossomo.indices foreach { e => // para cada evento e
          // calcula a penalidade para o evento e de ind
          // se e é factível, ou seja, e possui penalidade 0,
          if (ind.fapt.avaliaEvento(e, ind.cromossomo) == 0) {
            // então adicione o par sala-horário (r_i, t_i) atribuído a
            // e na lista l_i
            if (mmem contains e) {
              if (!(mmem(e) contains ind.cromossomo(e)))
                mmem(e) = ind.cromossomo(e) :: mmem(e)
            } else
              mmem(e) = List(ind.cromossomo(e))

      //      println("MEM(" + e + ") = " + mmem(e))
          }
        }
      }

    //println("MEM")
    //println("   número de eventos: " + mmem.keys.size)
    val entradas = mmem.values map (_.size)
    val media = entradas.sum.toDouble / entradas.size
    //println("   número médio de genes por entrada: " + media)
    //println("   comprimento da maior entrada: " + entradas.max)
    //println("   comprimento da menor entrada: " + entradas.min)
    mmem.toMap
  }


}
