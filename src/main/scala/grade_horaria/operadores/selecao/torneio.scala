package grade_horaria

import scala.collection.immutable.SortedSet

import grade_horaria._
import ag.{Indivíduo, População, Seleção}

abstract class Torneio[Ind <: Indivíduo[Cromossomo, Ind],
                       P <: População[Cromossomo,Ind, P]](
  par:Parâmetros
) extends Seleção[Cromossomo, Ind, P] {
  // EFEITOS: seleciona tamSel indivíduos da população e os retorna.

  def seleciona(pop:P)= {
    val vInd = pop.indivíduos.toVector
    val selecionados = SortedSet[Ind]() ++
    (for (i <- 1 to par.tamSel) yield {
      // Realiza o torneio entre tam indivíduos
      // O primeiro selecionado é considerado o melhor
      var melhor = vInd(rng.nextInt(vInd.size))
      for (k <- 2 to par.tamTorneio){
        val cand = vInd(rng.nextInt(vInd.size))
        if (cand.apt < melhor.apt)
          melhor = cand
      }
      melhor
    })
    pop.replica(selecionados)  
  }

}
