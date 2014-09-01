package grade_horaria

import scala.collection.immutable.SortedSet

import grade_horaria._
import ag.{Indivíduo, População, Atualização => AtualAG}

abstract class Atualização[Ind <: Indivíduo[Cromossomo, Ind],
                           P <: População[Cromossomo,Ind,P]] extends AtualAG[Cromossomo, Ind, P] {
  // EFEITOS: seleciona tamSel indivíduos da população e os retorna.

  def atualiza(pais:P, filhos:P):P = {
    val csPais = pais.indivíduos
    val csFilhos = filhos.indivíduos
    val inds = csPais.take(csPais.size - csFilhos.size) | csFilhos
    pais.replica(inds)
  }
}
