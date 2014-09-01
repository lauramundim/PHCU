package grade_horaria

import grade_horaria._
import ag.{Indivíduo, População, Mutação => MutInd, MutaçãoPop}

abstract class Mutação[Ind <: Indivíduo[Cromossomo, Ind],
                       P <: População[Cromossomo,Ind,P]](
  par:Parâmetros,
  mutação: MutInd[Cromossomo, Ind]
) extends MutaçãoPop[Cromossomo, Ind, P] {

  def opera(pop:P):P = {
    val mutantes = (for (ind <- pop.indivíduos) yield {
                        if (rng.nextFloat < par.pm)
                           mutação.opera(ind)
                        else 
                           ind })
    pop.replica(mutantes)
  }
}
