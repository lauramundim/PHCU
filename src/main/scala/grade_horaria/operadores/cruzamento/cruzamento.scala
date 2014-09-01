package grade_horaria

import scala.collection.immutable.SortedSet

import grade_horaria._
import ag.{Indivíduo, População, Cruzamento}

class CruzamentoBinario[Ind <: Indivíduo[Cromossomo, Ind],
                        P <: População[Cromossomo,Ind,P]](
  par:Parâmetros,
  prob: Problema
) extends Cruzamento[Cromossomo, Ind, P] {
  // EFEITOS: seleciona tamSel indivíduos da população e os retorna.

  def cruza(pop:P):P = {
    val ind = pop.indivíduos.toVector
    val filhos =  SortedSet[Ind]() ++
    (for (i <- 0 until par.nFilhos) yield {
      // Escolher aleatoriamente dois pais
      val p1 = rng.nextInt(par.tamSel)
      var p2 = rng.nextInt(par.tamSel)
      while (p1 == p2) p2 = rng.nextInt(par.tamSel)
      var cFilho = ind(p1).cromossomo
      val c2 = ind(p2).cromossomo
      if (rng.nextFloat < par.pc) 
        for (g <- 0 until prob.nEventos)
           if (rng.nextFloat < 0.5) 
              cFilho = cFilho updated (g, c2(g))
      ind(0).replica(cFilho)
     })
     pop.replica(filhos)
  }
}
