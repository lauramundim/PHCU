package grade_horaria.alocador

import collection.immutable.SortedSet

import grade_horaria._
import grade_horaria.CruzamentoBinario

class Cruzamento(
  par:Parâmetros,
  prob: Problema,
  rep: Reparação
) extends CruzamentoBinario[Indivíduo, População](par, prob) {

  override def cruza(pop:População):População = {
    val ind = pop.indivíduos.toVector
    val filhos =  SortedSet[Indivíduo]() ++
    (for (i <- 0 until par.nFilhos) yield {
      // Escolher aleatoriamente dois pais
      val p1 = rng.nextInt(par.tamSel)
      var p2 = rng.nextInt(par.tamSel)
      while (p1 == p2) p2 = rng.nextInt(par.tamSel)
      var cFilho = ind(p1).cromossomo
      val c2 = ind(p2).cromossomo
      if (rng.nextFloat < par.pc) {
         for (g <- 0 until prob.nEventos)
           if (rng.nextFloat < 0.5)
             cFilho = cFilho updated (g, c2(g))

         for (t <- 0 until prob.nHorários)
           if (cFilho contains t)
             cFilho = rep.realocaSalas(cFilho, t)
      }

     ind(0).replica(cFilho)
     })
     pop.replica(filhos)
  }
}
