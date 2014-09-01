package grade_horaria.basico

import grade_horaria._
import grade_horaria.CruzamentoBinario

class Cruzamento(
  par:Parâmetros,
  prob: Problema
) extends CruzamentoBinario[Indivíduo, População](par, prob) 
