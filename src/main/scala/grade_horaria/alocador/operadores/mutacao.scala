package grade_horaria.alocador

import grade_horaria._
import grade_horaria.Mutação

class MutaçãoPop(
  par:Parâmetros,
  mutação: MovimentoAleatório
) extends Mutação[Indivíduo, População](par, mutação) 
