package grade_horaria

import scala.collection.immutable.SortedSet

import grade_horaria._
import ag.{Indivíduo, População}

class SoluçãoGuiada[Ind <: Indivíduo[Cromossomo, Ind],
                    P <: População[Cromossomo,Ind,P]](prob:Problema) {

  // Cria um novo filho usando a MEM
  def novoFilho(mem: TMEM, ind: Ind, beta:Double):Ind = {
    var cFilho = Cromossomo(prob.nEventos)(prob.nSalas, prob.nHorários)

    // E_s = selecione aleatoriamente Beta * n eventos
    val es = rng.shuffle( (0 until prob.nEventos).toList ) take
      (beta * prob.nEventos).toInt

    // para cada evento e_i em E_s faça
    es foreach { e =>
      if (mem contains e){
        // selecione aleatoriamente um par sala-horário da lista l_i
        val sh = mem(e)( rng.nextInt(mem(e).size) )
        // atribua o par selecionado ao evento e_i do filho
        cFilho = cFilho updated (e, sh)
      }      
    }

    // Note que para cada evento restante e_i não presente em E_s
    // um par sala-horário aleatório já foi atribuído ao evento e_i
    // durante a criação do cromossomo.
    ind.replica(cFilho)
  }

  def aplica(mem: TMEM, pop: P, nFilhos:Int, beta:Double, gama:Double):P = {
    val filhos = SortedSet[Ind]() ++
      (for ( i <- 0 until nFilhos ) yield
        novoFilho(mem, pop.indivíduos.head, beta) )
    pop.replica(filhos)
  }
}
