package grade_horaria.alocador
import grade_horaria._

import collection.mutable.{Map => MMap}

class Reparação(pr: Problema) {
  // salas disponíveis
  private val salas = (pr.nEventos until  pr.nEventos + pr.nSalas).toList

  // ce(t) é conjunto dos eventos que ocorrem no horário t
  private val ce = MMap[Int,List[Int]]()

  // Atribuição de horários
  private var horários = List[Int]() // horários atribuídos aos eventos

  def realocaSalas(c:Cromossomo, h:Int):Cromossomo = {
    val cev = c.eventosSimultâneos 
    val genes = c.toArray

    //  Se existir algum evento em cev(h)
    if (cev(h).nonEmpty){
      //   Cria um grafo bipartido G com uma das partições sendo ce(h),
      // ou seja um conjunto de eventos, e com a outra partição sendo
      // o conjunto com todas as salas disponíveis. Haverá um aresta
      // ligando um evento E a uma sala S se, e somente se, E puder
      // ser realizado em S.
      val g = new GrafoBipartido[Int](cev(h), salas)
        cev(h) foreach { e =>
          salas foreach { s =>
            if (pr.possiveisSalas(e)(s-pr.nEventos))
              g.insereAresta(e,s)
          }
        }
        //   Realiza um emparelhamento no grafo G. Esse emparelhamento
        // alocará cada evento em uma sala diferente.

      val emp = g.emparelha
       
      // Posiciona os eventos com salas já alocadas no vetor de genes
      emp.keys foreach { e =>
        genes(e) = Gene(emp(e)-pr.nEventos, h)
      }
        
      val evAlocados = emp.keys.toSet // eventos alocados
      val evNãoAlocados:Set[Int] = cev(h).toSet -- evAlocados

      //   No caso do emparelhamento não ser perfeito, para cada
      // evento E não emparelhado, aloque E na sala menos ocupada
      // na qual E possa ser realizado.
      if (evNãoAlocados.nonEmpty) {
        val ocupação = Array.ofDim[Int](pr.nSalas) // ocupação de cada sala
          emp.values foreach { s =>
            ocupação(s-pr.nEventos) = 1 }
          evNãoAlocados foreach { e =>
            var lessBusy = -1  // sala ocupada pelo menor número de eventos
            var occupation = Int.MaxValue // numero de eventos na sala menos ocupada
            for (s <- g.vizinhos(e)){
              if (ocupação(s-pr.nEventos) < occupation){
                lessBusy = s
                  occupation = ocupação(s-pr.nEventos)
              }
            }
              genes(e) = Gene(lessBusy-pr.nEventos, h)
          }
      } // fim if evNãoAlocados
    } // fim if ce
    Cromossomo.fromSeq(genes)   

  } // fim realocaSalas

}
