package grade_horaria
import grade_horaria._

//Restrição Inviolável das instâncias XML.

import ag.Restrição
class AulasAgendadas(prob:Problema) extends Restrição[Cromossomo] {

  def penalizaEvento(e:Int, cromo:Cromossomo) = {
    /*def conta(ac: Int, h:Int, e:Int, i:Int, f:Int):Int =
      if (i >= f) ac
      else if (cromo(i).horário == h) {
        penalidades(e) += 1
        penalidades(i) += 1
        conta(ac + 1, h, e, i+1, f)
      } else conta(ac, h, e, i+1, f)

    def contad(ac:Int, inicio:Int, fim:Int):Int = {
      if (inicio >= fim) ac
      else {
        val contagem = conta(0, cromo(inicio).horário, inicio, inicio+1, fim)
        contad(ac + contagem, inicio+1, fim)
      }
    }

    def recorre(ac:Int, ds:Seq[Curso], inicio:Int):Int =
      if (ds.isEmpty) ac
      else {
        val fim = inicio + ds.head.aulas
        recorre( ac + contad(0,inicio,fim), ds.tail, fim)
    }

    val num = recorre(0, prob.listaDisciplinas, 0)*/
    if(debug)
      println("Violações de aulas agendadas do evento " + e + ":0")
    0
  }

  def eventosAfetados(e: Int, cromo:Cromossomo) = {
    0
  }

  // EFEITOS: calcula o número total de aulas não agendadas para todas
  //    as disciplinas. As aulas de uma mesma disciplina devem estar
  //    agendadas em horários diferentes. Assim, cada aula de uma
  //    disciplina agendada a mais para um mesmo horário conta como
  //    uma violação.


  def violadas(c:Cromossomo):Int = {
    var v_aulas = 0
    var somatorioAulas = 0
    var k = 0
    //percorre as disciplinas
    for(i <- 0 until prob.nDisciplinas){
      //guarda em aux1 os horarios de uma disciplina
      var aux1 = Array[Int]() 
      //percorre os horarios de uma disciplina
      for(j <- somatorioAulas until somatorioAulas + 
               prob.listaDisciplinas(i).aulas){
        aux1 = aux1 :+ c(k).horário	
        k = k +1
      }
      somatorioAulas = 
        somatorioAulas + prob.listaDisciplinas(i).aulas
      //aux2 sera apenas os horarios diferentes
      var aux2 = aux1.distinct
      //se os tamanhos de aux1 e aux2 forem diferentes
      //significa que eliminou algum horario repitido ou seja
      //violou uma restricao de aula q ficou no mesmo horario
      if(aux1.length != aux2.length)
        v_aulas = v_aulas + (aux1.length - aux2.length)
    }
    if (debug)
      println("AulasAgendadas: " + v_aulas) 
    v_aulas
  }
}

