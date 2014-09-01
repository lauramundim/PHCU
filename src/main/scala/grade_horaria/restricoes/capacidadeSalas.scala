package grade_horaria
import grade_horaria._

import ag.Restrição

//Restrição Preferencial das instâncias XML.

class CapacidadeSalas(prob:Problema) extends Restrição[Cromossomo] {
  // EFEITOS: calcula o número de alunos em excesso em cada sala
  //    para cada evento

  def penalizaEvento(e:Int, cromo:Cromossomo) = {
    var contsala = 0
    var s = cromo(e).sala // indice da sala de e
    var edisiciplia = prob.listaDisciplinas.find(a =>
      a.id == prob.eventos(e)).toArray //eh a disiciplina do e
    var esala = prob.listaSalas(s) // a sala da disciplina do e  
    if(edisiciplia(0).estudantes > esala.tamanho){
      contsala = 
        contsala + (edisiciplia(0).estudantes - esala.tamanho)
    }
    if(debug)
      println("Violações de capacidade salas do evento " + 
        e + ": " + contsala)
    contsala
  }

  def eventosAfetados(e: Int, cromo:Cromossomo) = {
    0
  }

  def violadas(c:Cromossomo):Int = {
    var v_capacidade_salas = 0
    var evento_contado = Array[(String,Int, Int)]()
    //percorre os eventos
    for(i <-0 until prob.nEventos){
      //ve ql disciplina e ql sala correspondem a tal evento
      var idisciplina = 
    prob.listaDisciplinas.find(a => a.id == prob.eventos(i)).toArray
      // eh a disciplina correspondente ao evento i
      var isala = prob.listaSalas(c(i).sala) 
      // eh a sala correspondente ao evento i
      //se o numero de alunos dessa disciplina for maior que a 
      //capacidade da sala o excesso de alunos contam violacoes
      if(idisciplina(0).estudantes > isala.tamanho){
        if(!evento_contado.contains((prob.eventos(i),c(i).sala, c(i).horário))){
          evento_contado = evento_contado :+ (prob.eventos(i), c(i).sala, c(i).horário)

          /*println("[S(" + 
                  (idisciplina(0).estudantes-isala.tamanho) + 
                  ")] Room " + isala.id +
                  " to small for course " +
                  idisciplina(0).id + " the period " +
                  c(i).horário )*/

          v_capacidade_salas = v_capacidade_salas + (idisciplina(0).estudantes - isala.tamanho)
        }//fim if
      }//fim if
    }//fim for i
    if (debug)
      println("CapacidadeSalas: " + v_capacidade_salas)
    v_capacidade_salas
  }//fim violadas

/*
def penalizaEventos(cromo:Cromossomo, penalidades:Array[Int]) = {
   cromo.indices foreach { e =>
      var contsala = 0
      var s = cromo(e).sala // indice da sala de e
      var edisiciplia = prob.listaDisciplinas.find(a =>
        a.id == prob.eventos(e)).toArray //eh a disiciplina do e
      var esala = prob.listaSalas(s) // a sala da disciplina do e  
      if(edisiciplia(0).estudantes > esala.tamanho){
        contsala = contsala + (edisiciplia(0).estudantes - esala.tamanho)
      }
      penalidades(e) += contsala
    }//fim cromo foreach
  }

*/
}//fim classe
