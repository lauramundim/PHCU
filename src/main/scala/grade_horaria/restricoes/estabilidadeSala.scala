package grade_horaria

import scala.collection.mutable.HashMap

import grade_horaria._
import ag.Restrição

//Restrição Preferencial das instâncias XML.

class EstabilidadeSala(prob:Problema) extends Restrição[Cromossomo] {
  // EFEITOS: calcula o número salas diferentes em que eventos de uma
  //   mesma disciplina foram alocados

  def penalizaEvento(e:Int, cromo:Cromossomo) = {

    var discSalas = new HashMap[Int, Array[Int]]
    for(i <- 0 until prob.nDisciplinas) discSalas += i -> Array()
    var somaA = 0
    for(i <- 0 until prob.nDisciplinas){
      for(j <- somaA until somaA + prob.listaDisciplinas(i).aulas){
        var sala = cromo(j).sala
        discSalas(i) = discSalas(i) :+ sala
      }
      somaA = somaA + prob.listaDisciplinas(i).aulas
    }

    var contsala = 0
    //pega a disciplina de e
    var edisciplina = prob.listaDisciplinas.find(a =>
      a.id == prob.eventos(e)).toArray//a posicao dessa disciplina
    var edisciplinaPos = prob.listaDisciplinas.indexOf(edisciplina(0))
    //array com as salas diferentes dessa disciplina
    var nSalasDiferentes = discSalas(edisciplinaPos).distinct
    if(nSalasDiferentes.length>1)
      contsala = contsala + (nSalasDiferentes.length-1)

    if(debug)
      println("Violações de estabilidade de sala do evento " + e + ": " + contsala)
    contsala
  }

  def eventosAfetados(e: Int, cromo:Cromossomo) = {
    0
  }

  def violadas(c:Cromossomo):Int = {
    //   Para calcular o número de diferentes salas em que eventos de uma
    // disciplina foram alocados vamos usar uma estrura que
    // chamamos de discSalas
    // discSalas é um hashMap onde as chaves são as disciplinas em que
    // para cada discplina tem um array de salas em que acontece
    // essa disciplina
    var discSalas = new HashMap[Int, Array[Int]]
    for(i <- 0 until prob.nDisciplinas) discSalas += i -> Array()
    var somaA = 0
    for(i <- 0 until prob.nDisciplinas){
      for(j <- somaA until somaA + prob.listaDisciplinas(i).aulas){
        var sala = c(j).sala
        discSalas(i) = discSalas(i) :+ sala
      }
      somaA = somaA + prob.listaDisciplinas(i).aulas
    } //fim fori
      
    //começando a contar as violações
    var v_estabilidade_sala = 0
    for(j <- 0 until prob.nDisciplinas){
      //aux guarda as sals diferentes de umadisciplina
      var aux = discSalas(j).distinct
      //se esse tamanho or maior q 1 tem salas diferentes
      if(aux.length > 1){

        /*println("[S(" + (aux.length-1)  +  ")] Course " +
                prob.listaDisciplinas(j).id + " uses " +
                aux.length + " different rooms")*/

        v_estabilidade_sala = 
          v_estabilidade_sala  + (aux.length-1) 
      }
    }//fim for

    if (debug)
      println("EstabilidadeSala: " + v_estabilidade_sala)

    v_estabilidade_sala
  }//fim violadas

/*
def penalizaEventos(cromo:Cromossomo, penalidades:Array[Int]) = {

    var discSalas = new HashMap[Int, Array[Int]]
    for(i <- 0 until prob.nDisciplinas) discSalas += i -> Array()
    var somaA = 0
    for(i <- 0 until prob.nDisciplinas){
      for(j <- somaA until somaA + prob.listaDisciplinas(i).aulas){
        var sala = cromo(j).sala
        discSalas(i) = discSalas(i) :+ sala
      }
      somaA = somaA + prob.listaDisciplinas(i).aulas
    }

    cromo.indices foreach { e =>
      var contsala = 0
      //pega a disciplina de e
      var edisciplina = prob.listaDisciplinas.find(a =>
        a.id == prob.eventos(e)).toArray//a posicao dessa disciplina
      var edisciplinaPos = prob.listaDisciplinas.indexOf(edisciplina(0))
      //array com as salas diferentes dessa disciplina
      var nSalasDiferentes = discSalas(edisciplinaPos).distinct
      if(nSalasDiferentes.length>1)
        contsala = contsala + (nSalasDiferentes.length-1)
      penalidades(e) += contsala
    }//fim cromo foreach
  }
*/
}//fim classe
