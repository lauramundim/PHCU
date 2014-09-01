package grade_horaria

import scala.collection.mutable.HashMap

import grade_horaria._
import ag.Restrição

//Restrição Preferencial das instâncias XML.

class MínimoDias(prob:Problema) extends Restrição[Cromossomo] {
  // EFEITOS: calcula o número de dias a menos do mínimo em que uma
  //    disciplina foi alocada

  def penalizaEvento(e: Int, cromo:Cromossomo) = {
    var discDias = new HashMap[Int, Array[Int]]
    for(i <- 0 until prob.nDisciplinas) discDias += i -> Array()
    var somaA = 0
    for(i <- 0 until prob.nDisciplinas){
      for(j <- somaA until somaA + prob.listaDisciplinas(i).aulas){
        var dia = (cromo(j).horário/prob.periodosPorDia).toInt
        if(!discDias(i).contains(dia))
          discDias(i) = discDias(i) :+ dia
      }//fim for j
      somaA = somaA + prob.listaDisciplinas(i).aulas
    }//fim for i

    var contdia = 0
    //disciplina do e
    var edisiciplina = 
      prob.listaDisciplinas.find(a => a.id == prob.eventos(e)).toArray
    if(discDias(prob.listaDisciplinas.indexOf(edisiciplina(0))).length <
            edisiciplina(0).min_dias){
      contdia = contdia + (edisiciplina(0).min_dias -
                discDias(prob.listaDisciplinas.indexOf(edisiciplina(0))).length)
    }
    if(debug)
      println("Violações de mínimo de dias " + e + ": " + contdia)
    contdia
  }

  def eventosAfetados(e: Int, cromo:Cromossomo) = {
    0
  }
  
  def violadas(c:Cromossomo):Int = {
    //para calcular o número de dias em que uma disciplina foi
    //alocada, vamos usar uma estrura que chamamos de discDias
    //discDias é um hashMap onde as chaves são as disciplinas em que
    //para cada discplina tem um array de dias que acontece essa disciplina
    var discDias = new HashMap[Int, Array[Int]]
    for(i <- 0 until prob.nDisciplinas) discDias += i -> Array()
    var somaA = 0
    for(i <- 0 until prob.nDisciplinas){
      for(j <- somaA until somaA + prob.listaDisciplinas(i).aulas){
        var dia = (c(j).horário/prob.periodosPorDia).toInt
        if(!discDias(i).contains(dia))
          discDias(i) = discDias(i) :+ dia
      }//fim for j
      somaA = somaA + prob.listaDisciplinas(i).aulas
    }//fim for i
            
    var v_min_dias = 0
    for(i <- 0 until prob.nDisciplinas){
      if(discDias(i).length < prob.listaDisciplinas(i).min_dias){
        v_min_dias =
          v_min_dias + (prob.listaDisciplinas(i).min_dias -
              discDias(i).length )
      }//fim if
    }// fim for i

    if (debug)
      println("MinimoDias: " + (v_min_dias*5))
    v_min_dias * 5
  }//fim violadas


/*
def penalizaEventos(cromo:Cromossomo, penalidades:Array[Int]) = {

    var discDias = new HashMap[Int, Array[Int]]
    for(i <- 0 until prob.nDisciplinas) discDias += i -> Array()
    var somaA = 0
    for(i <- 0 until prob.nDisciplinas){
      for(j <- somaA until somaA + prob.listaDisciplinas(i).aulas){
        var dia = (cromo(j).horário/prob.periodosPorDia).toInt
        if(!discDias(i).contains(dia))
          discDias(i) = discDias(i) :+ dia
      }//fim for j
      somaA = somaA + prob.listaDisciplinas(i).aulas
    }//fim for i

    cromo.indices foreach { e =>
      var contdia = 0
      //disciplina do e
      var edisiciplina = prob.listaDisciplinas.find(a => a.id == prob.eventos(e)).toArray
  
      if(discDias(prob.listaDisciplinas.indexOf(edisiciplina(0))).length <
            edisiciplina(0).min_dias){
        contdia = contdia + (edisiciplina(0).min_dias -
                  discDias(prob.listaDisciplinas.indexOf(edisiciplina(0))).length)
      }
      penalidades(e) += contdia
    }
  }
*/
}//fim classe
