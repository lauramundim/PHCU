package grade_horaria

import grade_horaria._
import scala.collection.mutable.HashMap

abstract class Problema extends ag.Problema {
  val nome = "Grade Horária Universitária"
  val nEventos:Int
  val nSalas:Int
  val nHorários:Int
  val nDisciplinas:Int
  val nCurriculos:Int
  val nRestricoes:Int
  val nEstudantes: Int
  val periodosPorDia:Int
  val diasPorSemana: Int
  val estudanteNum: Array[Int]
  val tamSalas: Array[Int]
  val estudanteEventos: Array[Array[Int]]
  var diasHorários: HashMap[Int, Array[Int]] 
  val siglasSalas: Array[String]
  val possiveisSalas: Array[Array[Boolean]]
  val listaDisciplinas: Seq[Curso]
  val listaCurriculos: Seq[Curriculum]
  val listaSalas: Seq[Sala]
  val listaRestricoes: Seq[Restricao]
  val eventos: Array[String]
  val eventoCorrelacoes: Array[Array[Boolean]]
}

object Problema {
case class ExtensaoEx(message: String) extends Exception(message)

  def apply(nome_arq: String):Problema = {
    nome_arq.split('.').drop(1).lastOption match {
      case Some("tim") => new Tim(nome_arq)
      case Some("xml") => new Ctt(nome_arq)
      case _ =>
        throw ExtensaoEx("Arquivo sem extensão ou com extensão não conhecida")
    }
  }
  
}
