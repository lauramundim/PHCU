package grade_horaria
import grade_horaria._

import scala.io.Source
import scala.util.control.Breaks._
import scala.util.Random
import scala.collection.mutable.HashMap

class Tim(arquivo: String) extends Problema {
  val diasPorSemana = 5
  val periodosPorDia = 9
  val nHorários = diasPorSemana * periodosPorDia
  private val linhas = Source.fromFile(arquivo).getLines

  // Variáveis da classe Problema que não serão efativamente
  //   usadas neste formato de instância.
  val siglasSalas = Array[String]()
  val listaCurriculos = List[Curriculum]()
  val nCurriculos = listaCurriculos.length
  val listaRestricoes = List[Restricao]()
  val nRestricoes = listaRestricoes.length  
  
  // Le uma instancia de problema de um arquivo .tim
  // Os primeiros quatro inteiros são
  //    nEventos: numero total de eventos
  //    nSalas: numero total de salas
  //    nRecursos: numero de recursos disponiveis
  //    nEstudantes: numero de estudantes
  val Array(nEventos,nSalas,nRecursos,nEstudantes) =
    linhas.next.split(" ") map(_.toInt)
 
  // Vetor com as capacidades de cada sala
  val tamSalas = new Array[Int](nSalas)
  for (i <- 0 until nSalas)
    tamSalas(i) = linhas.next.split(" ")(0).toInt

  // Matriz estudante e suas participacoes em eventos
  val estudanteEventos = Array.ofDim[Int](nEstudantes, nEventos)
  for (i <- 0 until nEstudantes)
    for (j <- 0 until nEventos)
      estudanteEventos(i)(j) = linhas.next.split(" ")(0).toInt

  // Vetor que guarda o numero de estudantes em cada evento
  val estudanteNum = new Array[Int](nEventos)
  for (i <- 0 until nEventos) {
    var soma = 0
    for (j <- 0 until nEstudantes)
      soma += estudanteEventos(j)(i)
    estudanteNum(i) = soma
  }
  val listaSalas =
    (for (i <- 0 until nSalas) yield {
      val id = "r%03d".format(i)
      val tamanho = tamSalas(i)
      val predio = "0"
      Sala(id, tamanho, predio)
    }).toList
  val listaDisciplinas =
    (for (i <- 0 until nEventos) yield {
      val id = "c%04d".format(i)
      val professor = "t%04d".format(i)
      val aulas = 1
      val min_dias = 1
      val estudantes = estudanteNum(i)
      val aulas_duplas = "yes"
      Curso(id, professor, aulas, min_dias, estudantes, aulas_duplas)
    }).toList

  val nDisciplinas = listaDisciplinas.length

  // O vetor eventos é um vetor que mapeia cada evento (o índice) 
  //  com o id da disciplina que é por ele representado.
  val eventos = new Array[String](nEventos)

  // Matriz que guarda informação sobre eventos que tenham
  //   estudantes em comum.
  val eventoCorrelacoes = Array.ofDim[Boolean](nEventos,nEventos)
  
  for (i <- 0 until nEventos)
    for (j <- 0 until nEventos)
      breakable { for (k <- 0 until nEstudantes)
        if ( estudanteEventos(k)(i) == 1 &&
          estudanteEventos(k)(j) == 1 ) {
          eventoCorrelacoes(i)(j) = true
          break
        }
      }

  // Matriz que guarda as informacoes sobre os recursos disponiveis
  //   em cada sala
  val salaRecursos = Array.ofDim[Int](nSalas,nRecursos)
  for (i <- 0 until nSalas)
    for (j <- 0 until nRecursos)
      salaRecursos(i)(j) = linhas.next.split(" ")(0).toInt

  // Matriz que armazena a informação sobre os recursos necessários
  //   para cada evento
  val eventoRecursos = Array.ofDim[Int](nEventos,nRecursos)
  for (i <- 0 until nEventos)
    for (j <- 0 until nRecursos)
      eventoRecursos(i)(j) = linhas.next.split(" ")(0).toInt
  
  // Matriz que mantem a informacao sobre qual sala e adequada
  // para cada evento
  val possiveisSalas = Array.ofDim[Boolean](nEventos,nSalas)
  for (i <- 0 until nEventos)
    for (j <- 0 until nSalas)
      if (tamSalas(j) >= estudanteNum(i)) {
        var k = 0
        var eAdequada = true
        while ( k < nRecursos && eAdequada) {
          if (eventoRecursos(i)(k) == 1 && salaRecursos(j)(k) == 0)
            eAdequada = false
          k += 1
        }
        possiveisSalas(i)(j) = eAdequada
      }
	
  // diasHorários mapeia os horarios que estão em um dia
  var diasHorários = new HashMap[Int, Array[Int]]
  for(i <- 0 until diasPorSemana) diasHorários += i -> Array()
    
  var k = 0
  for(i <- 0 until diasPorSemana){
    for(j <- 0 until periodosPorDia){
      diasHorários(i) = diasHorários(i) :+ k
      k = k+1
    }
  }
}


















