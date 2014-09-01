package grade_horaria
import grade_horaria._

import scala.collection.mutable.HashMap

class Ctt(nome_arquivo: String) extends Problema {
  
  //inseri para ter na classe mãe
   val estudanteEventos = Array(Array(0))
   val nEstudantes = 0

  type Tabela = Array[Array[Option[Int]]]
    
  // Leitura do arquivo XML
  private val arq = scala.xml.XML.loadFile(nome_arquivo)
    
  //Pegando os dados e convertendo os tipos
  //--------------DESCRITOR-------------------
  val nomeInstancia = 
   ((arq \\ "instance").map{_ \ "@name"}).head.text

  val diasPorSemana =
    ((arq \ "descriptor" \"days").map{_ \ "@value"}).head.text.toInt

  val periodosPorDia =
    ((arq \ "descriptor" \"periods_per_day").map{_ \ "@value"}).head.text.toInt

  val nHorários = diasPorSemana * periodosPorDia

  val minAulas =
    ((arq \ "descriptor" \"daily_lectures").map{_ \ "@min"} ).head.text.toInt

  val maxAulas =
    ((arq \ "descriptor" \"daily_lectures").map{_ \ "@max"}).head.text.toInt

  //---------------- LISTAS---------------------
  // Criando  lista de disciplinas
  val listaDisciplinas = (arq \ "courses" \ "course").map { course =>
    Curso(
      (course \ "@id").text,
      (course \ "@teacher").text,
      (course \ "@lectures").text.toInt,
      (course \ "@min_days").text.toInt,
      (course \ "@students").text.toInt,
      (course \ "@double_lectures").text
    )
  }

    
  // Criando lista de salas
  val listaSalas = (arq \ "rooms" \ "room").map { room =>
    Sala(
      (room \ "@id").text,
      (room \ "@size").text.toInt,
      (room \ "@building").text
    )
  }

  // Criando lista de curriculos
  val listaCurriculos = (arq \ "curricula" \ "curriculum").map{ c =>
    val id = (c \ "@id").text
    val cursos = (c \ "course").map { cur =>
      CursoRef((cur \ "@ref").text)
    }
      Curriculum(id, cursos)
  }

  //Criando uma lista de restrições
  val listaRestricoes = (arq \ "constraints" \ "constraint").map{ c =>
    val tipo = (c \ "@type").text
    val curso = (c \"@course").text
    val timeslots = (c \ "timeslot").map { time =>
      Timeslot((time \ "@day").text.toInt, (time \ "@period").text.toInt)
    }
    val salas = (c \ "room").map { sala =>
      SalaRef((sala \ "@ref").text)
    }
      Restricao(tipo, curso, timeslots, salas)
  }
    
  
  //---------contagem dos elementos das listas----

  // O número de salas, restrições, currículos
  val nSalas = listaSalas.length
  val nRestricoes = listaRestricoes.length
  val nCurriculos = listaCurriculos.length
  val nDisciplinas = listaDisciplinas.length
    
  // Cada uma das aulas de uma disciplina é um evento
  val nEventos = listaDisciplinas.foldLeft(0)( (total,d) => total + d.aulas )
  val disciplinas =
    listaDisciplinas.foldRight(Map[String,(Int,Int)]())((d,m) =>
      m + (d.id -> (d.aulas, d.aulas) ))
      
  //-------------montando disponibilidade das disciplinas
  //de acordo com as restrições de período vamos criar a matriz dispDisciplinas
  
  val dispDisciplinas:Array[Tabela] =
    Array.fill[Option[Int]](nDisciplinas, periodosPorDia, diasPorSemana)(Some(1))

  for(i <-0 until nRestricoes){
    //se for restricao de horario
    if(listaRestricoes(i).tipo == "period"){
      //pegar a disciplina relacionada a siga do curso
      val disciplina = listaDisciplinas.find(a =>
          a.id ==listaRestricoes(i).curso).toArray
      val d = disciplina(0)
	//pegar a posicao dela
      val posDis = listaDisciplinas.indexOf(d)
      for(j <- 0 until listaRestricoes(i).timeslots.length){
	val dia = listaRestricoes(i).timeslots(j).dia
	val horario = listaRestricoes(i).timeslots(j).periodo
	  dispDisciplinas(posDis)(horario)(dia) = None
      }
    }
  }
    
  //-----------vetor de siglas de evento e salas-------
  // O vetor eventos é um vetor que mapeia cada evento (o índice) com o
  // id da disciplina que é por ele representado.
  val eventos = new Array[String](nEventos)

  val nAulas = {
    var soma = 0
    for(i <- 0 until nDisciplinas){
      for(j <- soma until soma + listaDisciplinas(i).aulas){
        eventos(j) = listaDisciplinas(i).id
      }
      soma += listaDisciplinas(i).aulas
    }
    soma
  }
    
  val siglasSalas = new Array[String](nSalas)
  for(i <- 0 until nSalas){
    siglasSalas(i) = listaSalas(i).id
  }
    
  val siglasDisciplinas = new Array[String](nDisciplinas)
  for(i <- 0 until nDisciplinas){
    siglasDisciplinas(i) = listaDisciplinas(i).id
  }

  //-------- vetores com numero de alunos e numero de salas------

  // Vetor que guarda o numero de estudantes em cada evento
  val estudanteNum = new Array[Int](nEventos)
  for (i <- 0 until nEventos) {
    var aux = listaDisciplinas.find(a => a.id == eventos(i)).toArray
      estudanteNum(i) = aux(0).estudantes
  }
    
  // vetor com capacidades das salas
  val tamSalas = new Array[Int](nSalas)
  for(i <- 0 until nSalas){
    tamSalas(i) = listaSalas(i).tamanho
  }

  //-----------------possiveis salas------------------
  //para as impossiveis salas sera uma matriz onde o numero de
  //linhas sao os eventos e as salas sao as colunas
  // marca 1 na onde as salas nao sao possiveis
  //e o numero de aula vezes
  val impossiveisSalas = Array.ofDim[Boolean](nEventos,nSalas)
  for (i <- 0 until nEventos){
    for (j <- 0 until nSalas){
      for(k <- 0 until nRestricoes){
	if(listaRestricoes(k).tipo == "room"){
	  for(l <- 0 until listaRestricoes(k).salas.length){
	    if(listaRestricoes(k).salas(l).refs == listaSalas(j).id &&
               eventos(i) == listaRestricoes(k).curso)
	      impossiveisSalas(i)(j) = true
	  }
	}
      }
    }
  }

  //salas que possuem capacidade suficiente para as disciplinas
  //permitindo que ultrapasse 1 aluno
  val possiveisSalas = Array.ofDim[Boolean](nEventos,nSalas)
  for (i <- 0 until nEventos){
    for (j <- 0 until nSalas) {
      //procuro ql disciplina eh com o id do evento, retorna array de uma posicao
      var aux = listaDisciplinas.find(a => a.id == eventos(i)).toArray
      if (tamSalas(j) >= aux(0).estudantes) {
	possiveisSalas(i)(j) = true
      }
    }
  }

  // agora tirando as impossiveis salas das possiveis
  for (i <- 0 until nEventos){
    for (j <- 0 until nSalas) {
      if(impossiveisSalas(i)(j)){
	possiveisSalas(i)(j) = false
      }
    }
  }

  //------------- correlacionando os eventos -----------
  // disciplinas do mesmo curriculo sao eventos correlacionados
  
  val eventoCorrelacoes = Array.ofDim[Boolean](nEventos,nEventos)
    //todos os evento -> todos os horarios de cada materia
  for ( i <-0 until nEventos){
    for (j <-0 until nEventos){
      var disciplinai = listaDisciplinas.find(a => a.id == eventos(i)).toArray
      var di = disciplinai(0)
      var disciplinaj = listaDisciplinas.find(a => a.id == eventos(j)).toArray
      var dj = disciplinaj(0)
      if (di.professor == dj.professor || // possui o mesmo professor ou
          eventos(i) == eventos(j)) {     // é a mesma disciplina
         eventoCorrelacoes(i)(j) = true
      } 
      else{
	//se evento i esta no mesmo curriculo do evento j
	for(k <-0 until nCurriculos){
	  if(listaCurriculos(k).refs.contains(CursoRef(eventos(i))) &&
             listaCurriculos(k).refs.contains(CursoRef(eventos(j)))){
	    eventoCorrelacoes(i)(j) = true
	  }
	}
      }
    }
  }
    
  var horariosDisponiveis = Map[(TEvento,TSala), Set[(TDiaSemana,THorário)]]()
    
  for {
    ie <- 0 until nEventos
    disciplina = listaDisciplinas.find(a => a.id == eventos(ie)).toArray
    d = disciplina(0)
    posDis = listaDisciplinas.indexOf(d)
    (salaOK,s) <- possiveisSalas(ie).view.zipWithIndex
    h <- 0 until periodosPorDia
    dia <- 0 until diasPorSemana
    if (!List(dispDisciplinas(posDis)(h)(dia)).contains(None) && salaOK)
  } 
    horariosDisponiveis += (ie,s) ->
      (horariosDisponiveis.getOrElse((ie,s), Set[(TDiaSemana,THorário)]()) + (dia -> h))
	
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


