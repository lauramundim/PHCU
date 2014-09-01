package object grade_horaria {

  type THorário = Int
  type TDiaSemana = Int
  type TEvento = Int  
  type TSala = Int

  //Criando classe Curso
  case class Curso(
    id:           String,
    professor:    String,
    aulas:        Int,
    min_dias:     Int,
    estudantes:   Int,
    aulas_duplas: String
  )

  // Cursos referenciados nos curriculos
  case class CursoRef(ref: String)

  // Curriculum
  case class Curriculum(
    id: String,
    refs: Seq[CursoRef]
  )

  case class Sala(
    id: String,
    tamanho: Int,
    predio: String
  )

  // Sala referenciada nas restrições
  case class SalaRef(refs: String)

  case class Timeslot(
    dia:Int,
    periodo: Int
  )

  case class Restricao(
    tipo:String,
    curso: String,
    timeslots: Seq[Timeslot],
    salas: Seq[SalaRef]
  )

  var rng = new scala.util.Random

  // O tipo da estrutura de dados MEM
  type TMEM = Map[Int, List[Gene]]

  val debug = false // se desejar informações de debug, mude para true


}
