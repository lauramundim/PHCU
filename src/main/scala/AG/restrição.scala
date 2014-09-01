package ag

abstract class Restrição[Cromossomo] {

  //def penalizaEventos(c:Cromossomo, penalidades:Array[Int])
  def penalizaEvento(e:Int, c:Cromossomo):Int
  def eventosAfetados(e:Int, c:Cromossomo):Int
  def violadas(c:Cromossomo):Int
  // EFEITOS: calcula o número de restrições violadas
}
