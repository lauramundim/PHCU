package ag

abstract class Parâmetros {
  val tamPop:Int // número de indivíduos na população
  val tamSel:Int // número de indivíduos selecionados da população
                 // para cruzamento
  val pc: Double  // probabilidade de cruzamento
  val pm: Double  // probabilidade de mutação
  val semente: Option[Long] 

  //assert(tamPop > 1, 
    // { println("O número de indivíduos deve ser maior que 1.")})

}
