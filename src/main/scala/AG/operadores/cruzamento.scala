package ag

abstract class Cruzamento[Cromossomo,
                 Ind <: Indivíduo[Cromossomo, Ind],
                 P <: População[Cromossomo, Ind, P]] {
  def cruza(pop:P) : P
}

