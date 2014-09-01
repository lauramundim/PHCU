package ag

abstract class Seleção[Cromossomo,
              Ind <: Indivíduo[Cromossomo, Ind],
              P <: População[Cromossomo, Ind, P]] {
  def seleciona(pop:P) : P
}

