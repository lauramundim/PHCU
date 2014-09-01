package ag

abstract class Atualização[Cromossomo,
                 Ind <: Indivíduo[Cromossomo, Ind],
                 P <: População[Cromossomo, Ind, P]] {
  def atualiza(pais:P, filhos:P) : P
}

