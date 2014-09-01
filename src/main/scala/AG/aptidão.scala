package ag

abstract class Aptidão[Cromossomo] {

  // lista de restrições invioláveis
  private var rInvioláveis:List[Restrição[Cromossomo]] = List()

  // lista de restrições preferenciais
  private var rPreferenciais:List[Restrição[Cromossomo]] = List()

  def addRestriçãoInviolável(r:Restrição[Cromossomo]) =
    rInvioláveis = r::rInvioláveis

  def addRestriçãoPreferencial(r:Restrição[Cromossomo]) =
    rPreferenciais = r::rPreferenciais

  private def invioláveis(c:Cromossomo):Int =
    (0 /: rInvioláveis)( _ + _.violadas(c))

  private def preferenciais(c:Cromossomo):Int =
    (0 /: rPreferenciais)( _ + _.violadas(c))

  def avaliaEventoInv(e:Int, c:Cromossomo):Int = 
    (0 /: rInvioláveis)( _ + _.penalizaEvento(e, c))

  def avaliaEventoPref(e:Int, c:Cromossomo):Int = 
    (0 /: rPreferenciais)( _ + _.penalizaEvento(e, c))

  def avaliaEvento(e:Int, c:Cromossomo):Int =
     avaliaEventoInv(e,c) + avaliaEventoPref(e,c)

  def avaliaAfetadoInv(e:Int, c:Cromossomo):Int = 
    (0 /: rInvioláveis)( _ + _.eventosAfetados(e, c))

  def avaliaAfetadoPref(e:Int, c:Cromossomo):Int = 
    (0 /: rPreferenciais)( _ + _.eventosAfetados(e, c))
  
  def avalia(c:Cromossomo):Int =
    1000000 * invioláveis(c) + preferenciais(c)
}
