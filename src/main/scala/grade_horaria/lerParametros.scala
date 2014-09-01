package grade_horaria

class LerParâmetros(nomeArq: String){
  val parâmetros = scala.xml.XML.loadFile(nomeArq)

  val par = (parâmetros \\ "parametros").map { p =>
    val tamPop = (p \ "@tamPop").text.toInt
    val tamSel =  (p \ "@tamSel").text.toInt
    val pc =  (p \ "@pc").text.toDouble
    val pm =  (p \ "@pm").text.toDouble
    val semente =  try {
      Some( (p \ "@semente").text.toLong )
    } catch {
       case e:NumberFormatException => None
    }
    val nMaxGeração = (p \ "@nMaxGeração").text.toInt
    val tamTorneio =   (p \ "@tamTorneio").text.toInt
    val nFilhos =  (p \ "@nFilhos").text.toInt
    val divisor =  (p \ "@divisor").text.toInt
    val prob1 =  (p \ "@prob1").text.toDouble
    val prob2 =  (p \ "@prob2").text.toDouble
    val prob3 =  (p \ "@prob3").text.toDouble
    val maxSteps =  (p \ "@maxSteps").text.toInt
    val limiteBL = (p \ "@limiteBL").text.toDouble
    val maxStepsLS2 =  (p \ "@maxStepsLS2").text.toInt
    val percentualLS2 =  (p \ "@percentualLS2").text.toDouble
    val alfa =  (p \ "@alfa").text.toDouble
    val beta =  (p \ "@beta").text.toDouble
    val gama =  (p \ "@gama").text.toDouble
    val tal =  (p \ "@tal").text.toDouble
    val limiteExec = (p \ "@limiteExec").text.toDouble
    val limiteBL2 = (p \ "@limiteBL2").text.toDouble
    val nExecuções = (p \ "@nExecuções").text.toInt
    Parâmetros(
      tamPop,
      tamSel,
      pc,
      pm,
      semente,
      nMaxGeração,
      tamTorneio,
      nFilhos,
      divisor,
      prob1,
      prob2,
      prob3,
      maxSteps,
      limiteBL,
      maxStepsLS2,
      percentualLS2,
      alfa,
      beta,
      gama,
      tal,
      limiteExec,
      limiteBL2,
      nExecuções
    )
  }

  def getPar(i: Int): Parâmetros = par(i)

  /*def setTempo(par: Parâmetros, limExec:Double):Parâmetros {
    par.limiteExec = limExec
    par
  }*/
  

}
