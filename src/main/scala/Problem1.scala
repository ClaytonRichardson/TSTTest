object BestGroupPriceCalculator {

  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    val rateMap = rates.map(rate => rate.rateCode -> rate.rateGroup).toMap

    val bestPrices = prices.foldLeft(Map[String, CabinPrice]()) { (acc, cp) =>
      val key = s"${cp.cabinCode}_${rateMap(cp.rateCode)}"
      acc.get(key) match {
        case Some(existing) if existing.price <= cp.price => acc
        case _ => acc + (key -> cp)
      }
    }

    bestPrices.map { case (_, cp) =>
      BestGroupPrice(cp.cabinCode, cp.rateCode, cp.price, rateMap(cp.rateCode))
    }.toSeq
  }
}

object Problem1 {
  import BestGroupPriceCalculator._

  def main(args: Array[String]): Unit = {
    val rateTest = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior"))

    val pricesTest = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00))

    val bestGroupPrices = getBestGroupPrices(rateTest, pricesTest)
    println(bestGroupPrices)
  }
}
