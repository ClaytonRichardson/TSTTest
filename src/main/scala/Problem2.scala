object PromotionProcessor {

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    val promoMap = allPromotions.map(p => p.code -> p).toMap

    def isCombinable(subset: Seq[String]): Boolean = {
      subset.combinations(2).forall { case Seq(a, b) =>
        (promoMap.get(a), promoMap.get(b)) match {
          case (Some(promoA), Some(promoB)) =>
            !promoA.notCombinableWith.contains(b) && !promoB.notCombinableWith.contains(a)
          case _ => false
        }
      }
    }

    val allSubsets = promoMap.keySet.subsets().filter(_.nonEmpty).map(_.toSeq).toSeq

    val combinableSubsets = allSubsets.filter(isCombinable)

    val maximalCombinableSubsets = combinableSubsets.filter { subset =>
      !combinableSubsets.exists(other => subset.toSet.subsetOf(other.toSet) && subset != other)
    }

    maximalCombinableSubsets.map(subset => PromotionCombo(subset.sorted))
  }


  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    val possiblePromotions = allCombinablePromotions(allPromotions)
    possiblePromotions.filter(_.promotionCodes.contains(promotionCode))
  }
}

object Problem2 {
  import PromotionProcessor._

  def main(args: Array[String]): Unit = {

    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val allCombos = allCombinablePromotions(promotions)
    println("All Promotion Combinations:")
    println(allCombos)

    val promotionComb1 = combinablePromotions("P1", promotions)
    println("Promotion Combinations for promotionCode = P1")
    println(promotionComb1)

    val promotionComb3 = combinablePromotions("P3", promotions)
    println("Promotion Combinations for promotionCode = P3 ")
    println(promotionComb3)
  }
}