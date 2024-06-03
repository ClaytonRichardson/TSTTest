import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import PromotionProcessor._


class PromotionComboTest extends AnyFunSuite with Matchers {

  test("allCombinablePromotions - basic scenario") {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val expectedCombos = Seq(
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P3", "P4", "P5")),
      PromotionCombo(Seq("P1", "P4", "P5"))
    )

    val result = allCombinablePromotions(promotions)
    result should contain theSameElementsAs expectedCombos
  }

  test("combinablePromotions - basic scenario 1") {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val expectedCombos = Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5"))
    )

    val result = combinablePromotions("P1", promotions)
    result should contain theSameElementsAs expectedCombos
  }

  test("combinablePromotions - basic scenario 2") {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val expectedCombos = Seq(
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P1", "P2"))
    )

    val result = combinablePromotions("P2", promotions)
    result should contain theSameElementsAs expectedCombos
  }

  test("combinablePromotions - basic scenario 3") {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val expectedCombos = Seq(
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P3", "P4", "P5")),
    )

    val result = combinablePromotions("P3", promotions)
    result should contain theSameElementsAs expectedCombos
  }

  test("combinablePromotions - basic scenario 4") {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val expectedCombos = Seq(
      PromotionCombo(Seq("P3", "P4", "P5")),
      PromotionCombo(Seq("P1", "P4", "P5"))
    )

    val result = combinablePromotions("P4", promotions)
    result should contain theSameElementsAs expectedCombos
  }

  test("combinablePromotions - basic scenario 5") {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val expectedCombos = Seq(
      PromotionCombo(Seq("P3", "P4", "P5")),
      PromotionCombo(Seq("P1", "P4", "P5"))
    )

    val result = combinablePromotions("P5", promotions)
    result should contain theSameElementsAs expectedCombos
  }

  test("getBestGroupPrices - basic scenario") {
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

    val expectedBestGroupPrices = Seq(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CA", "S1", 225.00, "Senior"),
      BestGroupPrice("CB", "M1", 230.00, "Military"),
      BestGroupPrice("CB", "S1", 245.00, "Senior"))

    val actualBestGroupPrices = BestGroupPriceCalculator.getBestGroupPrices(rateTest, pricesTest)

    assert(actualBestGroupPrices == expectedBestGroupPrices)
  }
}

