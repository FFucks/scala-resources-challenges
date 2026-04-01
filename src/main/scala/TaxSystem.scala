object TaxSystem extends App {

    sealed trait ProductType
    case object Electronics extends ProductType
    case object Food extends ProductType
    case object Clothing extends ProductType

    case class Product(name: String, price: BigDecimal, productType: ProductType)

    case class State(code: String)

    type Year = Int

    case class TaxRule(
                          state: State,
                          year: Year,
                          productType: ProductType,
                          rate: BigDecimal
                      )

    object TaxCalculator {

        def calculateTax(
                            product: Product,
                            state: State,
                            year: Year,
                            rules: List[TaxRule]
                        ): BigDecimal =
            rules
                .find(r =>
                    r.state == state &&
                        r.year == year &&
                        r.productType == product.productType
                )
                .map(rule => product.price * rule.rate)
                .getOrElse(BigDecimal(0))

        def finalPrice(
                          product: Product,
                          state: State,
                          year: Year,
                          rules: List[TaxRule]
                      ): BigDecimal =
            product.price + calculateTax(product, state, year, rules)
    }


    val sp = State("SP")
    val rj = State("RJ")

    val rules = List(
        TaxRule(sp, 2024, Electronics, BigDecimal(0.18)),
        TaxRule(sp, 2024, Food, BigDecimal(0.05)),
        TaxRule(rj, 2024, Electronics, BigDecimal(0.20)),
        TaxRule(rj, 2025, Electronics, BigDecimal(0.22))
    )

    val notebook = Product("Notebook", BigDecimal(5000), Electronics)
    val apple = Product("Apple", BigDecimal(10), Food)

    val taxNotebook = TaxCalculator.calculateTax(notebook, sp, 2024, rules)
    val totalNotebook = TaxCalculator.finalPrice(notebook, sp, 2024, rules)

    val taxApple = TaxCalculator.calculateTax(apple, sp, 2024, rules)
    val totalApple = TaxCalculator.finalPrice(apple, sp, 2024, rules)

    println(s"Tax Notebook SP 2024: $taxNotebook")
    println(s"Total Notebook: $totalNotebook")

    println(s"Tax Apple SP 2024: $taxApple")
    println(s"Total Apple: $totalApple")
}
