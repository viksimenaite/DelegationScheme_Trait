package Delegation

import java.time.LocalDate

object Main {
  def main(args: Array[String]): Unit = {

    val client = Client(Vegetarian(), toleratesLactose = false)

    Console.println("Would you like:")
    Console.println("1 - a cake\r\n2 - a chocolate\r\n3 - a cupcake")
    Console.println("Enter your choice:")
    val choice = scala.io.StdIn.readInt()
    var productIsSuitableForLactoseIntolerant = false
    var productIsSuitableForNonMeatEaters = false

    choice match{
      case 1 =>
        val product = FoodProduct(Vegetarian(), "2020-12-01", 17.5, containsLactose = false)

        val cakeFactory = new CakeFactory()

        if (client.eatingHabits == Vegan() || client.eatingHabits == Vegetarian()){
          productIsSuitableForNonMeatEaters = cakeFactory.isSuitableForNonMeatEaters(product)
        }
        if (!client.toleratesLactose){
          productIsSuitableForLactoseIntolerant = cakeFactory.isSuitableForLactoseIntolerantPeople(product)
        }

        if (productIsSuitableForNonMeatEaters && productIsSuitableForLactoseIntolerant){
          Console.println("Cake price: " + cakeFactory.countPrice(product)+ " eur")
        }else{
          Console.println("This product is not suitable for you.")
        }

      case 2 =>
        val product = FoodProduct(Vegan(), "2021-06-01", 10.5, containsLactose = false)

        val chocolateFactory = new ChocolateFactory()
        Console.println("Chocolate price: " + chocolateFactory.countPricePer200g(product) * (1-chocolateFactory.countExpirationDiscount(product)) + " eur")

      case 3 =>
        val product = FoodProduct(Vegetarian(), "2020-12-01", 22.5, containsLactose = false)

        val cupcakeFactory = new CupcakeFactory()
        var toppingsPrice = 0.0
        Console.println("The price of one cupcake:" + cupcakeFactory.countPricePerOne(product)+ " eur")
        if (client.eatingHabits == Vegan() || client.eatingHabits == Vegetarian()){
          productIsSuitableForNonMeatEaters = cupcakeFactory.isSuitableForNonMeatEaters(product)
        }
        if (!client.toleratesLactose){
          productIsSuitableForLactoseIntolerant = cupcakeFactory.isSuitableForLactoseIntolerantPeople(product)
        }

        if (productIsSuitableForNonMeatEaters && productIsSuitableForLactoseIntolerant){
          Console.println("Would you like to add toppings?")
          Console.println("1 - yes\r\n2 - no")
          Console.println("Enter your choice:")
          val answer = scala.io.StdIn.readInt()
          answer match{
            case 1 => toppingsPrice = cupcakeFactory.addMarshmallows(product) + cupcakeFactory.addSprinkles(product)
            case _ => toppingsPrice = 0.0
          }

          Console.println("How many cupcakes would you like?")
          Console.println("Enter your choice:")
          val quantity = scala.io.StdIn.readInt()
          Console.println("Total price:" + (cupcakeFactory.sellRequiredAmount(product, quantity) + toppingsPrice * quantity) + " eur")

        }else{
          Console.println("This product is not suitable for you.")
        }

      case _ => Console.println("Please choose a product from the list.")
  }
  }
}

// "enum"

sealed trait EatingHabits
case class Vegan() extends EatingHabits
case class Vegetarian() extends EatingHabits
case class EatsEverything() extends EatingHabits

//entities

case class FoodProduct(intendedFor: EatingHabits, expiryDate:String, baseKgPrice:Double, containsLactose:Boolean)
case class Client(eatingHabits: EatingHabits, toleratesLactose:Boolean)

class CakeFactory extends SuitabilityForDifferentEatingHabits{

  def countPrice(product:FoodProduct): Double = {
    if (product.intendedFor == Vegan()){
      product.baseKgPrice + 8
    }else if(product.intendedFor == Vegetarian()){
      product.baseKgPrice + 6
    }else if(!product.containsLactose){
      product.baseKgPrice + 4
    }else{
      product.baseKgPrice + 2
    }

  }

}

class ChocolateFactory extends HalfPriceDiscount{
  def countPricePer200g(product:FoodProduct): Double = product.baseKgPrice * 0.2
}

class CupcakeFactory extends  SuitabilityForDifferentEatingHabits with DecoratingWithToppings {
  val averageWeightInGrams = 100
  def countPricePerOne(product:FoodProduct): Double =  if(product.containsLactose){
    product.baseKgPrice * averageWeightInGrams/1000 + 1
  } else{
    product.baseKgPrice * averageWeightInGrams/1000 + 2
  }

  def sellRequiredAmount(product:FoodProduct, quantity:Int): Double ={
    if (quantity>10){
      countPricePerOne(product) * quantity * 0.9
    }else {
      countPricePerOne(product) * quantity
    }
  }
}

//additional functionalities

trait Decorating{
  def addSprinkles(product:FoodProduct):Double
  def addMarshmallows(product:FoodProduct):Double
}

trait Discounts{
  def countExpirationDiscount(product:FoodProduct):Double
}

trait Suitability{
  def isSuitableForLactoseIntolerantPeople(product:FoodProduct): Boolean
  def isSuitableForNonMeatEaters(product:FoodProduct): Boolean
}

//additional functionalities' implementations

trait DecoratingWithToppings extends Decorating{
  def addSprinkles(product:FoodProduct):Double = 0.5 //add to price
  def addMarshmallows(product:FoodProduct):Double = if(product.intendedFor == Vegan()){
    0
  }else{
    1.2
  }
}

trait HalfPriceDiscount extends Discounts{
  def countExpirationDiscount(product:FoodProduct):Double = {
    val dtf = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val expiryDate = java.time.LocalDate.parse(product.expiryDate, dtf)

    if(LocalDate.now.isEqual(expiryDate) || LocalDate.now.isAfter(expiryDate)){
      0.5
    }else{
      0
    }
  }
}

trait SuitabilityForDifferentEatingHabits extends Suitability{
  def isSuitableForLactoseIntolerantPeople(product:FoodProduct): Boolean = if(!product.containsLactose){
    true
  }else{
    false
  }
  def isSuitableForNonMeatEaters(product:FoodProduct): Boolean = if(product.intendedFor == Vegan() || product.intendedFor == Vegetarian()){
    true
  }else{
    false
  }
}




