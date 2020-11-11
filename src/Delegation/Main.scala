package Delegation

import java.time.LocalDate

object Main {
  def main(args: Array[String]): Unit = {

  }

}

sealed trait Accessories
case class Sprinkles() extends Accessories
case class Berries() extends Accessories
case class ChocolateChips() extends Accessories

sealed trait EatingHabits
case class Vegan() extends EatingHabits
case class Vegetarian() extends EatingHabits
case class EatsEverything() extends EatingHabits

//entities

case class FoodProduct(intendedFor: EatingHabits, expiryDate:String, baseKgPrice:Double, containsLactose:Boolean)

class CakeFactory{

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

class ChocolateFactory{
  def countPricePer200g(product:FoodProduct): Double = product.baseKgPrice * 0.2
}

class CupcakeFactory{
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
  def isSuitableForEverybody(product:FoodProduct): Boolean
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

trait HalfPriceDiscount{
  def countExpirationDiscount(product:FoodProduct):Double = {
    val dtf = java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val expiryDate = java.time.LocalDate.parse(product.expiryDate, dtf)

    if(LocalDate.now.isEqual(expiryDate) || LocalDate.now.isAfter(expiryDate)){
      0.5
    }else{
      0
    }
  }

  trait SuitabilityForDifferentEatingHabits{
    def isSuitableForEverybody(product:FoodProduct): Boolean = if(product.intendedFor == Vegan() && !product.containsLactose){
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
}



