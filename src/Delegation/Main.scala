package Delegation

object Main {
  def main(args: Array[String]): Unit = {

  }

}

//entities

case class Shoe(isLeather:Boolean, isLimitedEdition:Boolean, basePrice:Double, isLuxury:Boolean)

class ShoeFactory{ //1st entity
  def makeShoes(isFullWorkDay:Boolean):Int = if(isFullWorkDay){
    2500
  }else{
    1000
  }

  def sellAtBasePrice(shoe:Shoe): Double = shoe.basePrice

}

class ShoeWarehouse{ //2nd entity
  val maxNoOfShoesPairs = 500000
  var currentNoOfShoesPairs = 0
  val storageFeePerPair = 0.05
  def storeShoes(noOfPairs:Int):Boolean={
    if((currentNoOfShoesPairs + noOfPairs) <= maxNoOfShoesPairs){
      currentNoOfShoesPairs = currentNoOfShoesPairs + noOfPairs
      true //successfully stored
    }else{
      false
    }
  }

  def requestShoes(noOfPairs:Int): Double = noOfPairs * storageFeePerPair;
}

class ShoeShop{ //3rd entity
  var currentNoOfShoesPairs = 0

  def sellShoes(isLuxury:Boolean):Double= if(isLuxury){
    190.00
  }else{
    50.90
  }

  def reserveShoes(isPopular:Boolean):Double = if(isPopular){
    7.00 //reservation fee
  }else{
    3.5 //reservation fee
  }

  def receiveShoes(noOfPairs:Int):Unit = {
    currentNoOfShoesPairs = currentNoOfShoesPairs + noOfPairs
  }
}

//additional functionalities
trait shoeDecorating{

}

