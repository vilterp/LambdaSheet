package lambdasheet

abstract class LambdaSheetVal
  class IntVal(int:Int) extends LambdaSheetVal
  class StringVal()
  class Application(name:String, args:List[LambdaSheetVal]) extends LambdaSheetVal