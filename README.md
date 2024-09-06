# all-of-type

 EG:

 ```scala
 @AllOfType
 object Foo {
   val x: String = "ASDAS"
   val y = "ASDAS"
   val all: Seq[String] = AllOfType.derive
 }
 ```

 Will result in the `all` value being implemented as `Seq(x, y)`.
