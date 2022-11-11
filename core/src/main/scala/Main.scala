import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax._

//shapeless: KeyTag, type
//circe: DerivationMacros, KeyTag
//https://stackoverflow.com/questions/74384745/encoding-decoding-a-field-with-any-datatype-assigned-with-value-none-in-scala
object Main extends App {
  case class Demo(
                   field1: Any
                 )

  val myDemo=Demo(field1=None)
  print(myDemo.asJson+"\n")

  implicit val valueEncoderValue: Encoder[Any] = Encoder.encodeString.contramap[Any](x=>{
    x.toString})

  implicit val valueDecoderValue: Decoder[Any] = Decoder.decodeString.map[Any](x => {
    if (x == "Any")
      x.asInstanceOf[Any]
    else
      x.toString
  })

  //java.lang.NullPointerException
  implicit lazy val DemoCodec: Codec[Demo] =
    deriveCodec[Demo]

}