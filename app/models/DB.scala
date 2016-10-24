package models

import play.api.libs.json.Json

case class Apartment(owner:String, name: String, description: String)
case class User(username: String, email: String, password: String)

object User {
  implicit val formatter = Json.format[User]
}

object Apartment {
  implicit val formatter = Json.format[Apartment]

}
