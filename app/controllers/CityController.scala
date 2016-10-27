package controllers

import javax.inject._

import models._
import play.api.Logger
import play.api.libs.json.{JsObject, _}
import play.api.mvc._
import play.modules.reactivemongo._
import reactivemongo.api.ReadPreference
import reactivemongo.play.json._
import reactivemongo.play.json.collection._
import utils.Errors

import scala.concurrent.{ExecutionContext, Future}
import play.api.data._
import play.api.data.Forms._
import reactivemongo.bson.{BSONDocument, BSONDocumentWriter, BSONObjectID, BSONValue}
import java.io.File


/**
  *
db.getCollection('persons').find({"apartments":{$elemMatch:{"$exists":true}}})
db.getCollection('persons').find({"apartments":{$elemMatch:{"$exists":true}}}).map(function(elem) { return elem.apartments;})[0]
Ok.sendFile(new java.io.File("/tmp/fileToServe.pdf"))

*/
@Singleton
class CityController @Inject()(val reactiveMongoApi: ReactiveMongoApi)(implicit exec: ExecutionContext) extends Controller with MongoController with ReactiveMongoComponents {

  val userRegForm = Form(
    mapping(
      "username" -> nonEmptyText,
      "email" -> email,
      "password" -> text
    )(User.apply)(User.unapply)
  )

  def personsFuture: Future[JSONCollection] = database.map(_.collection[JSONCollection]("persons"))

  def regNewUser = Action {
    Ok(views.html.reg())
  }

  def createAp = Action {
    Ok(views.html.add_apartment())
  }

  def getImage(path:String) = Action.async{
    Future(Ok.sendFile(new java.io.File(s"/home/incode51/IdeaProjects/MongoTest/app/images/$path")).withHeaders(CONTENT_DISPOSITION -> "inline"))
  }

  def createApartment = Action.async {
    implicit request =>
        val uid:String = request.session.get("user_id").get
        val answ = personsFuture.flatMap(_.find(Json.obj("_id" -> BSONObjectID(uid))).one[User])
        answ.map {
          case Some(user) => {
            request.body.asMultipartFormData match {
              case Some(apartmentForm) => {
                val apartmentData = apartmentForm.dataParts

                if (apartmentData("name").length == 0 || apartmentData("description").length == 0 ) {
                  println(apartmentData("name") + " " + apartmentData("description"))
                  BadRequest("incorrect form")
                }
                else {
                  val listOfImages: Seq[String] = apartmentForm.files.map(indx => {
                      val filenameRandom: String = scala.util.Random.alphanumeric.take(10).mkString("") + indx.filename
                      val fullPath = s"/home/incode51/IdeaProjects/MongoTest/app/images/$filenameRandom"
                      indx.ref.moveTo(new File(fullPath))
                      fullPath
                    }
                  )

                  val newApartment = Json.obj("_id" -> BSONFormats.toJSON(BSONObjectID.generate),
                    "name" -> apartmentData("name").mkString(""),
                    "description" -> apartmentData("description").mkString(""),
                    "images" -> Json.toJson(listOfImages))

                  val json = Json.obj("apartments" -> newApartment)
                  val modifier = BSONDocument("$push" -> json)
                  val selector = BSONDocument("_id" -> BSONObjectID(uid))
                  val futureUpdate = personsFuture.map(coll => coll.update(selector, modifier))
                  Ok("OK")
                }
              }
              case None => BadRequest("incorrect form2")
            }
          }
          case None => BadRequest("incorrect form1")
        }
  }

  def loggedIn = Action.async {
    implicit request =>
      request.session.get("logged_in").map(elem => elem match {
        case "true" => {
          val uid:String = request.session.get("user_id").get
          println(uid)
          val answ = personsFuture.flatMap(_.find(Json.obj("_id" -> BSONObjectID(uid))).one[User])
          answ.map {
            case Some(user) => Ok(views.html.logged_in(user.username, user.email))
            case _ => Ok(views.html.logged_in("test", "test"))
          }
        }
        case "false" =>  println("FALSE");Future(Redirect("/reg"))
      }
    ).getOrElse(Future(Redirect("/reg")))
  }

  def getAllApartments = Action.async {
    implicit request =>
      val answ = personsFuture.flatMap(_.find(Json.obj("apartments" -> Json.obj("$exists" -> "true"))).cursor[JsObject]().collect[List](0))
      answ.map(elem => { println(elem.map( one => (one \ "apartments").get)); Ok(views.html.logged_in( elem.map( one => (one \ "apartments").as[JsArray]).reduceLeft((a1:JsArray, a2:JsArray) => a1 ++ a2).toString, ""))})
  }

  def getUserInfo(id: String) = Action.async {
    implicit request =>
      val answ = personsFuture.flatMap(_.find(Json.obj("_id" -> BSONObjectID(id))).one[User])
      answ.map {
        case Some(user) => Ok(views.html.logged_in(user.username, user.email))
        case _ => Ok(views.html.logged_in("Unknown", "Unknown"))
      }
  }

  implicit val userWrites = new Writes[User] {
    def writes(user: User):JsObject = Json.obj(
      "_id" -> BSONFormats.toJSON(BSONObjectID.generate),
      "username" -> user.username,
      "email" -> user.email,
      "password" -> user.password,
      "apartments" -> JsArray()
    )
  }

  def createNewUser = Action.async {
    implicit request =>

      userRegForm.bindFromRequest.fold(
      formWithErrors => {
        Future(BadRequest("incorrect"))
      },
      userData => {

        val json:JsObject = Json.toJson(userData).as[JsObject]

        val answ = personsFuture.flatMap(elem => {
          elem.insert(json)
        })

        answ.map(elem => {
          elem.errmsg.getOrElse("without errors") match {
            case "without errors" => Ok("without errors").withSession("logged_in" -> "true", "user_id" -> (json \ "_id" \ "$oid").asOpt[String].get);
            case _ => BadRequest("Errors while adding")
          }
        })
      }
    )
  }




//  def create(name: String, population: Int) = Action.async {
//    for {
//      cities <- citiesFuture
//      lastError <- cities.insert(City(name, population))
//    } yield
//      Ok("Mongo LastError: %s".format(lastError))
//  }
//
//  def createFromJson = Action.async(parse.json) { request =>
//    Json.fromJson[City](request.body) match {
//      case JsSuccess(city, _) =>
//        for {
//          cities <- citiesFuture
//          lastError <- cities.insert(city)
//        } yield {
//          Logger.debug(s"Successfully inserted with LastError: $lastError")
//          Created("Created 1 city")
//        }
//      case JsError(errors) =>
//        Future.successful(BadRequest("Could not build a city from the json provided. " + Errors.show(errors)))
//    }
//  }
//
//  def createBulkFromJson = Action.async(parse.json) { request =>
//    Json.fromJson[Seq[City]](request.body) match {
//      case JsSuccess(newCities, _) =>
//        citiesFuture.flatMap { cities =>
//          val documents = newCities.map(implicitly[cities.ImplicitlyDocumentProducer](_))
//
//          cities.bulkInsert(ordered = true)(documents: _*).map { multiResult =>
//            Logger.debug(s"Successfully inserted with multiResult: $multiResult")
//            Created(s"Created ${multiResult.n} cities")
//          }
//        }
//      case JsError(errors) =>
//        Future.successful(BadRequest("Could not build a city from the json provided. " + Errors.show(errors)))
//    }
//  }
//
//  def findByName(name: String) = Action.async {
//    // let's do our query
//    val futureCitiesList: Future[List[City]] = citiesFuture.flatMap {
//      // find all cities with name `name`
//      _.find(Json.obj("name" -> name)).
//      // perform the query and get a cursor of JsObject
//      cursor[City](ReadPreference.primary).
//      // Collect the results as a list
//      collect[List]()
//    }
//
//    // everything's ok! Let's reply with a JsValue
//    futureCitiesList.map { cities =>
//      Ok(Json.toJson(cities))
//    }
//  }
}


