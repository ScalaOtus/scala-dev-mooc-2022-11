package module3

import zio.Has
import zio.Task
import userService.{User, UserID}
import zio.{ZLayer, ULayer}
import zio.macros.accessible

package object userDAO {

    /**
     * Реализовать сервис с двумя методами
     *  1. list - список всех пользоватиелей
     *  2. findBy - поиск по User ID
     */

     type UserDAO = Has[UserDAO.Service]

     object UserDAO{
       trait Service{
         def list(): Task[List[User]]
         def findBy(id: UserID): Task[Option[User]]
       }

       val live: ULayer[UserDAO] = ???
     }

}
