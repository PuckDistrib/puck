/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.search

import puck.graph.{LoggedTry, error}
import puck.util.Logged

import scalaz.{-\/, \/-}

class SearchState[T]
( val id : Int,
  val prevState : Option[SearchState[T]],
  val loggedResult : LoggedTry[T]
){

  import scalaz.syntax.writer._

  def isSuccess = loggedResult.value.isRight
  def success : Logged[T] = loggedResult.value match {
      case \/-(res) => res set loggedResult.log
      case -\/(err) => error("state contains a failed result : " + err)
  }

  def fail : Logged[String] = loggedResult.value match {
    case -\/(err) => err set loggedResult.log
    case _ => error("state contains a success")
  }


  private def uuid0 : Seq[Int] = {
    prevState match{
      case None => Seq(id)
      case Some(parent) => id +: parent.uuid0
    }
  }

  def uuid : Seq[Int] = {
    this.uuid0.reverse
  }

  def depth : Int = uuid0.size

  def isMarkPointState = true

  def uuid(sep : String = "_",
           considerMarkPoint : Boolean = false,
           markPointSep : String = "/") : String = {

    if(!considerMarkPoint) uuid.mkString(sep)
    else ??? //todo decide if markpointSep is after or before the mark point state !
    /*
    {
      val ancestorsSeq = ancestors(includeSelf = true)
       //here the markPointSep is before the mark point state !
      val tailStrList =  ancestorsSeq.tail map {s =>
          (if(s.isMarkPointState) markPointSep
          else sep) + s.id.toString
        }
      (ancestorsSeq.head.id.toString +: tailStrList).mkString
    }*/


  }

  def markedPointDepth : Int = {
    def aux(sstate : Option[SearchState[T]], acc : Int) : Int = sstate match {
      case None => acc
      case Some(s) => aux(s.prevState,
        if(s.isMarkPointState) acc + 1
        else acc)
    }
    aux(prevState, 0)
  }

  def ancestors(includeSelf : Boolean) :Seq[SearchState[T]] = {
    def aux(sState : Option[SearchState[T]],
            acc : Seq[SearchState[T]]) : Seq[SearchState[T]] =
      sState match {
        case None => acc
        case Some(state) => aux(state.prevState, state +: acc)
      }

    aux(if(includeSelf) Some(this) else this.prevState, Seq())

  }


  override def toString = uuid()

  var cid = -1

  def nextChildId() : Int = {
    cid += 1
    cid
  }


}
