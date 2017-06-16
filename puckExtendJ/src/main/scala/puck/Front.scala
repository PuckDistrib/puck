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

//package puck
//
//import jastadd._
//
//object Front extends PuckApplication(ExtendJGraphUtils, JavaIcons)


package puck

import java.awt.Dimension
import java.io.File
import javax.swing.UIManager

import org.extendj.ExtendJGraphUtils
import puck.config.Config
import puck.view.PuckMainPanel

import scala.swing.{MainFrame, SwingApplication}

object Front extends SwingApplication {

  val workspace = "."

  val pmp = new PuckMainPanel(ExtendJGraphUtils, JavaIcons)

  def startup(args: Array[String]) : Unit = {

    val f0 =
      if(args.isEmpty || ! new File(args(0)).exists()) new File(workspace)
      else new File(args(0))

    val f = f0.getAbsoluteFile

    val sConf =
      if (f.isDirectory && Config.defaultConfFile(f).exists())
        Some(Config.defaultConfFile(f))
      else if (f.isFile) Some(f)
      else None

    sConf foreach pmp.control.loadConf


    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
    if (top.size == new Dimension(0,0)) top.pack()
    top.visible = true

  }

  val top = new MainFrame {
    title = "Puck"
    contents  = pmp
  }
}

/*
 from http://stackoverflow.com/questions/2315912/scala-best-way-to-parse-command-line-parameters-cli
 val usage = """
   Usage: mmlaln [--min-size num] [--max-size num] filename
             """
 def main(args: Array[String]) {
   if (args.length == 0) println(usage)
   val arglist = args.toList
   type OptionMap = Map[Symbol, Any]

   def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
     def isSwitch(s : String) = (s(0) == '-')
     list match {
       case Nil => map
       case "--max-size" :: value :: tail =>
         nextOption(map ++ Map('maxsize -> value.toInt), tail)
       case "--min-size" :: value :: tail =>
         nextOption(map ++ Map('minsize -> value.toInt), tail)
       case string :: opt2 :: tail if isSwitch(opt2) =>
         nextOption(map ++ Map('infile -> string), list.tail)
       case string :: Nil =>  nextOption(map ++ Map('infile -> string), list.tail)
       case option :: tail => println("Unknown option "+option)
         exit(1)
     }
   }
   val options = nextOption(Map(),arglist)
   println(options)
 }
 */
