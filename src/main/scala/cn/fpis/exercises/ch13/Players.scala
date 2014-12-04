package cn.fpis.exercises.ch13

/**
 * Created by guillermo on 4/12/14.
 */

class Player(val name: String, val score: Int)

object Player{
  def winnerMsg(p: Player) = p.name + " is the winner!"
}


