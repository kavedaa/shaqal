package org.shaqal

object Debug {

  private var q = 0
  private var r = 0
  
  def clear() = {
    q = 0
    r = 0
  }

  private[shaqal] def incQueries() = { q = q +1 }
  private[shaqal] def incRows() = { r = r +1 }
  
  def numQueries = q
  def numRows = r
}