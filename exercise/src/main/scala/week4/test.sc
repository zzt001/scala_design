object test {
  class Rep(cmd: => Unit) {
    def UNTIL(cond: => Boolean): Unit = {
      cmd
      if (cond) ()
      else UNTIL(cond)
    }
  }
  def REPEAT(cmd: => Unit): Rep = {
    new Rep(cmd)
  }

  var x = 5
  REPEAT {
    print(x)
    x = x - 1
  } UNTIL(x == 0)

}