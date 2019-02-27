object SwitchY {

  def run(e: => Unit)(implicit level: SwitchLevel.Value): Unit = {
    if (level.compare(SwitchLevel.On) == 0) {
      e
    }
  }
}
