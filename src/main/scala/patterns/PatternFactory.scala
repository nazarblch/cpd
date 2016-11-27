package patterns

/**
  * Created by buzun on 27/11/16.
  */
trait PatternFactory {

  def apply(windowSize: Int): CurvePattern

}
