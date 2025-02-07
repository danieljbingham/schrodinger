package schrodinger

enum State {
  case Closed(failures: Int)
  case Open(startTime: Long)
  case HalfOpen
}
