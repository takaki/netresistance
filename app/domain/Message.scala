package domain

trait LogQueue {
  def add(logMessage: LogMessage): LogQueue

  def queue: Seq[LogMessage]
}

trait LogMessage {
  def message: String
}

trait ErrorLog

trait InfoLog {
  def message = toString
}




