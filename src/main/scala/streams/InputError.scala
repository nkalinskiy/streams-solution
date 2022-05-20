package streams

sealed abstract class InputError {
  def message: String
}

case class EmptyInput(
  paramName: String
) extends InputError {
  override def message: String = "Empty input is invalid for " + paramName
}

case class FileDoesNotExist(
  filename: String
) extends InputError {
  override def message: String = "File does not exist: " + filename
}

case class InvalidExtension(
  extension: String
) extends InputError {
  override def message: String = "InvalidExtension: " + extension
}

case class InvalidHashConst(
  passedValue: String
) extends InputError {
  override def message: String = "Expected a positive int, got: " + passedValue
}
