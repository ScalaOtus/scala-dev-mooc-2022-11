package module3.emailService

case class Email(to: EmailAddress, body: Html)

case class EmailAddress(address: String) extends AnyVal

case class Html(raw: String) extends AnyVal